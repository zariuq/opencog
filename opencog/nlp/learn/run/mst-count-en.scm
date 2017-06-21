;
; mst-count-en.scm
;
; Run the cogserver, needed for the language-learning disjunct
; counting pipeline. Starts the cogserver, opens the database,
; loads the database (whcih can take an hour or more!)
;
(use-modules (opencog) (opencog cogserver))
(use-modules (opencog persist) (opencog persist-sql))
(use-modules (opencog nlp) (opencog nlp learn))

; Start the cogserver.
; Edit the below, setting it to the desired langauge.
; This has almost no effect, other than to set the cogserver
; port-number and the prompt-style.
(start-cogserver "opencog-mst-en.conf")

; Open the database.
; Edit the below, setting the database name, user and password.
(sql-open "postgres:///en_pairs_ttwo_mst?user=ubuntu&password=asdf")

; Load up the words
(display "Fetch all words from database. This may take several minutes.\n")
(fetch-all-words)

; Load up the word-pairs -- this can take over half an hour!
(display "Fetch all word-pairs. This may over half-an-hour!\n")

; The object which will be providing pair-counts for us.
; We can also do MST parsing with other kinds of pair-count objects,
; for example, the clique-pairs, or the distance-pairs.
(define pair-obj (make-any-link-api))
(pair-obj 'fetch-pairs)

; Print the sql stats
(sql-stats)

; Clear the sql cache and the stats counters
(sql-clear-cache)
(sql-clear-stats)
