
; disjunct-stats.scm
;
; Assorted ad-hoc collection of tools for understanding the
; word-simillarity obtained via pseudo-disjunct overlap.
;
; These were used to create the section "Connetor Sets 7 May 2017"
; in the diary. These can ONLY be used by hand, by cutting and pasting
; the interesting bits from this file, into a guile prompt. This is
; more of a kind-of work-log of what was needed to generate those
; pictures, than anything else. Of course, this can be recycled for
; other datasets, too.
;
; TODO: move this file to the lang-learn-diary directory. It does
; not belong here, in the main repo
;
; Copyright (c) 2017 Linas Vepstas
;

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------
; Load the dataset that is analyzed throughout.
(use-modules (opencog) (opencog persist) (opencog persist-sql))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog analysis))
(sql-open "postgres:///en_pairs_sim?user=linas")

(fetch-all-words)         ; 581 seconds from cold spinning media.
                          ; 22 seconds from hot buffer cache
(length (get-all-words))  ; 396262 words as always.

(define pca (make-pseudo-cset-api))
(pca 'fetch-pairs)        ; 456 secs after above

(define ac (get-all-cset-words))
(length ac)               ; 37413 s always

(define ad (get-all-disjuncts))
(length ad)               ; 291637 as always

(fetch-all-sims)          ; 17 seconds


; ---------------------------------------------------------------------
; Ranking and printing utilities
;
; Assign each word a score, using SCORE-FN, and then rank them by
; score: i.e. sort them, with highest score first.
(define (score-and-rank SCORE-FN WORD-LIST)
	(sort
		(map (lambda (wrd) (cons (SCORE-FN wrd) wrd)) WORD-LIST)
		(lambda (a b) (> (car a) (car b)))))

; Print to port a tab-separated table of rankings
(define (print-ts-rank-fn scrs port fn-str)
	(define cnt 0)
	(for-each
		(lambda (pr)
			(set! cnt (+ cnt 1))
			(format port "~A	~A	\"~A\"\n" cnt (car pr) (fn-str (cdr pr))))
		scrs))

(define (print-ts-rank scrs port)
	(print-ts-rank-fn scrs port cog-name))

(define (print-ts-rank-cset scrs port)
   (print-ts-rank-fn scrs port
		(lambda (x) (cog-name (gar x)))))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; A list of all words that have csets. (Not all of the words
; in the database got tagged with a cset)
(define all-cset-words (get-all-cset-words))

; A list of all disjucnts (appearing in all csets)
(define all-disjuncts (get-all-disjuncts))

(define all-csets (get-all-csets))

; words with 100 or more observations.
(define top-cset-words
	(filter (lambda (wrd) (< 100 (cset-vec-word-observations wrd)))
		all-cset-words))

; ---------------------------------------------------------------------
; A sorted list of score-word pairs, where the score is the count
; of the cset observations. Note that this score is *identical* to the
; number of times that the word was observed during MST parsing. That is
; because exactly one disjunct is extracted per word, per MST parse.
(define sorted-word-obs
	(score-and-rank cset-vec-word-observations all-cset-words)

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-word-obs.dat" "w")))
	(print-ts-rank sorted-word-obs outport)
	(close outport))

; A sorted list of score-disjunct pairs, where the score is the count
; of the cset observations.
(define sorted-dj-obs
	(score-and-rank cset-vec-dj-observations all-disjuncts))

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-dj-obs.dat" "w")))
	(print-ts-rank-fn sorted-dj-obs outport get-disjunct-string)
	(close outport))

; A sorted list of score-cset pairs, where the score is the count
; of the cset observations.
(define sorted-cset-obs (score-and-rank get-count all-csets))

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-cset-obs.dat" "w")))
	(print-ts-rank-fn sorted-cset-obs outport get-cset-string)
	(close outport))

; ---------------------------------------------------------------------
; Compute the average number of observations per disjunct.
(define (avg-obs WORD)
	(/ (cset-vec-word-observations WORD) (cset-vec-word-support WORD)))

; Compute the average number of observations per disjunct.
; Discard anything with less than 100 observations.
(define top-sorted-avg
	(score-and-rank avg-obs top-cset-words))

(let ((outport (open-file "/tmp/ranked-avg.dat" "w")))
	(print-ts-rank top-sorted-avg outport)
	(close outport))

; --------------------------
; As above, but this time, instead of ranking, we bin-count.
;
(define sorted-avg
	(score-and-rank avg-obs all-cset-words))

(define binned-avg (bin-count-simple sorted-avg 100 1.0 6.0))

(let ((outport (open-file "/tmp/binned-avg.dat" "w")))
	(print-bincounts-tsv binned-avg outport)
	(close outport))

; ---------------------------------------------------------------------
; A sorted list of the support of a word.
; The support is simply how many basis elements of a vector are
; non-zero.  Equivalently, it is the size of the set of unique
; disjuncts associated with a word (counted without multiplicity).

(define sorted-support (score-and-rank cset-vec-word-support all-cset-words))

(let ((outport (open-file "/tmp/ranked-support.dat" "w")))
	(print-ts-rank sorted-support outport)
	(close outport))

; ---------------------------------------------------------------------
; A sorted list of score-word pairs, where the score is the cset length

(define sorted-lengths (score-and-rank cset-vec-word-len all-cset-words)

(let ((outport (open-file "/tmp/ranked-lengths.dat" "w")))
	(print-ts-rank sorted-lengths outport)
	(close outport))

; ---------------------------------------------------------------------
; Consider, for example, the length-squared, divided by the number
; of observations.
(define (lensq-vs-obs wrd)
	(define len (cset-vec-word-len wrd))
	(/ (* len len) (cset-vec-word-observations wrd)))

; The length vs observation ranking; but discard everything
; with a small number of observations.
(define sorted-lensq-norm-top
	(score-and-rank lensq-vs-obs top-cset-words))

(let ((outport (open-file "/tmp/ranked-sqlen-norm.dat" "w")))
	(print-ts-rank sorted-lensq-norm-top outport)
	(close outport))

(define sorted-lensq-norm
	(score-and-rank lensq-vs-obs all-cset-words))

(define binned-sqlen-norm (bin-count-simple sorted-lensq-norm 100 0.0 10.0))

(let ((outport (open-file "/tmp/binned-sqlen-norm.dat" "w")))
	(print-bincounts-tsv binned-sqlen-norm outport)
	(close outport))

; ---------------------------------------------------------------------
; RMS deviation from mean.
;  sum_i (x-a)^2 = sum_i x^2 - 2a sum_i x + a^2 sum_i
; but now divide by N = sum_i, again, to get
;  sum_i (x-a)^2 = <x^2> - a^2
;
(define (avg-obs WORD)
	(/ (cset-vec-word-observations WORD) (cset-vec-word-support WORD)))

(define (meansq-obs WORD)
	(define len (cset-vec-word-len WORD))
	(/ (* len len) (cset-vec-word-support WORD)))

(define (rms-deviation WORD)
	(define avg (avg-obs WORD))
	(sqrt (- (meansq-obs WORD) (* avg avg))))

; Compute the RMS deviation from the average number of observations
; per disjunct.
; Discard anything with less than 100 observations.
(define sorted-rms
	(score-and-rank rms-deviation top-cset-words))

(let ((outport (open-file "/tmp/ranked-rms.dat" "w")))
	(print-ts-rank sorted-rms outport)
	(close outport))

;
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Vertex degrees and hubiness.
; The vertex degree is the same thing as the number of connectors
; on a disjunct.
; Hubiness is the second moment of the vertex degree.

(define (avg-con-count wrd)
	(/ (cset-vec-connectors wrd) (cset-vec-word-observations wrd)))

; Rank by average disjunct size; but discard everything
; with a small number of observations.
(define sorted-avg-connectors
	(score-and-rank avg-con-count top-cset-words))

(define sorted-avg-connectors
	(score-and-rank avg-con-count all-cset-words))

(let ((outport (open-file "/tmp/ranked-avg-connectors.dat" "w")))
	(print-ts-rank sorted-avg-connectors outport)
	(close outport))

; ----
; Second moment is sum (c^2/n) - (sum c/n)^2
; RMS is sqrt (sum (c^2/n) - (sum c/n)^2)
(define (moment-con-count wrd)
	(define meansq
		(/ (cset-vec-lp-connectors wrd 2) (cset-vec-word-observations wrd)))
	(define avg (avg-con-count wrd))
	(sqrt (- meansq (* avg avg))))

(define sorted-hub-connectors
	(score-and-rank moment-con-count top-cset-words))

(let ((outport (open-file "/tmp/ranked-hub-connectors.dat" "w")))
	(print-ts-rank sorted-hub-connectors outport)
	(close outport))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Distributions for two particular words.

(define dj-united (score-and-rank get-count
		(get-cset-vec (Word "United"))))

(define dj-it (score-and-rank get-count
		(get-cset-vec (Word "It"))))

; Print to port a tab-separated table of rankings
(define (print-ts-dj-rank cset-a cset-b port)
	(define idx 0)
	(for-each
		(lambda (pra prb)
			(set! idx (+ idx 1))
			(format port "~A	~A	~A\n" idx (car pra) (car prb)))
		cset-a cset-b))

(let ((outport (open-file "/tmp/ranked-dj-united.dat" "w")))
	(print-ts-dj-rank dj-united dj-it outport)
	(close outport))

; ---------------------------------------------------------------------
; Sum over distributions. Basically, above gave two ranking
; distributions, one for each word.  They can be averaged together
; to get a smoother graph.  Here, we create a ranking for each
; word, and then average them all together. This is kind of hokey,
; in the end, but whatever.

; Create and zero out array.
(define dj-sum (make-array 0 312500))

; Create a ranked list of disjuncts for WORD
(define (make-ranked-dj-list WORD)
	(score-and-rank get-count (get-cset-vec WORD)))

; Accumulate the disjunct counts for WORD, into dj-sum
(define (accum-dj-counts WORD)
	(define idx 0)
	(for-each
		(lambda (pr)
			(array-set! dj-sum (+ (car pr) (array-ref dj-sum idx)) idx)
			(set! idx (+ idx 1)))
		(make-ranked-dj-list WORD)))

; Accumulate the disjunct counts for all the words in the word-list
(define (accum-dj-all WORD-LIST)
	(for-each
		(lambda (word) (accum-dj-counts word))
		WORD-LIST))

; Accumulate all, but only for those with 100 or more observations.
(accum-dj-all top-csets-words)

(define (print-dj-acc port)
	(define idx 0)
	(for-each
		(lambda (cnt)
			(set! idx (+ idx 1))
			(if (< 0 cnt)
				(format port "~A	~A\n" idx cnt)))
		(array->list dj-sum)))

(let ((outport (open-file "/tmp/ranked-dj-counts.dat" "w")))
	(print-dj-acc outport)
	(close outport))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Entropies.

; Several ways of counting the same thing. These should all
; give the same result.

(define total-cset-count (get-total-cset-count))

; Get the "fractional entropy".
(define (fractional-entropy FUNC LIST)
	(fold
		(lambda (item sum)
			(define pitem (/ (FUNC item) total-cset-count))
			(- sum (* pitem (log pitem))))
		0
		LIST))

; Get the "word entropy".
(define word-entropy
	(fractional-entropy cset-vec-word-observations all-cset-words))
(define word-entropy-bits (/ word-entropy (log 2.0)))

(define disjunct-entropy
	(fractional-entropy cset-vec-dj-observations all-disjuncts))
(define disjunct-entropy-bits (/ disjunct-entropy (log 2.0)))

(define cset-entropy-bits
	(fold
		(lambda (word sum) (+ sum  (cset-vec-word-entropy word)))
		0
		all-cset-words))

; A sloppy, slow, wasteful algorithm
(define (get-total-mi-slow)
	(define prog 0)
	(define prog-len (length all-csets))
	(fold
		(lambda (cset sum)
			(define word (cset-get-word cset))
			(define dj (cset-get-disjunct cset))
			(define p-word (cset-vec-frequency word))
			(define p-dj (cset-vec-frequency dj))
			(define p-cset (get-cset-frequency cset))

			(set! prog (+ prog 1))
			(if (eqv? 0 (modulo prog 100))
				(format #t "doing ~A of ~A\n" prog prog-len))
			(+ sum (* p-cset (log (/ p-cset (* p-word p-dj)))))
		)
		0
		all-csets))

(define slow-total-mi (get-total-mi-slow))

; A much faster, optimized version. Still takes tens of minutes
; to run.
(define (get-total-mi-bits)
	(define prog 0)
	(define prog-len (length all-cset-words))
	(define sum 0)
	(for-each
		(lambda (word)
			(set! prog (+ prog 1))
			(if (eqv? 0 (modulo prog 100))
				(format #t "doing ~A of ~A\n" prog prog-len))
			(set! sum (+ sum (cset-vec-word-mi word))))
		all-cset-words)
	sum
)

(define total-mi-bits (get-total-mi-bits))

; The in-a-jiffy version which needs no new computations:
(define total-mi-bits
	(- (word-entropy-bits disjunct-entropy-bits) cset-entropy-bits)

; ---------------------------------------------------------------------
; Rank words according to thier fractonal entropy
(define pca (make-pseudo-cset-api))
(define pcw (add-pair-wildcards pca))
(define pmi (add-pair-mi-compute pca))
(define (cset-vec-word-ent WORD)
		(pmi 'compute-right-fentropy WORD))

; rank only the top-100
(define sorted-word-ent (score-and-rank cset-vec-word-ent top-cset-words))

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-word-ent.dat" "w")))
	(print-ts-rank sorted-word-ent outport)
	(close outport))

(define sorted-word-ent (score-and-rank cset-vec-word-ent all-cset-words))
(define binned-word-ent (bin-count-simple sorted-word-ent 100))

(let ((outport (open-file "/tmp/binned-word-ent.dat" "w")))
	(print-bincounts-tsv binned-word-ent outport)
	(close outport))

;--------------
; Create a binned graph of the number of words observed with a given
; log-liklihood.  Although this is in the binning tradition of the
; other binned graphs, its really the same thing as the log-log
; graph of the word-counts, but renormalized.
(define (cset-vec-word-logli WORD)
	(pmi 'right-wild-logli WORD))

(define sorted-word-logli (score-and-rank cset-vec-word-logli all-cset-words))
(define binned-word-logli (bin-count-simple sorted-word-logli 200 10.0 20.0))

(let ((outport (open-file "/tmp/binned-word-logli.dat" "w")))
	(print-bincounts-tsv binned-word-logli outport)
	(close outport))

; -------
; A simple graph of how many words were observed once, twice, etc.
; So: first column: how many times a word was observed.
; Second column: the number of words that were observed tat many times.
;
(define sorted-word-counts
	(score-and-rank cset-vec-word-observations all-cset-words))
(define binned-word-counts
	(bin-count-simple sorted-word-counts 200 0.5 200.5))

(let ((outport (open-file "/tmp/binned-word-counts.dat" "w")))
	(print-bincounts-tsv binned-word-counts outport)
	(close outport))

;--------------
; cheat method, assumes we are not making errors,
; it would be better to double-check this by running the
; sums the other way.
(define (cset-vec-word-crazy WORD)
	(- (pmi 'compute-right-mi WORD)
		(+ (pmi 'right-wild-logli WORD)
			(pmi 'compute-right-fentropy WORD))))

(define sorted-word-crazy (score-and-rank cset-vec-word-crazy all-cset-words))
(define binned-word-crazy (bin-count-simple sorted-word-crazy 100))

(let ((outport (open-file "/tmp/binned-word-crazy.dat" "w")))
	(print-bincounts-tsv binned-word-crazy outport)
	(close outport))

(define (cset-vec-word-crazy2 WORD)
	(- (pmi 'compute-right-mi WORD)
		(pmi 'compute-right-fentropy WORD)))

(define sorted-word-crazy2 (score-and-rank cset-vec-word-crazy2 all-cset-words))
(define binned-word-crazy2 (bin-count-simple sorted-word-crazy2 100))

(let ((outport (open-file "/tmp/binned-word-crazy2.dat" "w")))
	(print-bincounts-tsv binned-word-crazy2 outport)
	(close outport))

; ---------------------------------------------------------------------
; Rank word-disjunct pairs according to thier fractonal MI.

(define pfrq (add-pair-freq-api  pca))
(define (cset-mi CSET) (pfrq 'pair-fmi CSET))

(define sorted-cset-mi (score-and-rank cset-mi all-csets))

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-cset-mi.dat" "w")))
	(print-ts-rank-cset sorted-cset-mi outport)
	(close outport))

(define binned-cset-mi (bin-count-simple sorted-cset-mi 100))
(define binned-cset-mi (bin-count-simple sorted-cset-mi 200))

(let ((outport (open-file "/tmp/binned-cset-mi.dat" "w")))
	(print-bincounts-tsv binned-cset-mi outport)
	(close outport))

(define (cset-freq CSET) (pfrq 'pair-freq CSET))

(define weighted-cset-mi
	(bin-count-weighted sorted-cset-mi 200
		(lambda (scored-item) (cset-freq (cdr scored-item)))))

(let ((outport (open-file "/tmp/weighted-cset-mi.dat" "w")))
	(print-bincounts-tsv weighted-cset-mi outport)
	(close outport))

(define (cset-logli CSET) (pfrq 'pair-logli CSET))

(define wlogli-cset-mi
	(bin-count-weighted sorted-cset-mi 200
		(lambda (scored-item) (cset-logli (cdr scored-item)))))

(let ((outport (open-file "/tmp/wlogli-cset-mi.dat" "w")))
	(print-bincounts-tsv wlogli-cset-mi outport)
	(close outport))

; ---------------------------------------------------------------------
; Rank words according to the MI between them and thier disjuncts
(define sorted-word-mi (score-and-rank cset-vec-word-mi all-cset-words))

; Like above, but high-ferequency only
(define sorted-word-mi-hi-p
	(score-and-rank cset-vec-word-mi top-cset-words))

; Print above to a file, so that it can be graphed.
(let ((outport (open-file "/tmp/ranked-word-mi.dat" "w")))
	(print-ts-rank sorted-word-mi outport)
	(close outport))

(let ((outport (open-file "/tmp/ranked-word-mi-hi-p.dat" "w")))
	(print-ts-rank sorted-word-mi-hi-p outport)
	(close outport))

; ---------------------------------------------------------------------
; Create the binned distribution graphs for word-MI

(define binned-word-mi (bin-count-simple sorted-word-mi 100))

(define (new-cset-vec-word-mi WORD)
	(define pca (make-pseudo-cset-api))
	(define pmi (add-pair-mi-compute pca))
	(pmi 'compute-right-fmi WORD)
)

(define osorted-word-mi (score-and-rank new-cset-vec-word-mi all-cset-words))

(let ((outport (open-file "/tmp/binned-word-mi.dat" "w")))
	(print-bincounts-tsv binned-word-mi outport)
	(close outport))

; ---------------------------------------------------------------------
; Create the binned distribution graphs for dj-MI

(define (cset-vec-disjunct-mi DJ)
	(pseudo-cset-mi-api 'compute-left-fmi DJ))

(define sorted-dj-mi (score-and-rank cset-vec-disjunct-mi all-disjuncts))

(define binned-dj-mi (bin-count-simple sorted-dj-mi 100))

(let ((outport (open-file "/tmp/binned-dj-mi.dat" "w")))
	(print-bincounts-tsv binned-dj-mi outport)
	(close outport))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Similarity.  Cosine distance.
; Assumes that cosine similarities have been batch-computed, already,
; using the code in gram-sim.scm

(define cos-key (PredicateNode "*-Cosine Distance Key-*"))

(define all-sims '())
(cog-map-type
	(lambda (sim)  (set! all-sims (cons sim all-sims)) #f)
	'SimilarityLink)

(length all-sims)    ; 23993

(define good-sims
	(filter
		(lambda (sim) (and
				(< 0.5 (sim-cosine sim))
				(< 8 (cset-vec-word-len (gar sim)))
				(< 8 (cset-vec-word-len (gdr sim)))))
		all-sims))

(length good-sims)   ;  15808

(define ranked-sims
	(sort good-sims
		(lambda (a b) (> (sim-cosine a) (sim-cosine b)))))

(define (prt-sim sim port)
	(format port "~A	'~A .. ~A'\n" (sim-cosine sim)
		(cog-name (gar sim)) (cog-name (gdr sim))))

(let ((outport (open-file "/tmp/ranked-sims.dat" "w")))
	(define cnt 0)
	(for-each (lambda (sim)
			(set! cnt (+ cnt 1))
			(format outport "~A	" cnt)
			(prt-sim sim outport))
		ranked-sims)
	(close outport))

; ------ again, but binned.

(define sorted-sims (score-and-rank sim-cosine all-sims))

(define binned-sims (bin-count-simple sorted-sims 100))

(let ((outport (open-file "/tmp/binned-sims.dat" "w")))
	(print-bincounts-tsv binned-sims outport)
	(close outport))

; ---------------------------------------------------------------------
