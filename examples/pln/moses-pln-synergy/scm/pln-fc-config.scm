;;
;; URE Configuration file for PLN
;;
;; Before running any PLN inference you must load that file in the
;; AtomSpace
;;
;; In order to add new rules you need to hack this file in 2 places
;;
;; 1. In the Load rules section, to add the file name where the rule is
;; defined (see define rule-files).
;;
;; 2. In the Associate rules to PLN section, to add the name of the
;; rule and its weight (see define rules).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load required modules and utils ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (opencog))
(use-modules (opencog rule-engine))

;XXX This is bad and broken and wrong; one should not try to bypass the
; scheme module system like this, its just asking for carpet burns.
(load-from-path "utilities.scm")
(load-from-path "av-tv.scm")
(load-from-path "opencog/rule-engine/rule-engine-utils.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define PLN rule-based system ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pln-rbs (ConceptNode "PLN"))
(InheritanceLink
   pln-rbs
   (ConceptNode "URE")
)

;; Define pln-fc and pln-bc for convenience 
(define (pln-fc source) (cog-fc pln-rbs source (List) (Set)))
(define (pln-bc target) (cog-bc pln-rbs target (List) (Set)))

;;;;;;;;;;;;;;;;
;; Load rules ;;
;;;;;;;;;;;;;;;;

;; Load the rules. Either w.r.t this file path
(add-to-load-path "../../../../opencog/pln/rules/")
;; Or the corresponding unit test
(add-to-load-path "../../../opencog/pln/rules/")

(define rule-filenames
  (list "wip/implication-instantiation-rule.scm"
        "wip/implication-scope-to-implication-rule.scm"
        "wip/and-lambda-distribution-rule.scm"
        "wip/closed-lambda-evaluation-rule.scm"
        "wip/implication-introduction-rule.scm"
        "wip/implication-implicant-distribution-rule.scm"
        "wip/implication-and-lambda-factorization-rule.scm"
        "term-logic/deduction-rule.scm"
        "wip/equivalence-to-implication-rule.scm"
        "wip/implication-implicant-disjunction-rule.scm"
        )
  )
(for-each load-from-path rule-filenames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associate rules to PLN ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; List the rules and their weights.
(define rules
  (list (list implication-partial-instantiation-rule-name 1)
        (list implication-scope-to-implication-rule-name 1)
        (list and-lambda-distribution-rule-name 1)
        (list closed-lambda-evaluation-rule-name 1)
        (list implication-introduction-rule-name 1)
        (list implication-implicant-distribution-rule-name 1)
        (list implication-and-lambda-factorization-rule-name 1)
        (list deduction-implication-rule-name 1)
        (list implication-full-instantiation-rule-name 1)
        (list equivalence-to-implication-rule-name 1)
        (list implication-implicant-disjunction-rule-name 1)
        )
  )

;; Associate rules to PLN
(ure-add-rules pln-rbs rules)

;;;;;;;;;;;;;;;;;;;;;;
;; Other parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Termination criteria parameters
(ure-set-num-parameter pln-rbs "URE:maximum-iterations" 1000000)

;; Attention allocation (0 to disable it, 1 to enable it)
(ure-set-fuzzy-bool-parameter pln-rbs "URE:attention-allocation" 0)
