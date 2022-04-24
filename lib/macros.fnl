



;; DEPS

(fn partition-2 [seq]
  ;; Partition `seq` by 2.
  ;; If `seq` has odd amount of elements, the last one is dropped.
  ;;
  ;; Input: [1 2 3 4 5]
  ;; Output: [[1 2] [3 4]]
  (let [firsts []
        seconds []
        res []]
    (for [i 1 (length seq) 2]
      (let [first (. seq i)
            second (. seq (+ i 1))]
        (table.insert firsts (if (not= nil first) first `nil))
        (table.insert seconds (if (not= nil second) second `nil))))
    (each [i v1 (ipairs firsts)]
      (let [v2 (. seconds i)]
        (if (not= nil v2)
            (table.insert res [v1 v2]))))
    res))


;; MATCH

(fn match-values [vals pattern unifications match-pattern]
  (let [condition `(and)
        bindings []]
    (each [i pat (ipairs pattern)]
      (let [(subcondition subbindings) (match-pattern [(. vals i)] pat
                                                      unifications)]
        (table.insert condition subcondition)
        (each [_ b (ipairs subbindings)]
          (table.insert bindings b))))
    (values condition bindings)))

(fn match-table [val pattern unifications match-pattern]
  (let [condition `(and (= (_G.type ,val) :table))
        bindings []]
    (each [k pat (pairs pattern)]
      (if (= pat `&)
          (let [rest-pat (. pattern (+ k 1))
                rest-val `(select ,k ((or table.unpack _G.unpack) ,val))
                subcondition (match-table `(pick-values 1 ,rest-val)
                                          rest-pat unifications match-pattern)]
            (if (not (sym? rest-pat))
                (table.insert condition subcondition))
            (assert (= nil (. pattern (+ k 2)))
                    "expected & rest argument before last parameter")
            (table.insert bindings rest-pat)
            (table.insert bindings [rest-val]))
          (= k `&as)
          (do
            (table.insert bindings pat)
            (table.insert bindings val))
          (and (= :number (type k)) (= `&as pat))
          (do
            (assert (= nil (. pattern (+ k 2)))
                    "expected &as argument before last parameter")
            (table.insert bindings (. pattern (+ k 1)))
            (table.insert bindings val))
          ;; don't process the pattern right after &/&as; already got it
          (or (not= :number (type k)) (and (not= `&as (. pattern (- k 1)))
                                           (not= `& (. pattern (- k 1)))))
          (let [subval `(. ,val ,k)
                (subcondition subbindings) (match-pattern [subval] pat
                                                          unifications)]
            (table.insert condition subcondition)
            (each [_ b (ipairs subbindings)]
              (table.insert bindings b)))))
    (values condition bindings)))

(fn match-pattern [vals pattern unifications]
  "Takes the AST of values and a single pattern and returns a condition
to determine if it matches as well as a list of bindings to
introduce for the duration of the body if it does match."
  ;; we have to assume we're matching against multiple values here until we
  ;; know we're either in a multi-valued clause (in which case we know the #
  ;; of vals) or we're not, in which case we only care about the first one.
  (let [[val] vals]
    (if (or (and (sym? pattern) ; unification with outer locals (or nil)
                 (not= "_" (tostring pattern)) ; never unify _
                 (or (in-scope? pattern) (= :nil (tostring pattern))))
            (and (multi-sym? pattern) (in-scope? (. (multi-sym? pattern) 1))))
        (values `(= ,val ,pattern) [])
        ;; unify a local we've seen already
        (and (sym? pattern) (. unifications (tostring pattern)))
        (values `(= ,(. unifications (tostring pattern)) ,val) [])
        ;; bind a fresh local
        (sym? pattern)
        (let [wildcard? (: (tostring pattern) :find "^_")]
          (if (not wildcard?) (tset unifications (tostring pattern) val))
          (values (if (or wildcard? (string.find (tostring pattern) "^?")) true
                      `(not= ,(sym :nil) ,val)) [pattern val]))
        ;; guard clause
        (and (list? pattern) (= (. pattern 2) `?))
        (let [(pcondition bindings) (match-pattern vals (. pattern 1)
                                                   unifications)
              condition `(and ,(unpack pattern 3))]
          (values `(and ,pcondition
                        (let ,bindings
                          ,condition)) bindings))
        ;; multi-valued patterns (represented as lists)
        (list? pattern)
        (match-values vals pattern unifications match-pattern)
        ;; table patterns
        (= (type pattern) :table)
        (match-table val pattern unifications match-pattern)
        ;; literal value
        (values `(= ,val ,pattern) []))))

(fn match-condition [vals clauses]
  "Construct the actual `if` AST for the given match values and clauses."
  (if (not= 0 (% (length clauses) 2)) ; treat odd final clause as default
      (table.insert clauses (length clauses) (sym "_")))
  (let [out `(if)]
    (for [i 1 (length clauses) 2]
      (let [pattern (. clauses i)
            body (. clauses (+ i 1))
            (condition bindings) (match-pattern vals pattern {})]
        (table.insert out condition)
        (table.insert out `(let ,bindings
                             ,body))))
    out))

(fn match-val-syms [clauses]
  "How many multi-valued clauses are there? return a list of that many gensyms."
  (let [syms (list (gensym))]
    (for [i 1 (length clauses) 2]
      (let [clause (if (and (list? (. clauses i)) (= `? (. clauses i 2)))
                       (. clauses i 1)
                       (. clauses i))]
        (if (list? clause)
            (each [valnum (ipairs clause)]
              (if (not (. syms valnum))
                  (tset syms valnum (gensym)))))))
    syms))

(fn match* [val ...]
  ;; Old implementation of match macro, which doesn't directly support
  ;; `where' and `or'. New syntax is implemented in `match-where',
  ;; which simply generates old syntax and feeds it to `match*'.
  (let [clauses [...]
        vals (match-val-syms clauses)]
    (assert (= 0 (math.fmod (length clauses) 2))
            "expected even number of pattern/body pairs")
    ;; protect against multiple evaluation of the value, bind against as
    ;; many values as we ever match against in the clauses.
    (list `let [vals val] (match-condition vals clauses))))


;; MATCH WRAPPER

(fn match-where [val ...]
  "Perform pattern matching on val. See reference for details.

Syntax:

(match data-expression
  pattern body
  (where pattern guard guards*) body
  (where (or pattern patterns*) guard guards*) body)"
  (let [conds-bodies (partition-2 [...])
        else-branch (if (not= 0 (% (select "#" ...) 2))
                        (select (select "#" ...) ...))
        match-body []]
    (each [_ [cond body] (ipairs conds-bodies)]
      (each [_ cond (ipairs (transform-cond cond))]
        (table.insert match-body cond)
        (table.insert match-body body)))
    (if else-branch
        (table.insert match-body else-branch))
    (match* val (unpack match-body))))




{
 :match-val-syms match-val-syms
 ;; :when2 when2
 }
