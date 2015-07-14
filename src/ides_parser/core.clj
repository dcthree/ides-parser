(ns ides-parser.core
  (:gen-class)
  (:require [clojure.string :as st]))

(defn substring-before
  [string1 string2]
  (.substring string1 0 (if (.contains string1 string2) (.indexOf string1 string2) 0)))

(defn substring-after
  "Returns the part of string1 that comes after the first occurrence of string2, or
  nil if string1 does not contain string2."
  [string1 string2]
  (when (.contains string1 string2) (.substring string1 (+ (.indexOf string1 string2) (.length string2)))))

(defn vec-remove
  "Given a vector and a position, returns a vector with the element at that position removed"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn re-match?
  "Returns true if the regex re matches the string s"
  [re s]
  (let [result (re-find re s)]
    (or 
     (seq result)
     (not (nil? result)))))

(defn index-of
  ([lst re index]
   (when (> (count lst) index)
     (loop [i index]
       (if (re-match? re (nth lst i))
         i
         (if (= (inc i) (count lst))
           nil
           (recur (inc i)))))))
  ([lst re]
   (index-of lst re 0)))

(defn indexes-of
  [lst re]
  (loop [i 0 result []]
    (if-let [in (index-of lst re i)]
      (recur (inc in) (conj result in))
      result)))

(defn last-index-of
  [s re]
  (loop [i 0 rs (reverse s)]
    (if (re-match? re (nth rs i))
      (- (count s) i 1)
      (if (= (inc i) (count s))
        nil
        (recur (inc i) rs)))))

(def separator #"(?:, ?| |:|(?<=(?:\]|\d))\.(?! |$)|(?<= )\([^)]+\))")
;; non-capturing: , optional space|space|:; period preceded by ] or a digit and not followed by a space
;; #"(?:,? |: ?|(?<!(p|n))\.(?=(\d|pp?\.|n\.)))"

(defn split-cite
  [cite]
 (let [body (st/split cite separator)
       ws (vec (re-seq separator cite))
       parens (indexes-of ws #"\(")] 
   (loop [p parens newbody body newws ws] 
     (if (seq p) 
      (let [nws (vec-remove newws (first p))] 
        (recur (indexes-of nws #"\(") 
               (let [sb (split-at (first p) newbody)] 
                 (prn (first sb))
                 (prn (second sb))
                 (concat (conj (vec (first sb)) (nth newws (first p))) (vec (nthrest (second sb) 2))))
               nws))
      {:body (vec newbody), :ws newws}))))

(def vol-patterns-claros
  {"ABV" #"\d+(-\d+)?\.?"
   "AD N.S." #"\d+( A| B)?, \d{4}"
   "AJA" #"(N.S. )?\d+, \d{4}"
   "SEG" #"^\d{1,2}$"
   "BE" #"^\(?(1[89]|20)\d\d\)?(-\d+)?$"
   "BLund" #"^\d{4}-\d+"
   "ICr" #"^\d$"})

(def vol-patterns-claros-title
  {})

(def vol-patterns
  (ref
    {"AAA" #"^\d+ \(\d{4}\)$"
     "AD" #"^\d+ ([A-Z]\d?)? ?\(\d{4}(/\d?\d)?\)$"
     "BE" #"^\(?(1[89]|20)\d\d\)?(-\d+)?$"
     "FD III" #"^\d$"
     "IC" #"^[IV]+( [ivx]+)?$"
     "SEG" #"^\d{1,2}$"
}))

(defn test-vol-pattern
  "Returns the matched value if vol has a known pattern and it matches value, false if not,
  and true if vol doesn't have a known pattern."
  [vol value]
  (if-let [re (@vol-patterns vol)]
    (or (re-match? re value) false)
    true))

(defn locations
  [s ch]
  (loop [i 0 r []]
    (let [loc (.indexOf s ch i)]
      (if (> loc 0)
        (recur (inc loc) (conj r loc))
        r))))

(defn balanced?
  ""
  [s ch1 ch2]
  (let [a (locations s ch1)
        b (locations s ch2)]
    (and
      (= (count a) (count b))
      (every? true? (map < a b)))))

(defn test-brackets-balance
  [cite]
  (if (every? true?
        (map #(vector
          (balanced? % (int \() (int \)))
          (balanced? % (int \[) (int \]))) cite))
    cite
    (throw (Exception. "Unbalanced brackets."))))

(defn parse-validate
  [cite]
  ;; (prn (str "Validating: " cite))
  (if (test-vol-pattern (first cite) (second cite))
    cite
    (throw (Exception. "Volume in wrong format."))))

(defn parse-cleanup
  [title vol item lst sp]
  ;; (prn "parse-cleanup")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn (and (re-match? #"^\(?(1[89]|20)\d\d\)?$" vol) (not= title "BE")))
  ;; (prn "---------")
  (if (and (empty? lst) (test-vol-pattern title vol))
    [title vol item]
     ;; if the vol doesn't look like a vol or a year, it's part of the title
    (let [result
          (if
            (not (re-match? #"^(\(?(1[89]|20)\d\d(/\d\d?)?\)?|[IVXLC])[-0-9IVXLC²³.(), \[\]]*[a-z]?(Suppl\.)?(\(?[^)]+\))?$" vol))
            (if (empty? lst)
              (list (str title (last sp) vol) "" item)
              (list (str title (st/join (interleave sp lst)) (last sp) vol) "" item))
            (if (empty? lst)
              (list title vol item)
              (if (re-match? #"^(\d{1,2}|[IVXLC])[0-9IVXLC²³.(), \[\]]*[a-z]?(Suppl\.)?(\(?[^)]+\))?$" (last lst))
                (parse-cleanup title (str (last lst) (last sp) vol) item (drop-last lst) (drop-last sp))
                (list (str title (st/join (interleave sp lst))) vol item))))]
    (parse-validate result))))

(declare parse-vol-set)

(defn parse-vol-incomplete
  [title vol item lst sp]
  ;; (prn "parse-vol-incomplete")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn "---------")
  ;; if vol doesn't look like a volume #, but the preceding list element does,
  ;; then decide whether the current vol belongs in the item or not
  (if
    (and
      (not (empty? lst))
      (or
        (not (re-match? #"^(\d{1,2}|[IVXLC²³.()]+)$" vol))
        (re-match? #"^([0-9()]+|[IVXLC²³()]+)$" (last lst))))
    (if
      (or
       (and (re-match? #"\d" vol) (re-match? #"\p{L}" vol))  ;; a mixture of numeric and non-numeric chars is probably an item
       (not (test-vol-pattern title vol))
       (string? (test-vol-pattern title (last lst))))
      (parse-vol-set title nil (str vol (last sp) item) lst (drop-last sp))
      (parse-cleanup title (str (last lst) (last sp) vol) item (drop-last lst) (drop-last sp)))
    (parse-cleanup title vol item lst sp)))

(defn interleave-unequal
  "Interleaves two lists, the first of which may have more members than the second"
  [list1 list2]
  (loop [l1 list1 l2 list2 result nil]
    (if (seq l1)
      (recur (rest l1) (rest l2) (str result (first l1) (first l2)))
      result)))

(defn parse-vol-set
  [title vol item lst sp]
  ;; (prn "parse-vol-set")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn "---------")
  (if-not (empty? lst)
    ;; if true, we haven't got the full title yet
    (if (contains? @vol-patterns (str title (first sp) (first lst)))
      (parse-vol-set (str title (first sp) (first lst)) vol item (rest lst) (rest sp))
      ;; if true, we have a matching pattern, and so can stop
      (if (and (contains? @vol-patterns title) (re-match? (get @vol-patterns title) (st/trim (st/join (interleave-unequal lst (rest sp))))))
        (parse-validate (list title (st/trim (st/join (interleave-unequal lst (rest sp)))) item))
        (if-let [i (index-of lst #"^(\d+[\[\]()0-9]*|[IVXLC][0-9IVXLC²³.,()\[\]]*)([a-z](?![ ,]))?$")]          
          (let [v (str (st/join (interleave (take-last (- (count lst) i) lst) (take-last (- (count (drop-last (rest sp))) i) (drop-last (rest sp))))) (last lst))]
            (prn v)
            ;; if the vol looks good, then skip to title
            (if-let [vv (re-find #"^\d?[0-9IVXLC²³., ()\[\]]+[a-z]?(Suppl\.)?(\(?[^)]+\))?" v)]
              (if (re-match? #"^\d?[0-9IVXLC²³., ()\[\]]+[a-z]?(Suppl\.)?(\(?[^)]+\))?$" v)
                (parse-cleanup title v item (drop-last (- (count lst) i) lst) (drop-last (- (count lst) i) sp))
                (parse-cleanup title (st/replace (first vv) "[, ]+$" "") (str (st/trim (substring-after v (first vv))) (last sp) item) (drop-last (- (count lst) i) lst) (drop-last (- (count lst) i) sp)))
              (parse-vol-incomplete title (last lst) item (drop-last lst) (drop-last sp))))
          (parse-vol-incomplete title (last lst) item (drop-last lst) sp))))
    (parse-validate (list title (or vol "") item))))

(defn parse-year
  "Years go only in volume info, so we can settle on what's in the item if we find a year."
  [title vol item lst sp]
  ;; (prn "parse-year")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn "---------")
  (if (empty? lst)
    (parse-validate (list title (or vol "") item))
    (if-let [i (last-index-of lst #"\(?(1[89]|20)\d\d(/\d\d?)?\)?")]
      (parse-vol-set title vol
        (str (st/join (interleave (take-last (dec (- (count lst) i)) lst) (take-last (dec (- (count lst) i)) sp))) item)
        (drop-last (dec (- (count lst) i)) lst) (drop-last (dec (- (count lst) i)) sp))
      (parse-vol-set title vol item lst sp))))

;;(defn parse-item-+
;;  [title vol item lst sp]
;;  (if-let [plus (index-of "\+" lst)]
;;    ))

(defn parse-item-sub-item
  "If the text in item is prefixed with something like 'col.' then we haven't got the whole item yet"
  [title vol item lst sp]
  ;; (prn "parse-item-sub-item")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn "---------")
  (if (re-match? #"^(col\.|fig\.)" item)
    (let [i (inc (- (count lst) (last-index-of lst #"\d")))]
      (parse-year title vol (str (st/join (interleave (take-last (inc i) lst) (take-last i sp))) (last lst)) (drop-last (inc i) lst) (drop-last i sp)))
    (parse-year title vol item lst sp)))

(defn parse-item-number
  [title vol item lst sp]
   ;; (prn "parse-item-number")
   ;; (prn title)
   ;; (prn vol)
   ;; (prn item)
   ;; (prn lst)
   ;; (prn sp)
   ;; (prn "---------")
  ;; end of list doesn't look like a fascicle # (and we're not dealing with SEG) or item is non-numeric, but end of list is
  (if (empty? lst)
    (parse-validate (list title (or vol "") item))
    (if
      (or
        (and (re-match? #"^([2-9]\d+|\d{3})$" (last lst)) (re-match? #"^not(?:e|a)" item))
        (and (not (re-match? #"^\d" item)) (re-match? #"\d" (last lst))))
      (parse-item-sub-item title vol (str (last lst) (last sp) item) (drop-last lst) (drop-last sp))
      (parse-item-sub-item title vol item lst sp))))

(defn parse-item-labeled
  "Some items/item parts are labeled, e.g p. 13, so spot them"
  [title vol item lst sp]
  ;; (prn "parse-item-labeled")
  ;; (prn title)
  ;; (prn vol)
  ;; (prn item)
  ;; (prn lst)
  ;; (prn sp)
  ;; (prn "---------")
  (if (empty? lst)
    (parse-validate (list title (or vol "") item))
    (if-let [i (index-of lst #"^[0-9,]*(p(p|l)?\.|no?(t(e|a))?\.?)$")]
      (parse-item-number
       title vol
       (str
        (st/join
         (interleave
          (take-last
           (- (count lst) i) lst)
          (take-last
           (- (count lst) i) sp))) item)
       (drop-last (- (count lst) i) lst)
       (drop-last (- (count lst) i) sp))
      (parse-item-number title vol item lst sp))))

(defn parse-citation
  [cite]
  (let [spcite (split-cite cite)]
    (when (> (count (:body spcite)) 1)
      (parse-item-labeled 
       (first (:body spcite)) 
       nil 
       (last (:body spcite)) 
       (drop-last (rest (:body spcite))) 
       (:ws spcite)))))

(defmacro with-vol-pattern
  "Use a different set of volume patterns than the default."
  [pattern & body]
  `(let [vp# @vol-patterns]
     (dosync (ref-set vol-patterns ~pattern))
     (let [result# (do ~@body)]
       (dosync (ref-set vol-patterns vp#))
       result#)))

(defn parse-citation-volume
  "Start parsing with the assumption we don't have an item."
  [cite]
  (let [spcite (split-cite cite)]
    (parse-vol-set (first (:body spcite)) nil "" (rest (:body spcite)) (:ws spcite))))

(defn parse-citation-with-title
  "The citation comes with a title, so we don't need to figure out what the title part is"
  [cite title]
  (if (.startsWith cite title)
    (let [citepart (st/replace (substring-after cite title) #"^( |\.)" "")
          vol (with-vol-pattern {} (parse-citation-volume title))
          spcite (split-cite citepart)]
      (with-vol-pattern vol-patterns-claros
        (if-not (st/blank? (second vol))
          (parse-validate (list (first vol) (second vol) citepart))
          (parse-item-labeled (first vol) (second vol) (last (:body spcite)) (drop-last (:body spcite)) (:ws spcite)))))
    (parse-citation cite)))
