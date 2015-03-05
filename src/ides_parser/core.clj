(ns ides-parser.core
  (:gen-class)
  (:require [clojure.string :as s]))

(defn substring-before
  [string1 string2]
  (.substring string1 0 (if (.contains string1 string2) (.indexOf string1 string2) 0)))

(defn substring-after
  "Returns the part of string1 that comes after the first occurrence of string2, or 
  nil if string1 does not contain string2."
  [string1 string2]
  (when (.contains string1 string2) (.substring string1 (+ (.indexOf string1 string2) (.length string2)))))

(defn index-of
  [s re]
  (loop [i 0]
    (if (re-find re (nth s i))
      i
      (if (= (inc i) (count s))
        nil
        (recur (inc i))))))

(defn last-index-of
  [s re]
  (loop [i 0 rs (reverse s)]
    (if (re-find re (nth rs i))
      (- (count s) i 1)
      (if (= (inc i) (count s))
        nil
        (recur (inc i) rs)))))

(def vol-patterns-claros
  {"SEG" #"^\d{1,2}$"
   "BE" #"^\(?(1[89]|20)\d\d\)?(-\d+)?$"
   "BLund" #"^\d{4}-\d+"
   "ICr" #"^\d$"})

(def vol-patterns-claros-title
  {})

(def vol-patterns 
  (ref 
    {"AAA" #"^\d+ \(\d{4}\)$"
     "AD" #"^\d+ ([A-Z]\d)? ?\(\d{4}(/\d?\d)?\)$"
     "BE" #"^\(?(1[89]|20)\d\d\)?(-\d+)?$"
     "FD III" #"^\d$"
     "IC" #"^[IV]+$"
     "SEG" #"^\d{1,2}$"
}))

(defn match-vol-pattern
  "Returns the matched value if vol has a known pattern and it matches value, false if not, 
  and true if vol doesn't have a known pattern."
  [vol value]
  (if-let [re (@vol-patterns vol)]
    (or (re-find re value) false)
    true))

(defn parse-validate
  [cite]
  ;; (prn (str "Validating: " cite))
  (if (match-vol-pattern (first cite) (second cite))
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
 ;; (prn (and (re-find #"^\(?(1[89]|20)\d\d\)?$" vol) (not= title "BE")))
 ;; (prn "---------")
  ;; if the vol doesn't look like a vol, or looks like a year, it's part of the title
  (let [result 
          (if 
            (not (re-find #"^(\(?\d{1,4}\)?|[IVXLC])[-0-9IVXLC²³.(), \[\]]*[a-z]?(Suppl\.)?(\(?[^)]+\))?$" vol))
            (if (empty? lst)
              (list (str title (last sp) vol) "" item)
              (list (str title (s/join (interleave sp lst)) (last sp) vol) "" item))
            (if (empty? lst)
              (list title vol item)
              (if (re-find #"^(\d{1,2}|[IVXLC])[0-9IVXLC²³.(), \[\]]*[a-z]?(Suppl\.)?(\(?[^)]+\))?$" (last lst))
                (parse-cleanup title (str (last lst) (last sp) vol) item (drop-last lst) (drop-last sp))
                (list (str title (s/join (interleave sp lst))) vol item))))]
    (parse-validate result)))

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
        (not (re-find #"^(\d{1,2}|[IVXLC²³.()]+)$" vol)) 
        (re-find #"^([0-9()]+|[IVXLC²³()]+)$" (last lst))))
    (if 
      (or
       (and (re-find #"\d" vol) (re-find #"\p{L}" vol))  ;; a mixture of numeric and non-numeric chars is probably an item
       (not (match-vol-pattern title vol))
       (string? (match-vol-pattern title (last lst))))
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
      (if (and (contains? @vol-patterns title) (re-find (get @vol-patterns title) (s/trim (s/join (interleave-unequal lst (rest sp))))))
        (parse-validate (list title (s/trim (s/join (interleave-unequal lst (rest sp)))) item))
        (if-let [i (index-of lst #"^(\d+[\[\]()0-9]*|[IVXLC][0-9IVXLC²³.,()\[\]]*)[a-z](?![ ,])?$")]
          (let [v (str (s/join (interleave (take-last (- (count lst) i) lst) (take-last (- (count (drop-last (rest sp))) i) (drop-last (rest sp))))) (last lst))]
            ;; if the vol looks good, then skip to title
            (if-let [vv (re-find #"^[0-9IVXLC²³., ()\[\]]+[a-z]?(Suppl\.)?(\(?[^)]+\))?" v)]
              (if (re-find #"^[0-9IVXLC²³., ()\[\]]+[a-z]?(Suppl\.)?(\(?[^)]+\))?$" v)
                (parse-cleanup title v item (drop-last (- (count lst) i) lst) (drop-last (- (count lst) i) sp))
                (parse-cleanup title (s/replace (first vv) "[, ]+$" "") (str (s/trim (substring-after v (first vv))) (last sp) item) (drop-last (- (count lst) i) lst) (drop-last (- (count lst) i) sp)))
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
    (if-let [i (last-index-of lst #"\(?(1[89]|20)\d\d\)?")]
      (parse-vol-set title vol 
        (str (s/join (interleave (take-last (dec (- (count lst) i)) lst) (take-last (dec (- (count lst) i)) sp))) item) 
        (drop-last (dec (- (count lst) i)) lst) (drop-last (dec (- (count sp) i)) sp))
      (parse-vol-set title vol item lst sp))))

(defn parse-item-col
  [title vol item lst sp]
 ;; (prn "parse-item-col")
 ;; (prn title)
 ;; (prn vol)
 ;; (prn item)
 ;; (prn lst)
 ;; (prn sp)
 ;; (prn "---------")
  (if (re-find #"^col\.?" item)
    (let [i (inc (- (count lst) (last-index-of lst #"\d")))]
      (parse-year title vol (str (s/join (interleave (take-last (inc i) lst) (take-last i sp))) (last lst)) (drop-last (inc i) lst) (drop-last i sp)))
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
        (and (re-find #"^([2-9]\d+|\d{3})$" (last lst)) (or (> (count lst) 1) (re-find #"^not(?:e|a)" item))) 
        (and (not (re-find #"\d" item)) (re-find #"\d" (last lst))))
      (parse-item-col title vol (str (last lst) (last sp) item) (drop-last lst) (drop-last sp))
      (parse-item-col title vol item lst sp))))

(defn parse-item-labeled
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
        (s/join 
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
  (let [ls (s/split cite #"(?:,? |:|(?<!(?:p|n))\.(?! |$))")
        spacers (re-seq #"(?:,? |:|(?<!(?:p|n))\.(?! |$))" cite)]
        (when (> (count ls) 1)
          (parse-item-labeled (first ls) nil (last ls) (drop-last (rest ls)) spacers))))

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
  (let [ls (s/split cite #"(?:,? |: ?|(?<!(?:p|n))\.(?! |$))")
        spacers (re-seq #"(?:,? |: ?|(?<!(?:p|n))\.(?! |$))" cite)]
    (parse-vol-set (first ls) nil "" (rest ls) spacers)))

(defn parse-citation-with-title
  "The citation comes with a title, so we don't need to figure out what the title part is"
  [cite title]
  (if (.startsWith cite title)
    (let [citepart (s/replace (substring-after cite title) #"^( |\.)" "")
          vol (with-vol-pattern {} (parse-citation-volume title)) 
          ls (s/split citepart #"(?:,? |: ?|(?<!(p|n))\.(?=(\d|pp?\.|n\.)))")
          spacers (re-seq #"(?:,? |: ?|(?<!(?:p|n))\.(?=(?:\d|pp?\.|n\.)))" cite)]
      (with-vol-pattern vol-patterns-claros
        (if-not (s/blank? (second vol))
          (parse-validate (list (first vol) (second vol) citepart))
          (parse-item-labeled (first vol) (second vol) (last ls) (drop-last ls) spacers))))
    (parse-citation cite)))


