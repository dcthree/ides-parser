(ns ides-parser.core-test
  (:require [clojure.test :refer :all]
            [ides-parser.core :refer :all]))

(deftest parse-citation-tests
  (testing "parse-citation tests"
    (is (= (parse-citation "IG I³ 40") '("IG" "I³" "40")) "IG I³ 40")
    (is (= (parse-citation "IG I[3].40") '("IG" "I[3]" "40")) "IG I[3].40")
    (is (= (parse-citation "IG IX,1(2) 1:133,b") '("IG" "IX,1(2) 1" "133,b")))))

(deftest parse-citation-with-title-tests
  (testing "parse-citation-with-title tests"
    (is (= (parse-citation-with-title "AJA N.S. 58, 1954 lám. 43, 2" "AJA") '("AJA" "N.S. 58, 1954" "lám. 43, 2") ))))
