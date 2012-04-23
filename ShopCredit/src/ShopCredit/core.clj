;Problem
;
;You receive a credit C at a local store and would like to buy two items. You first walk through the store and create a list L of all available items. From this list you would like to buy two items that add up to the entire value of the credit. The solution you provide will consist of the two integers indicating the positions of the items in your list (smaller number first).
;
;Input
;
;The first line of input gives the number of cases, N. N test cases follow. For each test case there will be:
;
;    One line containing the value C, the amount of credit you have at the store.
;    One line containing the value I, the number of items in the store.
;    One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.
;    Each test case will have exactly one solution.
;
;Output
;
;For each test case, output one line containing "Case #x: " followed by the indices of the two items whose price adds up to the store credit. The lower index should be output first.
;
;Limits
;
;5 ≤ C ≤ 1000
;1 ≤ P ≤ 1000
;
;Small dataset
;
;N = 10
;3 ≤ I ≤ 100
;
;Large dataset
;
;N = 50
;3 ≤ I ≤ 2000 


(ns ShopCredit.core
  (:import (java.io BufferedReader FileReader))
  (:use clojure.string :only (split))
)
  
(defstruct test-case-model :test-case-num :store-credit :items)

(def
  ^{:doc "The start index of the set of items."}
  start-index-of-items 2)

(defn parse-raw-testcase [testCaseNum rawTestCase]
  (let [testCase (struct test-case-model)]
    (assoc testCase
           :test-case-num testCaseNum
           :store-credit (Integer/parseInt (first rawTestCase))
           :items (vec (map #(Integer/parseInt %) (split
                                                    (nth rawTestCase
                                                         start-index-of-items)
                                                    #" "))))))

(defn read-testcases-from-file [fileName]
  (with-open [rdr (BufferedReader. (FileReader. fileName))]
    (let [fileContents (line-seq rdr)]
      (let [rawTestCases (partition 3 (rest fileContents))]
        (loop [count 0 testCase (first rawTestCases) parsedTestCases '()]
          (if (nil? testCase)
            parsedTestCases
            (recur
              (inc count)
              (nth rawTestCases (inc count) nil)
              (conj parsedTestCases (parse-raw-testcase
                                      (inc count)
                                      testCase)))))))))



;Algorith to process list of items.
(defn calculate-items-to-buy [credit items]
  (let [numItems (count items)]
    (loop [outer 0 inner 1]
      (cond
        (= outer (dec numItems)) nil
        (= credit (+ (nth items outer) (nth items inner))) [(inc outer) (inc inner)]
        :else
          (cond (= inner (dec numItems)) (recur (inc outer) (inc (inc outer)))
          :else (recur outer (inc inner)))))))
