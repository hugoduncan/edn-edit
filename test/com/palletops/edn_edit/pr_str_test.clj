(ns com.palletops.edn-edit.pr-str-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [com.palletops.edn-edit.pr-str :as edn]))

(deftest transform-source-test
  (testing "change project coords"
    (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\")"
           (edn/pr-str
            (list '(defproject d.e/f "0.1.0-SNAPSHOT"))
            "(defproject a.b/c \"0.1\")"))))
  (testing "simple content replace"
    (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :url \"http://github.org\")"
           (edn/pr-str
            (list '(defproject d.e/f "0.1.0-SNAPSHOT"
                     :url "http://github.org"))
            "(defproject a.b/c \"0.1\" :url \"http://github.com\")"))))
  (testing "nested content replace"
    (testing "appending to vector"
      (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths [\"a\" \"b\" \"c\"])"
             (edn/pr-str
              (list '(defproject d.e/f "0.1.0-SNAPSHOT" :paths ["a" "b" "c"]))
              "(defproject a.b/c \"0.1\" :paths [\"a\" \"b\"])"))))
    (testing "appending to set"
      (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths #{\"a\" \"b\" \"c\"})"
             (edn/pr-str
              (list '(defproject d.e/f "0.1.0-SNAPSHOT" :paths #{"a" "b" "c"}))
              "(defproject a.b/c \"0.1\" :paths #{\"a\" \"b\"})")))))
  (testing "insert single keyword value"
    (is (= "(defproject d.e/f \"0.1\" :url \"http://github.org\")"
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :url "http://github.org"))
            "(defproject a.b/c \"0.1\")"))))
  (testing "remove single keyword value"
    (is (= "(defproject a \"0.1\" )"
           (edn/pr-str
            (list '(defproject a "0.1"))
            "(defproject a \"0.1\" :url \"http://github.org\")"))))
  (testing "insert keyword with map value"
    (is (= (str "(defproject d.e/f \"0.1\" :k "
                (string/replace (pr-str {:a 1 :b 2}) "," "") ")")
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :k {:a 1 :b 2}))
            "(defproject a.b/c \"0.1\")"))))
  (testing "insert keyword with string value"
    (is (= "(defproject d.e/f \"0.1\" :k \"l\")"
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :k "l"))
            "(defproject a.b/c \"0.1\")"))))
  (testing "insert keyword with vector value"
    (is (= "(defproject d.e/f \"0.1\" :k [\"l\"])"
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :k ["l"]))
            "(defproject a.b/c \"0.1\")"))))
  (testing "insert keyword with regex pattern value"
    (is (= "(defproject d.e/f \"0.1\" :k #\"abc\")"
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :k #"abc"))
            "(defproject d.e/f \"0.1\")"))))
  (testing "insert keyword with set value"
    (is (= "(defproject d.e/f \"0.1\" :k #{\"abc\"})"
           (edn/pr-str
            (list '(defproject d.e/f "0.1" :k #{"abc"}))
            "(defproject d.e/f \"0.1\")"))))
  (testing "comment round-trip"
    (let [src ";; comment\n(defproject d.e/f \"0.1\")"]
      (is (= src
             (edn/pr-str (list '(defproject d.e/f  "0.1")) src))))))
