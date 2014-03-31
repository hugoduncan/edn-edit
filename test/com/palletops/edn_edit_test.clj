(ns com.palletops.edn-edit-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [com.palletops.edn-edit :refer :all]))

;; (deftest transform-source-test
;;   (testing "change project coords"
;;     (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\")"
;;            (transform-source
;;             "(defproject a.b/c \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"))
;;             {:rules [transform]}))))
;;   (testing "simple content replace"
;;     (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :url \"http://github.org\")"
;;            (transform-source
;;             "(defproject a.b/c \"0.1\" :url \"http://github.com\")"
;;             (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"
;;                         :url "http://github.org"))
;;             {:rules [transform]}))))
;;   (testing "nested content replace"
;;     (testing "appending to vector"
;;       (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths [\"a\" \"b\" \"c\"])"
;;              (transform-source
;;               "(defproject a.b/c \"0.1\" :paths [\"a\" \"b\"])"
;;               (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"
;;                           :paths ["a" "b" "c"]))
;;               {:rules [transform]}))))
;;     (testing "appending to set"
;;       (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths #{\"a\" \"b\" \"c\"})"
;;              (transform-source
;;               "(defproject a.b/c \"0.1\" :paths #{\"a\" \"b\"})"
;;               (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"
;;                           :paths #{"a" "b" "c"}))
;;               {:rules [transform]})))))
;;   (testing "insert single keyword value"
;;     (is (= "(defproject d.e/f \"0.1\" :url \"http://github.org\")"
;;            (transform-source
;;             "(defproject a.b/c \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1" :url "http://github.org"))
;;             {:rules [transform]}))))
;;   (testing "remove single keyword value"
;;     (is (= "(defproject a \"0.1\" )"
;;            (transform-source
;;             "(defproject a \"0.1\" :url \"http://github.org\")"
;;             (list (list 'defproject 'a "0.1"))
;;             {:rules [transform]}))))
;;   (testing "insert keyword with map value"
;;     (is (= (str "(defproject d.e/f \"0.1\" :k "
;;                 (string/replace (pr-str {:a 1 :b 2}) "," "") ")")
;;            (transform-source
;;             "(defproject a.b/c \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1"
;;                         :k {:a 1 :b 2}))
;;             {:rules [transform]}))))
;;   (testing "insert keyword with string value"
;;     (is (= "(defproject d.e/f \"0.1\" :k \"l\")"
;;            (transform-source
;;             "(defproject a.b/c \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1" :k "l"))
;;             {:rules [transform]}))))
;;   (testing "insert keyword with vector value"
;;     (is (= "(defproject d.e/f \"0.1\" :k [\"l\"])"
;;            (transform-source
;;             "(defproject a.b/c \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1" :k ["l"]))
;;             {:rules [transform]}))))
;;   (testing "insert keyword with regex pattern value"
;;     (is (= "(defproject d.e/f \"0.1\" :k #\"abc\")"
;;            (transform-source
;;             "(defproject d.e/f \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1" :k #"abc"))
;;             {:rules [transform]}))))
;;   (testing "insert keyword with set value"
;;     (is (= "(defproject d.e/f \"0.1\" :k #{\"abc\"})"
;;            (transform-source
;;             "(defproject d.e/f \"0.1\")"
;;             (list (list 'defproject 'd.e/f "0.1" :k #{"abc"}))
;;             {:rules [transform]}))))
;;   (testing "comment round-trip"
;;     (let [src ";; comment\n(defproject d.e/f \"0.1\")"]
;;       (is (= src
;;              (transform-source
;;               src (list (list 'defproject 'd.e/f  "0.1"))
;;               {:rules [transform]}))))))

(deftest transform-edit-test
  ;; (testing "change project coords"
  ;;   (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\")"
  ;;          (transform-source
  ;;           "(defproject a.b/c \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"))
  ;;           {:rules [transform]}))))
  (testing "simple content replace"
    (is (= "{:url \"http://github.org\"}"
           (edit-source
            "{:url \"http://github.com\"}"
            (list {:url "http://github.org"})
            nil))))
  ;; (testing "nested content replace"
  ;;   (testing "appending to vector"
  ;;     (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths [\"a\" \"b\" \"c\"])"
  ;;            (transform-source
  ;;             "(defproject a.b/c \"0.1\" :paths [\"a\" \"b\"])"
  ;;             (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"
  ;;                         :paths ["a" "b" "c"]))
  ;;             {:rules [transform]}))))
  ;;   (testing "appending to set"
  ;;     (is (= "(defproject d.e/f \"0.1.0-SNAPSHOT\" :paths #{\"a\" \"b\" \"c\"})"
  ;;            (transform-source
  ;;             "(defproject a.b/c \"0.1\" :paths #{\"a\" \"b\"})"
  ;;             (list (list 'defproject 'd.e/f "0.1.0-SNAPSHOT"
  ;;                         :paths #{"a" "b" "c"}))
  ;;             {:rules [transform]})))))
  ;; (testing "insert single keyword value"
  ;;   (is (= "(defproject d.e/f \"0.1\" :url \"http://github.org\")"
  ;;          (transform-source
  ;;           "(defproject a.b/c \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1" :url "http://github.org"))
  ;;           {:rules [transform]}))))
  ;; (testing "remove single keyword value"
  ;;   (is (= "(defproject a \"0.1\" )"
  ;;          (transform-source
  ;;           "(defproject a \"0.1\" :url \"http://github.org\")"
  ;;           (list (list 'defproject 'a "0.1"))
  ;;           {:rules [transform]}))))
  ;; (testing "insert keyword with map value"
  ;;   (is (= (str "(defproject d.e/f \"0.1\" :k "
  ;;               (string/replace (pr-str {:a 1 :b 2}) "," "") ")")
  ;;          (transform-source
  ;;           "(defproject a.b/c \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1"
  ;;                       :k {:a 1 :b 2}))
  ;;           {:rules [transform]}))))
  ;; (testing "insert keyword with string value"
  ;;   (is (= "(defproject d.e/f \"0.1\" :k \"l\")"
  ;;          (transform-source
  ;;           "(defproject a.b/c \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1" :k "l"))
  ;;           {:rules [transform]}))))
  ;; (testing "insert keyword with vector value"
  ;;   (is (= "(defproject d.e/f \"0.1\" :k [\"l\"])"
  ;;          (transform-source
  ;;           "(defproject a.b/c \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1" :k ["l"]))
  ;;           {:rules [transform]}))))
  ;; (testing "insert keyword with regex pattern value"
  ;;   (is (= "(defproject d.e/f \"0.1\" :k #\"abc\")"
  ;;          (transform-source
  ;;           "(defproject d.e/f \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1" :k #"abc"))
  ;;           {:rules [transform]}))))
  ;; (testing "insert keyword with set value"
  ;;   (is (= "(defproject d.e/f \"0.1\" :k #{\"abc\"})"
  ;;          (transform-source
  ;;           "(defproject d.e/f \"0.1\")"
  ;;           (list (list 'defproject 'd.e/f "0.1" :k #{"abc"}))
  ;;           {:rules [transform]}))))
  ;; (testing "comment round-trip"
  ;;   (let [src ";; comment\n(defproject d.e/f \"0.1\")"]
  ;;     (is (= src
  ;;            (transform-source
  ;;             src (list (list 'defproject 'd.e/f  "0.1"))
  ;;             {:rules [transform]})))))
  )
