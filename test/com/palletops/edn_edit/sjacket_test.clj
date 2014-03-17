(ns com.palletops.edn-edit.sjacket-test
  (:require
   [clojure.test :refer :all]
   [com.palletops.edn-edit.sjacket :refer :all]))

(deftest node-symbol-test
  (is (= 'a.b/c
         (node-symbol
          {:tag :symbol
           :content [{:tag :ns :content ["a.b"]}
                     "/"
                     {:tag :name :content ["c"]}]}))))
