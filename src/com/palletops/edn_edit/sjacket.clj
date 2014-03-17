(ns com.palletops.edn-edit.sjacket
  "Additional functions around sjacket"
  (:require
   [clojure.zip :as zip]
   [net.cgrand.sjacket :as sjacket])
  (:import net.cgrand.parsley.Node))


;;; Break open some things from sjacket
(def sjacket-insert #'sjacket/insert)
(defmacro sjacket-subedit-> [loc & edits]
  `(#'sjacket/subedit ~loc
     (fn [loc#] (-> loc# ~@edits))))

;;; # sjacket extension

;;; There are currently no insert-pt-unknown methods in sjacket for
;;; maps, sets, or regex patterns.

;;; The vector inserter in sjacket uses the :list tag.

(defmethod sjacket/insert-pt-unknown clojure.lang.IPersistentMap
  [loc where expr ctx]
  (-> loc
      (sjacket-insert where {:tag :list :content ["{" "}"]})
      (sjacket-subedit->
       zip/down
       (sjacket/insert-pts :next (apply concat expr) ctx))))

(defmethod sjacket/insert-pt-unknown clojure.lang.IPersistentVector
  [loc where expr ctx]
  (-> loc
      (sjacket-insert where {:tag :list :content ["[" "]"]})
      (sjacket-subedit->
       zip/down
       (sjacket/insert-pts :next expr ctx))))

(defmethod sjacket/insert-pt-unknown clojure.lang.IPersistentSet
  [loc where expr ctx]
  (-> loc
      (sjacket-insert where {:set :list :content ["#{" "}"]})
      (sjacket-subedit->
       zip/down
       (sjacket/insert-pts :next expr ctx))))

(defmethod sjacket/insert-pt-unknown java.util.regex.Pattern
  [loc where expr ctx]
  (-> loc
      (sjacket-insert where {:tag :regex :content ["#"]})
      (sjacket-subedit->
       zip/down
       (sjacket/insert-pts :next [(str expr)] ctx))))

;;; # Node helpers
(defn whitespace
  [s]
  (Node. :whitespace [s]))

(defn node-literal? [node]
  (string? node))

(defn node-whitespace? [node]
  (= :whitespace (:tag node)))

(defn node-string? [node]
  (= :string (:tag node)))

(defn node-comment? [node]
  (= :comment (:tag node)))

(defn node-newline? [node]
  (= :newline (:tag node)))

(defn node-symbol? [node]
  (= :symbol (:tag node)))

(defn node-keyword? [node]
  (= :keyword (:tag node)))

(defn node-list? [node]
  (= :list (:tag node)))

(defn node-root? [node]
  (= :net.cgrand.sjacket.parser/root (:tag node)))

(defn node-symbol [node]
  (assert (node-symbol? node))
  (if (= 1 (count (-> node :content)))
    (symbol (-> node :content first :content first))
    (symbol (-> node :content first :content first)
            (-> node :content (nth 2) :content first))))

(defn node-keyword [node]
  (assert (node-keyword? node))
  (keyword (-> node :content second :content first)))

(defn node-string [node]
  (assert (node-string? node))
  (keyword (-> node :content second :content first)))

(defn node-list-first [node]
  (assert (node-list? node))
  (-> node :content second))
