(ns com.palletops.edn-edit.common
  (:require
   [clojure.zip :as zip]
   [com.palletops.edn-edit.sjacket :refer :all]
   [net.cgrand.sjacket :as sjacket]))

;;; # Debug helper
(defn print-node [{:keys [loc] :as state} comment]
  {:pre [loc]}
  (println comment (pr-str (zip/node loc)))
  state)

;;; # Apply functions for specific keys in the state map

;;; The state consists of a zip location, :loc, and a data value,
;;; :data.

(defn apply-loc
  "Apply f to the state loc."
  [state f & args]
  (apply update-in state [:loc] f args))

(defn apply-data
  "Apply f to the state data."
  [state f & args]
  (apply update-in state [:data] f args))


;;; # clojure.zip extensions
(defn remove-node
  "Remove a node, keeping the location at the same level if possible,
  or returning the parent if there is no previous node.  Assumes nodes
  are not repeated down tree branches."
  [loc]
  (if-let [l (and (zip/left loc) (zip/node (zip/left loc)))]
    (loop [loc (zip/remove loc)]
      (if (= (zip/node loc) l)
        loc
        (recur (zip/up loc))))
    (zip/remove loc))) ; will get the prev loc

;;; # sjacket node helpers
(def node-clj-ws? (some-fn node-comment? node-whitespace? node-newline?))

(defn skip-whitespace
  "Skip any (clojure) whitespace at the current level."
  [loc]
  {:pre [loc] :post [%]}
  (if (and (zip/node loc)
           (node-clj-ws? (zip/node loc))
           (zip/right loc))
    (skip-whitespace (zip/right loc))
    loc))

(defn delete-whitespace
  "Skip any whitepace at the current level."
  [loc]
  {:pre [loc] :post [%]}
  (loop [loc loc]
    (if (node-clj-ws? (zip/node loc))
      (let [loc (remove-node loc)]
        (if-let [r (zip/right loc)]
          (recur r)
          loc))
      loc)))

(defn insert-whitespace
  [loc ws]
  (->
    loc
    (zip/insert-right (whitespace ws))
    zip/right))

(defn insert
  "Insert values at the current location and level."
  [loc vs]
  (sjacket/insert-pts loc :next vs nil))

(defn replace-with-value
  "Replace a node with a representation of the state's data."
  [loc value]
  {:pre [loc]}
  (-> loc
      remove-node
      (insert [value])))

;;; # State modifiers
(defn add-new-kvs
  "Add the data as new key, value pairs at the current level."
  ([{:keys [data] :as state}]
     (add-new-kvs state (::data data)))
  ([state data]
     (if (seq data)
       (-> state
           (apply-loc zip/rightmost)
           (apply-loc zip/left)
           (apply-loc insert-whitespace " ")
           (apply-loc insert (apply concat data)))
       state)))

(defn add-new-values
  "Add the data as new values at the current level."
  ([{:keys [loc data] :as state}]
     (add-new-values state (::data data)))
  ([state data]
     (if (seq data)
       (-> state
           (apply-loc zip/rightmost)
           (apply-loc zip/left)
           (apply-loc insert-whitespace " ")
           (apply-loc insert data))
       state)))

(defn add-new-value
  "Add the data as a new value at the current level."
  ([{:keys [loc data] :as state}]
     (add-new-value state (::data data)))
  ([state data]
     (-> state
         (apply-loc zip/rightmost)
         (apply-loc zip/left)
         (apply-loc insert [data]))))

(defn remove-value
  "Remove a value, and any whitespace after it."
  [{:keys [loc data] :as state}]
  {:pre [loc]}
  (let [loc (remove-node loc)]
    (if-let [r (zip/right loc)]
      {:loc (-> r delete-whitespace zip/left) :data data}
      {:loc loc :data data})))

(defn state-node
  [{:keys [loc data] :as state}]
  (zip/node loc))

(defn read-value
  "Read the value at node'"
  [node]
  (read-string (sjacket/str-pt node)))
