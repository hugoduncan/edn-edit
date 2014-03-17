(ns com.palletops.edn-edit
  "Rules for source transforms"
  (:require
   [clojure.string :as string :refer [blank? trim]]
   [clojure.zip :as zip]
   [com.palletops.edn-edit.sjacket :refer :all]
   [net.cgrand.sjacket :as sjacket]
   [net.cgrand.sjacket.parser :as parser]))

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
(defn- remove-node
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

(defn- insert
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
(defn- add-new-kvs
  "Add the data as new key, value pairs at the current level."
  [{:keys [loc data] :as state}]
  (if (seq data)
    (-> state
        (apply-loc zip/rightmost)
        (apply-loc zip/left)
        (apply-loc insert-whitespace " ")
        (apply-loc insert (apply concat data)))
    state))

(defn- add-new-values
  "Add the data as new values at the current level."
  [{:keys [loc data] :as state}]
  (if (seq data)
    (-> state
        (apply-loc zip/rightmost)
        (apply-loc zip/left)
        (apply-loc insert-whitespace " ")
        (apply-loc insert data))
    state))

(defn- add-new-value
  "Add the data as a new value at the current level."
  [{:keys [loc data] :as state}]
  (-> state
      (apply-loc zip/rightmost)
      (apply-loc zip/left)
      (apply-loc insert [data])))

(defn- remove-value
  "Remove a value, and any whitespace after it."
  [{:keys [loc data] :as state}]
  {:pre [loc]}
  (let [loc (remove-node loc)]
    (if-let [r (zip/right loc)]
      {:loc (-> r delete-whitespace zip/left) :data data}
      {:loc loc :data data})))

(defn read-value
  "Read the value at the state's :loc"
  [{:keys [loc] :as state}]
  (read-string (sjacket/str-pt (zip/node loc))))

;;; # Tree Transforms

(defn data-coll-type
  "Return a collection type for the state data."
  [{:keys [loc data] :as state}]
  (cond
   (map? data) :map
   (vector? data) :vector
   (set? data) :set
   :else :list))

(defn node-coll-type [{:keys [loc data] :as state}]
  ;; {:pre [(#{:list :net.cgrand.sjacket.parser/root} (:tag node))]}
  (condp = (first (:content (zip/node loc)))
    "{" :map
    "[" :vector
    "#{" :set
    :list))

(defn coll-mode [{:keys [loc data] :as state}]
  (let [data-type (data-coll-type state)
        node-type (node-coll-type state)]
    (assoc state
      :coll-mode (cond
                  (= data-type node-type) :update-in-place
                  (every? #{:list :vector} [data-type node-type]) :update-type
                  :else :replace))))

(def open-delimiters
  {:map "{"
   :set "#{"
   :vector "["
   :list "("})

(def close-delimiters
  {:map "}"
   :set "}"
   :vector "]"
   :list ")"})

(defn replace-delimiters
  [{:keys [loc data] :as state} delimiters]
  (if (string? (zip/node loc))
    (apply-loc zip/edit (constantly (delimiters (data-coll-type state))))))

;;; ## Zip walker
(declare walk)

(def walk-level nil)
(defmulti walk-level
  "Walk all zip elements on the same level"
  (fn [{:keys [loc data] :as state} options]
    (data-coll-type state)))

(def coll-tags #{:list :set :map :vector :net.cgrand.sjacket.parser/root})

(defn walk
  "Walk a zip using the data and location in state, and the
  transformation rules."
  [{:keys [loc data] :as state} {:keys [rules root-only] :as options}]
  {:pre [loc rules]}
  (let [{:keys [loc data] :as state} (reduce #(%2 %) state rules)]
    (if (and (or (not root-only) (node-root? (zip/node loc)))
             (not (:finished state))
             (zip/branch? loc)
             (coll-tags (:tag (zip/node loc))))
      (let [{:keys [loc data]:as state} (coll-mode state)]
        (case (:coll-mode state)
          :replace (apply-loc loc replace-with-value data)
          :update-in-place
          (-> state
              (apply-loc zip/down)
              (walk-level options))
          :update-type
          (-> state
              (apply-loc zip/down)
              (replace-delimiters open-delimiters)
              (walk-level options)
              (replace-delimiters close-delimiters))))
      state)))

;;; ## Collection specific walkers
(defmethod walk-level :net.cgrand.sjacket.parser/root
  [{:keys [loc data] :as state} options]
  {:pre [loc]}
  (loop [state state]
    (let [{:keys [loc data] :as state} (walk state options)
          r (zip/right loc)]
      (if r
        (recur {:loc r :data data})
        {:loc (zip/up loc) :data data}))))

(defn walk-level-ordered
  [{:keys [loc data] :as state} options]
  {:pre [loc]}
  (loop [state (if (node-literal? (zip/node loc))
                 (apply-loc state zip/right)
                 state)
         rdata data]
    (let [state (apply-loc state skip-whitespace)]
      (if (node-literal? (zip/node (:loc state)))
        (add-new-values (assoc state :data rdata))
        (if (first rdata)
          (let [{:keys [loc data] :as state} (walk
                                              (assoc state :data (first rdata))
                                              options)
                r (zip/right loc)]
            (if r
              (recur {:loc r :data data} (rest rdata))
              {:loc (zip/up loc) :data data}))
          (remove-value state))))))

(defmethod walk-level :list
  [{:keys [loc data] :as state} options]
  {:pre [loc]}
  (walk-level-ordered state options))

(defmethod walk-level :vector
  [{:keys [loc data] :as state} options]
  {:pre [loc]}
  (walk-level-ordered state options))

(defmethod walk-level :map
  [{:keys [loc data] :as state} options]
  {:pre [loc (map? data)]}
  (loop [state state]
    (assert (:loc state))
    (let [key-state (apply-loc state skip-whitespace)
          val-state (if (:loc (apply-loc key-state zip/right))
                      (apply-loc key-state (comp skip-whitespace zip/right)))]
      (if (and key-state val-state)
        (let [key-value (read-value key-state)
              val-value (read-value val-state)]
          (if (contains? data key-value)
            (if (= val-value (get data key-value))
              (recur (-> val-state
                         (apply-loc zip/right)
                         (apply-data dissoc key-value)))
              (recur (-> val-state
                         (assoc :data (get data key-value))
                         (walk options)
                         (apply-loc zip/right)
                         (assoc :data (dissoc data key-value)))))
            (recur (-> state
                       remove-value
                       (apply-loc zip/right)
                       remove-value))))
        (-> state
            add-new-kvs
            (apply-loc zip/up))))))

(defmethod walk-level :set
  [{:keys [loc data] :as state} options]
  {:pre [loc (set? data)]}
  (loop [state (if (node-literal? (zip/node loc))
                 (apply-loc state zip/right)
                 state)]
    (let [{:keys [loc data] :as state} (apply-loc state skip-whitespace)]
      (if (:tag (zip/node loc))
        (let [value (read-value state)]
          (if (data value)
            (recur (-> state
                       (apply-loc zip/right)
                       (apply-data disj value)))
            (recur (-> state
                       remove-value))))
        (-> state
            add-new-values
            (apply-loc zip/up))))))


;;; # Generic data transform
(def transform nil)
(defmulti transform (fn [{:keys [loc data] :as state}]
                      {:pre [loc]}
                      (let [n (zip/node loc)]
                        (if (map? n)
                          (:tag n)
                          (type n)))))

(defmethod transform :default
  [{:keys [loc data] :as state}]
  {:pre [loc] :post [%]}
  state)

(defmethod transform :string
  [{:keys [loc data] :as state}]
  {:pre [loc] :post [%]}
  (if (= data (node-string (zip/node loc)))
    state
    (apply-loc state replace-with-value data)))

(defmethod transform :keyword
  [{:keys [loc data] :as state}]
  {:pre [loc] :post [%]}
  (if (= data (node-keyword (zip/node loc)))
    state
    (apply-loc state replace-with-value data)))

(defmethod transform :symbol
  [{:keys [loc data] :as state}]
  {:pre [loc] :post [%]}
  (if (= data (node-symbol (zip/node loc)))
    state
    (apply-loc state replace-with-value data)))

(defn transform-source
  "Transform the source using options :rules and the data in the
  data map."
  ([code data options]
     {:pre [(map? options) (:rules options)]}
     (if (blank? (trim code))
       code
       (-> (walk {:loc (zip/xml-zip (parser/parser code))
                  :data data}
                 options)
           :loc
           zip/root
           sjacket/str-pt)))
  ([code data]
     (transform-source code data {:rules transform})))
