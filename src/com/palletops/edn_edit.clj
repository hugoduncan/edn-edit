(ns com.palletops.edn-edit
  "Rules for source transforms"
  (:require
   [clojure.string :as string :refer [blank? trim]]
   [clojure.zip :as zip]
   [com.palletops.edn-edit.common :refer :all]
   [com.palletops.edn-edit.sjacket :refer :all]
   [net.cgrand.sjacket :as sjacket]
   [net.cgrand.sjacket.parser :as parser]))

;;; # Tree Transforms

(defn data-coll-type
  "Return a collection type for the state data."
  [{:keys [loc data] :as state}]
  (let [d (::data data)]
    (cond
     (map? d) :map
     (vector? d) :vector
     (set? d) :set
     :else :list)))

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
    (println "coll-mode" data-type node-type)
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
    (apply-loc state zip/edit (constantly (delimiters (data-coll-type state))))
    state))

;;; ## Zip walker
(declare walk)

(def walk-level nil)
(defmulti walk-level
  "Walk all zip elements on the same level"
  (fn [state options]
    (data-coll-type state)))

(def coll-tags #{:list :set :map :vector :net.cgrand.sjacket.parser/root})

(defn walk
  "Walk a zip using the data and location in state, and the
  transformation rules."
  [{:keys [loc data] :as state}
   {:keys [rules root-only walk-level] :as options}]
  {:pre [loc rules walk-level]}
  (println)
  (println "walk data" data)
  (let [{:keys [loc data] :as state} (reduce #(%2 %) state rules)]
    (assert loc "rule must return a non-nil loc")
    (print-node state "walk after rules")
    (if (and (or (not root-only) (node-root? (zip/node loc)))
             (not (:finished state))
             (zip/branch? loc)
             (coll-tags (:tag (zip/node loc))))
      (let [{:keys [loc data] :as state} (coll-mode state)]
        (println "walk coll-mode" (:coll-mode state) (::data data))
        (case (:coll-mode state)
          :replace (apply-loc state replace-with-value (::data data))
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
        (apply-loc zip/up)))))

(defn walk-level-ordered
  [{:keys [loc data] :as state} options]
  {:pre [loc]}
  (loop [state (if (node-literal? (zip/node loc))
                 (apply-loc state zip/right)
                 state)
         rdata (::data data)]
    (let [state (apply-loc state skip-whitespace)]
      (if (node-literal? (zip/node (:loc state)))
        (add-new-values (assoc-in state [:data ::data] rdata))
        (if (first rdata)
          (let [{:keys [loc data] :as state} (walk
                                              (assoc-in state [:data ::data]
                                                        (first rdata))
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
  {:pre [loc (map? (::data data))]}
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
                         (apply-data update-in [::data] dissoc key-value)))
              (recur (-> val-state
                         (apply-data assoc
                                     ::data (get-in [data ::data] key-value))
                         (walk options)
                         (apply-loc zip/right)
                         (apply-data update-in [::data] dissoc key-value))))
            (recur (-> state
                       remove-value
                       (apply-loc zip/right)
                       remove-value))))
        (-> state
            add-new-kvs
            (apply-loc zip/up))))))

(defmethod walk-level :set
  [{:keys [loc data] :as state} options]
  {:pre [loc (set? (::data data))]}
  (loop [state (if (node-literal? (zip/node loc))
                 (apply-loc state zip/right)
                 state)]
    (let [{:keys [loc data] :as state} (apply-loc state skip-whitespace)]
      (if (:tag (zip/node loc))
        (let [value (read-value state)]
          (if ((::data data) value)
            (recur (-> state
                       (apply-loc zip/right)
                       (apply-data update-in [::data] disj value)))
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

;;; # Edit level walker
(def edit-level nil)
(defmulti edit-level
  "Walk all zip elements on the same level"
  (fn [state options]
    (data-coll-type state)))

(defmethod edit-level :default
  [state options]
  (walk-level state options))

(defmethod edit-level :map
  [{:keys [loc data] :as state} options]
  {:pre [loc (map? data)]}
  (loop [state state]
    (assert (:loc state))
    (let [key-state (apply-loc state skip-whitespace)
          val-state (if (:loc (apply-loc key-state zip/right))
                      (apply-loc key-state (comp skip-whitespace zip/right)))]
      (if (and key-state val-state)
        (let [key-value (read-value key-state)
              val-value (read-value val-state)
              d (get-in data [::delete key-value])
              s (get-in data [::set key-value])
              delete? (and d (keyword? d))
              walk? (and d (map? d))]
          (cond
           (or s walk?) (recur (-> val-state
                                   (apply-data update-in [::delete]
                                               get key-value)
                                   (walk options)
                                   (apply-loc zip/right)
                                   (apply-data update-in [::set]
                                               dissoc key-value)))
           delete? (recur (-> state
                              remove-value
                              (apply-loc zip/right)
                              remove-value))
           :else (recur (-> val-state
                            (apply-loc zip/right)))))
        (-> state
            (add-new-kvs (get-in state [:data ::set]))
            (apply-loc zip/up))))))


;;; # Top level API function
(defn transform-source
  "Transform the source using options :rules and the data in the
  data map."
  ([code data options]
     {:pre [(map? options)]}
     (let [options (merge
                    {:rules [transform]
                     :walk-level walk-level}
                    options)
           data {::data data}]
       (if (blank? (trim code))
         code
         (-> (walk {:loc (zip/xml-zip (parser/parser code))
                    :data data}
                   options)
             :loc
             zip/root
             sjacket/str-pt))))
  ([code data]
     (transform-source code data {})))

(defn edit-source
  "Transform the source using options :rules and the data in the
  data map."
  ([code set-values delete-values options]
     {:pre [(map? options)]}
     (let [options (merge
                    {:rules [transform]
                     :walk-level edit-level}
                    options)
           data {::set set-values
                 ::delete delete-values}]
       (if (blank? (trim code))
         code
         (-> (walk {:loc (zip/xml-zip (parser/parser code))
                    :data data}
                   options)
             :loc
             zip/root
             sjacket/str-pt))))
  ([code set-values delete-values]
     (edit-source code set-values delete-values {})))
