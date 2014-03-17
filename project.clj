(defproject com.palletops/edn-edit "0.1.0-SNAPSHOT"
  :description "Edit EDN data in source files while preserving comments."
  :url "http://github.com/palletops/edn-edit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojars.trptcolin/sjacket "0.1.0.5"]
                 [org.clojure/clojure "1.5.1" :scope "provided"]]
  :profiles
  {:dev {:dependencies [[org.clojure/clojure "1.5.1"]]}
   :clojure-1.6 {:dependencies [[org.clojure/clojure "1.6.0-beta2"]]}
   :clojure-1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
   :clojure-1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}})
