# edn-edit

A Clojure library for editing EDN in source files.

## Install

Add the library to your project.clj:

```clj
:dependencies [[com.palletops/edn-edit "0.1.0-SNAPSHOT"]]
```

## Usage

The main function is `transform-source`, taking a source string, a
data structure and a map with at least a `:rules` keyword specifying
the transformation rules to apply to the forms in the data structure.

```clj
(require '[com.palletops.edn-edit :refer [transform-source]])
(transform-source
 ";;; my project
(defproject a.b/c \"0.1\")"
 (list 'defproject a.b/d "0.2"))
=> ";;; my project
(defproject a.b/d \"0.2\")"
```

## License

Copyright © 2014 Hugo Duncan

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
