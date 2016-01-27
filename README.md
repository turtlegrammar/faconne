Plum offers a way to create data transformation functions declaritively. For example:

```clj
(def swap-keys (plum.core/collector {k1 {k2 v}} {k2 {k1 v}}))
```

will swap the order of keys in a nested map.

Or, given a map of professors to classes to a vector of students, where a student is of the form `{:name :grade}`, we can easily retrieve a map from students to a set of their professors:

```clj
(def get-profs-per-student (plum.core/collector {prof {_ [student]}}
                                                {(:name student) #{prof}}))
;; or with map destructuring
(def get-profs-per-student2 (plum.core/collector {prof {_ [{:let {name :name}}]}}
                                                 {name #{prof}}))
```

See the tests for more examples.
