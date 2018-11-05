## 1.1.1
Changed syntax to apply function to collection built up in the range.

Before, faconne implicitly inferred when a transform used reducing logic on a collection (for example `(apply max [v])`, which requires [v] be fully constructed before applying the surrounding context). But, this led to a bug (#3) where it inappropriately infer that this was needed, making other useful types of transforms inexpressible. I concluded that it was not possible for faconne to infer this, and that the user would have to specify it. This is done with `^:expand` metadata: `(apply max [v])` becomes `(apply max ^:expand [v])`.

## 1.1.0

- Revamped README with examples inspired by a year of using faconne in a production code base.
- Allowing functions to be called on collections built up in range. For example
```clj (f/transform {:x [1 2 3], :y [4 5 6]} {k [v]} (apply max [v])) ;; => 6```
