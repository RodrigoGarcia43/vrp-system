(dolist (f (list "src/neigh-configuration.lisp"
                 "src/neigh-utilities.lisp"     
                 "src/neigh-class-macros.lisp"
                 "src/neigh-classes.lisp"
                 "src/neigh-generic-functions.lisp"
                 "src/neigh-criterion.lisp"
                 "src/neigh-cardinality.lisp"
                 "src/neigh-indexer.lisp"
                 "src/neigh-compatibility.lisp"
                 "src/neigh-exploration.lisp"
           ;;    "src/neigh-statistics.lisp"
                 "src/neigh-exploration-heuristics.lisp"
                 "data/neigh-data.lisp"
                 "src/neigh-strategies.lisp"
                 "src/neigh-algorithms.lisp"
                 ))
  (format t "Loading ~a...~%" f)
  (load f))
