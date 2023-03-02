(in-package :vrp)

(def-vrp-class use-neigh-tree ()
    ()
    :documentation "A class to represent a search strategy where we use the neghborhood tree to generate solutions")

(def-vrp-class use-eval-graph (use-neigh-tree)
    ()
    :documentation "A class to represent a search strategy where we use the eval graph to evaluate solutions")

(def-vrp-class there-is-a-best-solution ()
    ()
    :documentation "A class to represent a search strategy where we have a best-solution value that we update during the search.")

(def-vrp-class return-best-solution
    (there-is-a-best-solution)
    ()
    :documentation "A class to represent a search strategy where we always return the value of best-solution.  If there isn't a best-solution, we return nil.")

(def-vrp-class return-best-cost
    (there-is-a-best-solution)
    ()
    :documentation "A class to represent a selection strategy where we always return the cost of the best-neighbor found.  If there is not a best neighbor we return 0.")

(def-vrp-class has-candidates-for-best-neighbor
    ()
    () ;; no slots
    :documentation "A class to represent a search strategy where we collect a set of neighbors as candidates for the best-neighbor.")

(def-vrp-class exhaustive-neighborhood-search-strategy ()
    ()
    :documentation "A class to represent an exhaustive search of a neighborhood."
    :constructor (exhaustive-neighborhood-search-strategy ())
    :print-object-string ("<exhaustive-search>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +exhaustive-search-strategy+
  (exhaustive-neighborhood-search-strategy))

(def-vrp-class random-neighborhood-search-strategy ()
    ((neighborhood-size
      :documentation "The number of neighbors that should be explored."))
    :documentation "A class to represent an exhaustive search of a neighborhood."
    :constructor (random-neighborhood-search-strategy
                  (&optional (neighborhood-size 100)))
    :print-object-string ("<random-search: ~a>" neighborhood-size)
    :slots-for-obj= (neighborhood-size)
    :slots-for-clone (neighborhood-size))

(defparameter +random-search-strategy+
  (random-neighborhood-search-strategy 500))

(def-vrp-class jump-around-search-strategy
    (random-neighborhood-search-strategy)
    ()
    :documentation "A class to represent a `jump-around' search of a neighborhood."
    :constructor (jump-around-search-strategy
                  (&optional (neighborhood-size 100)))
    :print-object-string ("<jump-around-search: ~a>" neighborhood-size)
    :slots-for-obj= (neighborhood-size)
    :slots-for-clone (neighborhood-size))

(defparameter +jump-around-strategy+
  (jump-around-search-strategy 10))

(def-vrp-class best-improvement-search-strategy
    (
     return-best-solution
     use-eval-graph)

    ()
    :documentation "A class to represent a best-improvement strategy in the search of a neighborhood."
    :constructor (best-improvement-search-strategy ())
    :print-object-string ("<best-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +best-improvement+
  (best-improvement-search-strategy))

(def-vrp-class first-improvement-search-strategy
    (
     return-best-solution
     use-eval-graph)

    ()
    :documentation "A class to represent a first-improvement strategy in the search of a neighborhood."
    :constructor (first-improvement-search-strategy ())
    :print-object-string ("<first-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +first-improvement+
  (first-improvement-search-strategy))

(def-vrp-class random-improvement-with-candidates-selection-strategy
    (
     return-best-solution
     has-candidates-for-best-neighbor
     use-eval-graph)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood."
    :constructor (random-improvement-selection-strategy ())
    :print-object-string ("<random-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(def-vrp-class random-improvement-selection-strategy
    (
     return-best-solution
     use-eval-graph)

    ((acceptance-ratio
      :initform 0.5
      :documentation "The acceptance probability."))
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood."
    :constructor (random-improvement-selection-strategy
                  (&optional (acceptance-ratio 0.8)))
    :print-object-string ("<random-improvement-strategy>")
    :slots-for-obj= (acceptance-ratio)
    :slots-for-clone (acceptance-ratio))

(defparameter +random-improvement+
  (random-improvement-selection-strategy))

(def-vrp-class jump-around-return-last-neighbor
    (use-eval-graph)

    ()
    :documentation "A class to represent the simplest jump-around selection strategy."
    :constructor (jump-around-return-last-neighbor ())
    :print-object-string ("<jump-around-last-neighbor>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +jump-around-return-last-neighbor+
  (make-instance 'jump-around-return-last-neighbor))

(def-vrp-class return-last-neighbor-selection-strategy
    (return-best-cost
     return-best-solution
     use-eval-graph)
    () ;; no slots
    :documentation "A class to represent a search strategy where we return the last neighbor independently of its cost.")

(defparameter +return-last-neighbor+
  (make-instance 'return-last-neighbor-selection-strategy))

(def-vrp-class basic-neighborhood-strategy-blueprint ()
    ((initializations-inside-the-let
      :documentation "What we should initialize inside the let.")
     (initializations-outside-the-let
      :documentation "What we should initialize outside the let.")
     (loop-conditional
      :documentation "A list with the heading of the macro calls.")
     (code-inside-the-loop
      :documentation "What we should do with the current neigborh.")
     (code-outside-the-loop
      :documentation "What we should do after the iteration through all the neigborhs is over.")
     (return-code
      :documentation "What we should return from the exploration."))
    :documentation "A class to represent a description of a neigborhood strategy.  It is used in the automatic generation of neigborhood search algorithms."
    :constructor (basic-strategy-blueprint
                  (&key (initializations-inside-the-let nil)
                        (initializations-outside-the-let nil)
                        (code-inside-the-loop nil)
                        (loop-conditional nil)
                        (code-outside-the-loop nil)
                        (return-code nil)))

    :print-object-string ("<basic-strategy-blueprint>")
    :slots-for-obj= (initializations-inside-the-let
                     initializations-outside-the-let
                     loop-conditional
                     code-inside-the-loop
                     code-outside-the-loop
                     return-code)
    :slots-for-clone (initializations-inside-the-let
                     initializations-outside-the-let
                     loop-conditional
                     code-inside-the-loop
                     code-outside-the-loop
                     return-code))
