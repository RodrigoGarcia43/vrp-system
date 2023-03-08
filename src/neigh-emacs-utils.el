(progn
  (defvar neigh-tangle-list
    `("neigh-class-macros.org"
      "neigh-classes.org"
      "neigh-criterion.org"
      "neigh-compatibility.org"
      "neigh-cardinality.org"
      "neigh-indexer.org"
      "neigh-exploration.org"
      "neigh-statistics.org"
      "neigh-exploration-heuristics.org"
      "neigh-data.org"
      "neigh-search-strategies.org"
      "neigh-algorithms.org")
    "A list with all the files that should be tangled for the system neigh.")

  (defun neigh-tangle-all-files ()
    "Tangles all the required files for the vrp-suite."
    (interactive)
    (dolist (file neigh-tangle-list)
      (org-babel-tangle-file file))))
