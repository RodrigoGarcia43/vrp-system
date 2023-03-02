(font-lock-add-keywords 'lisp-mode
 '(("(\\(defmacro!\\)" 1 font-lock-keyword-face)
   ("(defmacro! +\\([^ ]+\\)" 1 font-lock-function-name-face)
   ("(\\(defbehavior\\)" 1 font-lock-keyword-face)
   ("(defbehavior +\\([^ ]+\\)" 1 font-lock-function-name-face)
   ("(\\(then\\)" 1 font-lock-keyword-face)
   ("(\\(else\\)" 1 font-lock-keyword-face)
   ("(\\(the-following\\)" 1 font-lock-keyword-face)))

(defun vrp-org-babel-tangle-collect-blocks-no-tests
    (&optional language tangle-file exclude-file)
  "Collect source blocks in the current Org file, excluding blocks
tangled to exclude-file.
Return an association list of source-code block specifications of
the form used by `org-babel-spec-to-string' grouped by language.
Optional argument LANGUAGE can be used to limit the collected
source code blocks by language.  Optional argument TANGLE-FILE
can be used to limit the collected code blocks by target file."
  (setq exclude-file (or exclude-file "../src/vrp-tests.lisp"))
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
             (org-with-wide-buffer
              (org-with-limited-levels (outline-previous-heading)))))
        (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
          (setq counter 1)
          (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
        (let* ((info (org-babel-get-src-block-info 'light))
               (src-lang (nth 0 info))
               (src-tfile (cdr (assq :tangle (nth 2 info)))))
          (unless (or (string= src-tfile "no")
                      (and tangle-file (not (equal tangle-file src-tfile)))
                      (and language (not (string= language src-lang)))
                      (and exclude-file (string= exclude-file src-tfile)))
            ;; Add the spec for this block to blocks under its
            ;; language.
            (let ((by-lang (assoc src-lang blocks))
                  (block (org-babel-tangle-single-block counter)))
              (if by-lang (setcdr by-lang (cons block (cdr by-lang)))
                (push (cons src-lang (list block)) blocks)))))))
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b))))
            (nreverse blocks))))

(defun vrp-org-babel-tangle-no-tests
    (&optional arg target-file lang exclude-file)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language."
  (interactive "P")
  (setq exclude-file (or exclude-file "../src/vrp-tests.lisp"))
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; Possibly Restrict the buffer to the current code block
  (save-restriction
    (save-excursion
      (when (equal arg '(4))
    (let ((head (org-babel-where-is-src-block-head)))
      (if head
          (goto-char head)
        (user-error "Point is not in a source code block"))))
      (let ((block-counter 0)
        (org-babel-default-header-args
         (if target-file
         (org-babel-merge-params org-babel-default-header-args
                     (list (cons :tangle target-file)))
           org-babel-default-header-args))
        (tangle-file
         (when (equal arg '(16))
           (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
           (user-error "Point is not in a source code block"))))
        path-collector)
    (mapc ;; map over all languages
     (lambda (by-lang)
       (let* ((lang (car by-lang))
          (specs (cdr by-lang))
          (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
          (lang-f (intern
               (concat
                (or (and (cdr (assoc lang org-src-lang-modes))
                     (symbol-name
                      (cdr (assoc lang org-src-lang-modes))))
                lang)
                "-mode")))
          she-banged)
         (mapc
          (lambda (spec)
        (let ((get-spec (lambda (name) (cdr (assoc name (nth 4 spec))))))
          (let* ((tangle (funcall get-spec :tangle))
             (she-bang (let ((sheb (funcall get-spec :shebang)))
                                     (when (> (length sheb) 0) sheb)))
             (tangle-mode (funcall get-spec :tangle-mode))
             (base-name (cond
                     ((string= "yes" tangle)
                      (file-name-sans-extension
                       (nth 1 spec)))
                     ((string= "no" tangle) nil)
                     ((> (length tangle) 0) tangle)))
             (file-name (when base-name
                      ;; decide if we want to add ext to base-name
                      (if (and ext (string= "yes" tangle))
                      (concat base-name "." ext) base-name))))
            (when file-name
              ;; Possibly create the parent directories for file.
              (let ((m (funcall get-spec :mkdirp))
                (fnd (file-name-directory file-name)))
            (and m fnd (not (string= m "no"))
                 (make-directory fnd 'parents)))
              ;; delete any old versions of file
              (and (file-exists-p file-name)
               (not (member file-name (mapcar #'car path-collector)))
               (delete-file file-name))
              ;; drop source-block to file
              (with-temp-buffer
            (when (fboundp lang-f) (ignore-errors (funcall lang-f)))
            (when (and she-bang (not (member file-name she-banged)))
              (insert (concat she-bang "\n"))
              (setq she-banged (cons file-name she-banged)))
            (org-babel-spec-to-string spec)
            ;; We avoid append-to-file as it does not work with tramp.
            (let ((content (buffer-string)))
              (with-temp-buffer
                (when (file-exists-p file-name)
                  (insert-file-contents file-name))
                (goto-char (point-max))
                ;; Handle :padlines unless first line in file
                (unless (or (string= "no" (cdr (assq :padline (nth 4 spec))))
                    (= (point) (point-min)))
                  (insert "\n"))
                (insert content)
                (write-region nil nil file-name))))
              ;; if files contain she-bangs, then make the executable
              (when she-bang
            (unless tangle-mode (setq tangle-mode #o755)))
              ;; update counter
              (setq block-counter (+ 1 block-counter))
              (unless (assoc file-name path-collector)
            (push (cons file-name tangle-mode) path-collector))))))
          specs)))
     (if (equal arg '(4))
         (org-babel-tangle-single-block 1 t)
       (vrp-org-babel-tangle-collect-blocks-no-tests
        lang tangle-file exclude-file)))
    (message "Tangled %d code block%s from %s" block-counter
         (if (= block-counter 1) "" "s")
         (file-name-nondirectory
          (buffer-file-name
           (or (buffer-base-buffer) (current-buffer)))))
    ;; run `org-babel-post-tangle-hook' in all tangled files
    (when org-babel-post-tangle-hook
      (mapc
       (lambda (file)
         (org-babel-with-temp-filebuffer file
           (run-hooks 'org-babel-post-tangle-hook)))
       (mapcar #'car path-collector)))
    ;; set permissions on tangled files
    (mapc (lambda (pair)
        (when (cdr pair) (set-file-modes (car pair) (cdr pair))))
          path-collector)
    (mapcar #'car path-collector)))))

(defun vrp-org-babel-tangle-file-no-tests
    (file &optional target-file lang exclude-file)
  "Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language.
Return a list whose CAR is the tangled file name."
  (interactive "fFile to tangle: \nP")
  (setq exclude-file (or exclude-file "../src/vrp-tests.lisp"))

  (let ((visited-p (get-file-buffer (expand-file-name file)))
    to-be-removed)
    (prog1
    (save-window-excursion
      (find-file file)
      (setq to-be-removed (current-buffer))
      (mapcar #'expand-file-name
              (vrp-org-babel-tangle-no-tests nil
                                             target-file
                                             lang
                                             exclude-file)))
      (unless visited-p
        (kill-buffer to-be-removed)))))

(defvar vrp-tangle-list
  `("vrp-suite.org"
    "vrp-classes.org"
    "vrp-moments.org"
    "vrp-data.org"
    "vrp-neighborhood-exploration-macros.org"
    "vrp-neighborhood-simulation.org"
    "vrp-neighborhood-operations.org"
    "vrp-delta-cost.org"
    "vrp-search-strategies.org"
    "vrp-criteria.org"
    "vrp-algorithms.org"

    "neigh-class-macros.org"
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
  "A list with all the files that should be tangled.")

(defun vrp-tangle-all-files ()
  "Tangles all the required files for the vrp-suite."
  (interactive)
  (dolist (file vrp-tangle-list)
    ;; (log-to-buffer "%s" file)
    (vrp-org-babel-tangle-file-no-tests file)))

(defun vrp-tangle-all-files-with-tests ()
  "Tangles all the required files for the vrp-suite."
  (interactive)
  (dolist (file vrp-tangle-list)
    (org-babel-tangle-file file)))

(defun vrp-switch-to-journal-other-frame ()
  "Pops up the journal on another frame."
  (interactive)
  (switch-to-buffer-other-frame "vrp-journal.org"))

(defun vrp-switch-to-repl-other-window ()
  "Goes to the next window and pops the REPL."
  (interactive)
  (switch-to-buffer-other-window "*slime-repl sbcl*"))

;; (define-key org-mode-map (kbd "M-m M-v") (make-sparse-keymap))
;; (define-key org-mode-map (kbd "M-m M-v j") 'vrp-switch-to-journal-other-frame)
;; (define-key org-mode-map (kbd "M-m M-v M-j") 'vrp-switch-to-journal-other-frame)
;; ;; keys to change to the repl in the other window
;; (define-key org-mode-map (kbd "M-m M-v r") 'vrp-switch-to-repl-other-window)
;; (define-key org-mode-map (kbd "M-m M-v M-r") 'vrp-switch-to-repl-other-window)
;; (define-key lisp-mode-map (kbd "M-m M-o r") 'vrp-switch-to-repl-other-window)
;; (define-key lisp-mode-map (kbd "M-m M-o M-r") 'vrp-switch-to-repl-other-window)
;; ;; key to insert (load everything.lisp) in the repl
;; (define-key lisp-mode-map (kbd "M-m M-o M-l") (lambda () (interactive) (insert "(load \"src/vrp-load-files.lisp\") (in-package :vrp) (load \"src/neigh-load-files.lisp\")")))
