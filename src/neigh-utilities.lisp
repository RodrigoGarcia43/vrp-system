(in-package :vrp)

(defun slice (lista start end)
  (loop for i from start to end
     collecting (nth (1- i) lista)))

(defun comp-less-lsts (lst1 lst2)
  (if (or (zerop (length lst1))
          (zerop (length lst2)))
      ;; if branch
      (if (and (zerop (length lst1))
               (zerop (length lst2)))
          nil
          (if (zerop (length lst1))
              t
              nil))
      ;; else branch
      (let ((x (first lst1))
            (y (first lst2)))
        (if (< x y)
            t
            (if (< y x)
                nil
                (comp-less-lsts (rest lst1)
                                (rest lst2)))))))

(defun finder (obj list start end fn)
  (let* ((range (- end start)))
    (if (zerop range)
        start
        (let ((mid (+ start (truncate (/ range 2)))))
          (let ((obj2 (nth mid list)))
            (if (funcall fn obj obj2)
                (finder obj list start (1- mid) fn)
                (let ((obj3 (nth (1+ mid) list)))
                  (if (not (funcall fn obj obj3))
                      (finder obj list (1+ mid) end fn)
                      mid))))))))

(defun binary-search (obj list &key (fn #'<))
  (finder obj list 0 (1- (length list)) fn))
