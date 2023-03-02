(in-package :vrp)

(defun mean-technique (neighborhood result-table region-id-list file-name)
  ;; the next line of code is added in order to avoid a warning
  ;; message since file-name parameter is not used in the function
  (declare (ignore file-name))
  ;; (format t "Mean technique~%")
  (mapcar #'(lambda (tuple) (cdr tuple))
	  (sort
	   (loop for id in region-id-list
	      collect
		(let* ((key (nth (1- id) (region-keys neighborhood)))
		       (lst (gethash key result-table)))
		  (cons (/ (apply #'+ lst) (length lst))			  
			id)))
	   #'< :key #'car)))
