(ql:quickload 'stem)

(defpackage :naive-bayes-mle
  (:use :CL :stem)
  (:export :main))

(defparameter *categories* (make-hash-table :test 'equal))
(defparameter *vocabulary* (make-hash-table :test 'equal))

(defstruct cat
  (word-counts nil :type hash-table)
  (total-words 0)
  (counts 0))

(defun create-category ()
  (make-cat :word-counts (make-hash-table :test 'equal)))

(defun word-prob (cat word)
  (if (gethash word (cat-word-counts cat))
      (float (/ (gethash word (cat-word-counts cat)) (cat-total-words cat)))
      0))

(defun cat-prob (cat)
  (let ((cat-sum (loop for c being the hash-values of *categories* sum (cat-counts c))))
    (float (/ (cat-counts cat) cat-sum))))

(defun incr-word (cat word)
  (if (gethash word (cat-word-counts cat))
      (incf (gethash word (cat-word-counts cat)))
      (setf (gethash word (cat-word-counts cat)) 1))
  (if (not (gethash word *vocabulary*))
      (setf (gethash word *vocabulary*) t))
  (incf (cat-total-words cat)))

(defun likelihood (cat words)
  (reduce #'* (map 'list #'(lambda (word)
			     (if (gethash word *vocabulary*)
				 (word-prob cat word)
				 1)) words)
	  :initial-value (cat-prob cat)))    

(defun train (cat-name words)
  (if (not (gethash cat-name *categories*))
      (setf (gethash cat-name *categories*) (create-category)))
  (loop for word across words with cat = (gethash cat-name *categories*) do
       (incr-word cat word)
       (incf (cat-counts cat))))

(defun classify (words)
  (let ((max 0) (win nil))
    (loop for cat-name being the hash-keys of *categories*
	 for cat being the hash-values of *categories* do
	 (let ((prob (likelihood cat words)))
	   (when (> prob max)
	     (setf max prob)
	     (setf win cat-name))))
    win))

(defun main ())
  