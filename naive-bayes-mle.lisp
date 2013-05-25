(ql:quickload 'stem)

(defpackage :naive-bayes-mle
  (:use :CL :stem)
  (:export :train
	   :classify))

(defparameter *categories* (make-hash-table :test 'equal))
(defparameter *vocabulary* (make-hash-table :test 'equal))

(defstruct cat
  "CAT (category) struct contains word-count vector, total number of words, and total word-count."
  (word-counts nil :type hash-table)
  (total-words 0)
  (counts 0))

(defun create-category ()
  "Return a CAT with initialized hashtable."
  (make-cat :word-counts (make-hash-table :test 'equal)))

(defun word-prob (cat word)
  "Return prior probability of WORD according to category CAT."
  (if (gethash word (cat-word-counts cat))
      (float (/ (gethash word (cat-word-counts cat)) (cat-total-words cat)))
      0))

(defun cat-prob (cat)
  "Return prior probability of category CAT."
  (let ((cat-sum (loop for c being the hash-values of *categories* sum (cat-counts c))))
    (float (/ (cat-counts cat) cat-sum))))

(defun incr-word (cat word)
  "Update counts in category CAT for WORD."
  (if (gethash word (cat-word-counts cat))
      (incf (gethash word (cat-word-counts cat)))
      (setf (gethash word (cat-word-counts cat)) 1))
  (incf (cat-total-words cat)))
  (incf (cat-counts cat))

(defun add-to-vocab (word)
  "Add the word to vocabulary if not already in it."
  (if (not (gethash word *vocabulary*))
      (setf (gethash word *vocabulary*) t)))

(defun likelihood (cat words)
  "Return the probability that category CAT produced these words."
  (reduce #'* (map 'list #'(lambda (word)
			     (if (gethash word *vocabulary*)
				 (word-prob cat word)
				 1)) words)
	  :initial-value (cat-prob cat)))    

(defun train (cat-name words)
  "Update vocabulary and category counts."
  (if (not (gethash cat-name *categories*))
      (setf (gethash cat-name *categories*) (create-category)))
  (loop for word across words with cat = (gethash cat-name *categories*) do
       (incr-word cat word)
       (add-to-vocab word)))

(defun classify (words)
  "Return the name of the category with the greatest likelihood."
  (let ((max 0) (win nil))
    (loop for cat-name being the hash-keys of *categories*
	 for cat being the hash-values of *categories* do
	 (let ((prob (likelihood cat words)))
	   (when (> prob max)
	     (setf max prob)
	     (setf win cat-name))))
    win))
