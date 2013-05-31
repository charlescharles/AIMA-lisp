(defparameter *T0* 30)
(defparameter *b* 30)
(defparameter *p* 35)
(defparameter *n* 10)


(defun set-initial-temp (temp)
  (setf *T0* temp))


(defun set-b (b)
  (setf *b* b))


(defun set-p (p)
  (setf *p* p))


(defparameter distances (make-array '(10 10)
			:element-type 'float
			:adjustable t
			:initial-contents
			'((.0000 .3361 .3141 .3601 .5111 .5176 .2982 .4564 .3289 .2842)
			  (-1.00 .0000 .1107 .6149 .8407 .8083 .5815 .6418 .4378 .3934)
			  (-1.00 -1.00 .0000 .5349 .7919 .8207 .5941 .6908 .4982 .4501)
			  (-1.00 -1.00 -1.00 .0000 .3397 .6528 .5171 .7375 .6710 .6323)
			  (-1.00 -1.00 -1.00 -1.00 .0000 .4579 .4529 .6686 .7042 .6857)
			  (-1.00 -1.00 -1.00 -1.00 -1.00 .0000 .2274 .2937 .4494 .4654)
			  (-1.00 -1.00 -1.00 -1.00 -1.00 -1.00 .0000 .2277 .2690 .2674)
			  (-1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 .0000 .2100 .2492)
			  (-1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 .0000 .0498)
			  (-1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 -1.00 .0000))))


(defvarnodes (make-array (list *n* *n*)
        :element-type 'integer
        :adjustable t
        :initial-element 0))


(defun init-distances (distances)
  "Make DISTANCES symmetric."
  (setf *n* (array-dimension distances 0))
  (loop for i from 0 to (1- *n*) do
       (loop for j from 0 to (1- i) do
	    (setf (aref distances i j)
		  (aref distances j i)))))


(defun temperature (k)
  "Return temperature at k-th epoch."
  (/ *T0* (log (1+ k))))


(defun init-nodes ()
  "Set nodes to random initial activations."
  (loop for i from 0 to (1- *n*) do
       (loop for j from 0 to (1- *n*) do
	    (setf (aref nodes i j) (random 2)))))


(defun weight (x1 y1 x2 y2)
  "Return weight between two nodes, (x1, y1) and (x2, y2)."
  (cond
    ((and (= x1 x2) (= y1 y2)) *b*)      ; same node
    ((or (= x1 x2) (= y1 y2)) (- *p*))   ; same row or same col
    ((or (= (mod (1- y1) *n*) y2)        ; adjacent cols
	 (= (mod (1+ y1) *n*) y2))
     (aref distances x1 x2))
    (t 0)))                              ; too far away


(defun distance (a b)
  "Return distance between cities at indices a and b."
  (aref distances a b))


(defun neighbors (x y)
  "Return neighbors (itself, same row, same column, adjacent columns) of node."
  (let ((res nil))
    (mapc #'(lambda (i)
	      (push (list (mod (1- x) *n*) i) res)
	      (push (list (mod (1+ x) *n*) i) res)
	      (push (list x i) res)
	      (if (/= i x)
		  (push (list i y) res)))
	  (loop for i from 0 to (1- *n*) collect i))
    (remove-duplicates res :test #'equal)))
		  

(defun activation (x y)
  "Return the activation of node (x, y)."
  (aref nodes x y))


(defun flipped (a)
  "Return the opposite activation (0->1, 1->0)."
  (- 1 a))


(defun flip (x y)
  "Flip node (x, y)."
  (setf (aref nodes x y) (flipped (activation x y))))


(defun energy (x y a)
  "Return energy contributed by node (x, y) with activation A."
  (loop for n in (neighbors x y) sum
       (destructuring-bind (i j) n
	 (* a (activation i j)
	    (weight x y i j)))))


(defun energy-delta (x y)
  "Change in energy from flipping node (x, y)"
  (let ((a (activation x y)))
    (- (energy x y (flipped a)) (energy x y a))))


(defun flip-prob (x y temp)
  "Return the probability for node (x, y) to flip."
  (/ (1+ (exp (/ (- (energy-delta x y)) temp)))))


(defun should-flip (x y temp)
  "Return T if node (x, y) flips at temperature TEMP; NIL otherwise."
  (< (random 1.0) (flip-prob x y temp)))


(defun run-epoch (k)
  "Run the k-th epoch."
  (let ((temp (temperature k)))
    (dotimes (i *n*)
      (dotimes (j *n*)
	(if (should-flip i j temp) (flip i j))))))


(defun run-multiple (epochs)
  (loop for k from 1 to epochs do
       (run-epoch k)))


(defun row-pos (elt col arr)
  "Return the row position of ELT in column COL in ARR; NIL if not there."
  (dotimes (i (array-dimension arr 0))
    (if (equal elt (aref arr i col))
	(return-from row-pos i))))


(defun compute-distance ()
  "Return the distance found for a circuit; NIL if circuit not completed."
  (flet ((rpos (x)
	   (let ((rp (row-pos 1 x nodes)))
	     (if rp rp (return-from compute-distance nil)))))
    (let ((dist 0))
      (do* ((i 1 (1+ i))
	    (prev (rpos 0) curr)
	    (curr (rpos 1) (rpos (mod i *n*))))
	   ((> i *n*))
	(incf dist (distance prev curr)))
      dist)))


(defun solve (&optional (epochs 20))
  "Initialize nodes, run 20 epochs, and print resulting activations and distance."
  (init-nodes)
  (run-multiple epochs)
  (format t "nodes:~%~A~%" nodes)
  (format t "distance:~A~%" (compute-distance)))