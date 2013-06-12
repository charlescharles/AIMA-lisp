(defparameter *n* 4)
(defparameter *size* (1- (* *n* *n*)))
(defparameter *goal* (let ((start (nreverse (cons 0 (loop for i from *size* downto 1 collect i)))))
		       (loop for i from 0 to *size* by *n* collect (subseq start i (+ i *n*)))))

(defun int/ (arg1 arg2)
  (truncate (/ arg1 arg2)))

(defun flatten (arr)
  "Return the board configuration as a list in row-major order ('flattened' by a level)."
  (cond
    ((null arr) ())
    ((atom arr) `(,arr))
    (t (mapcan #'flatten arr))))

(defun argmin (seq &key key)
  "Return the element of SEQ with the minimal value according to KEY"
  (let ((min (funcall key (first seq)))
	(res (first seq)))
    (loop for x in (subseq seq 1) do
	 (when (< (funcall key x) min)
	   (setf min (funcall key x)
		 res x)))
    res))

(defun manhattan-distance (config1 config2)
  "Return the Manhattan distance between two configurations"
  (loop for i from 0 to (1- *n*)  sum
       (loop for x in (nth i config1) for j from 0 to (1- *n*) sum
	    (+ (abs (- i (position-if #'(lambda (row) (member x row)) config2)))
	       (abs (- j (position x (find-if #'(lambda (row) (member x row)) config2))))))))

(defun in-bounds (x)
  "Return T if number is in the allowed range of column indices, else NIL"
  (and (>= x 0) (< x *n*)))

(defmacro xor (a b)
  "Return T iff one of the two arguments is true"
  `(and (or ,a ,b) (not (and ,a ,b))))

(defun divmod (x y)
  "Return two values: the integer division and modulo of the two arguments."
  (values (int/ x y) (mod x y)))

(defun neighbors (config)
  "Return a list of the board configurations accessible in one move."
  (multiple-value-bind (row col) (divmod (position 0 (flatten config)) *n*)
    (let ((res nil))
      (loop for i from (1- row) to (1+ row) do
	   (loop for j from (1- col) to (1+ col) do
		(if (and (in-bounds i) (in-bounds j)
			 (xor (= i row) (= j col)))
		    (let ((c (copy-tree config)))
		      (rotatef (nth j (nth i c))
			       (nth col (nth row c)))
		      (push c res)))))
      res)))

(defun random-config-gen ()
  "Return a randomly-generated board configuration."
  (let ((source (apply #'vector (loop for i from 0 to *size* collect i)))
	(config (apply #'vector (loop for i from 0 to *size* collect i))))
    (loop for i from 1 to *size* do
	 (let ((j (random (1+ i))))
	   (setf (svref config i) (svref config j)
		 (svref config j) (svref source i))))
    (loop for i from 0 to *size* by *n* collect (coerce (subseq config i (+ i *n*)) 'list))))

(defun random-config ()
  "Return a randomly-generated solvable board configuration."
 (do ((config (random-config-gen) (random-config-gen)))
     ((solvable-p config) config)))

(defun goal-p (config)
  "Return T if finished solving; NIL otherwise."
  (equal config *goal*))

(defun best-of (configs)
  "Return the configuration which is closest to being solved, measured by Manhattan distance."
  (if (null configs) nil
      (argmin configs :key #'distance-to-goal)))

(defun distance-to-goal (config)
  "Return Manhattan distance to goal configuration."
  (manhattan-distance config *goal*))

(defun hill-climb (start &optional (max-steps 100))
  "Perform a single hill-climb from a starting configuration."
  (do ((curr start (best-of (neighbors curr)))
       (i 0 (1+ i)))
      ((or (> i max-steps) (goal-p curr) (null curr))
       (values curr (distance-to-goal curr)))))

(defun inversions (config)
  "Return the number of inversions in the board configuration."
  ;; Currently N^4 in time; can improve to be N^2logN
  (let* ((seq (remove-if #'zerop (flatten config)))
	(end (1- (length seq))))
    (loop for x in seq for i from 0 to end sum
	 (loop for y in seq for j from 0 to end
	      count (and (< x y) (> i j))))))

(defun solvable-p (config)
  "Return T if board configuration is solvable; NIL otherwise."
  (if (oddp *n*)
      (evenp (inversions config))
      (oddp (+ (inversions config) (position-if #'(lambda (row) (member 0 row)) config)))))

(defun hash-config (config)
  "Return a hash function of the board configuration. May produce conflicts."
   (reduce #'+ (mapcar #'(lambda (x i) (expt x i))
		       (flatten config)
		       (loop for i from 0 to *size* collect i))))

(defstruct state
  "Contains a board configuration and its hash, pointer to prev state, and costs."
  (config nil)
  (prev-state nil)
  (g-cost nil)
  (h-cost nil)
  (f-cost nil)
  (hash nil))

(defun make-new-state (config prev-state g-cost)
  "Return a new state with a board configuration, previous config, and cost."
  (make-state :config config
	      :prev-state prev-state
	      :g-cost g-cost
	      :h-cost (distance-to-goal config)
	      :f-cost (+ g-cost (distance-to-goal config))
	      :hash (hash-config config)))

(defun a-star-solve (start &optional (max-steps 500))
  "Solve an 8-puzzle using A* search."
  (if (not (solvable-p start)) (return-from a-star-solve nil))
  (let ((curr (make-new-state start nil 0))
	(q (make-pq :key #'state-f-cost))
	(open (make-hash-table))
	(steps 0))
    (loop until (or (goal-p (state-config curr))
		    (> steps max-steps))
	 do
	 (mapc #'(lambda (config)
		   (if (not (gethash (hash-config config) open))
		       (enqueue q (make-new-state config curr steps))))
	       (neighbors (state-config curr)))
	 (incf steps)
	 (remhash (state-hash curr) open)
	 (setf curr (dequeue q))
	 (format t "state:~a~%g:~a; h:~a; f:~a; hash:~a; steps:~a~%--------------------------~%"
		 (state-config curr)
		 (state-g-cost curr)
		 (state-h-cost curr)
		 (state-f-cost curr)
		 (state-hash curr)
		 steps))
    (list (state-config curr) steps)))

;;;; Priority queue implementation from AIMA Lisp utils
;;;; Mostly copied from http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp
(defstruct pq
  (key #'identity)
  (elements nil))

(defun empty-pq? (pq)
  "Are there no elements in the queue?"
  (zerop (length (pq-elements pq))))

(defun enqueue (pq items)
  "Insert item into priority queue."
  (if (null (pq-elements pq)) (setf (pq-elements pq) (make-heap)))
  (if (atom items)
      (heap-insert (pq-elements pq) items (pq-key pq))
      (loop for item in items do
	   (heap-insert (pq-elements pq) item (pq-key pq)))))

(defun dequeue (pq)
  "Remove and return the minimum element."
  (heap-extract-min (pq-elements pq) (pq-key pq)))

(defun heap-val (heap i key)
  (declare (fixnum i))
  (funcall key (aref heap i)))

(defun heap-parent (i)
  (declare (fixnum i))
  (floor (- i 1) 2))

(defun heap-left (i)
  (declare (fixnum i))
  (the fixnum (+ 1 i i)))

(defun heap-right (i)
  (declare (fixnum i))
  (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  "Assume the children of i are heaps, but heap[i] may be larger than its children. If it is, move heap[i] down where it belongs."
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (1- (length heap)))
	smallest)
    (setf smallest (if (and (<= l N)
			    (<= (heap-val heap l key)
				(heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest-valued) key off the heap."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Put an item into a heap."
  ;; ITEM is the value to be inserted; KEY is a function that extracts a numerical comparison value from the item.
  (vector-push-extend nil heap)
  (let ((i (1- (length heap)))
	(val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val)) do
	 (setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))
