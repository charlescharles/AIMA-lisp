(defun threat (i j x y)
  (and (not (= i j))
       (or (= x y)
	   (= y (+ x (- i j)))
	   (= y (- x (- i j))))))

(defun conflicts (config)
  (let ((end (- (length config) 1)) (acc 0))
    (loop for x in config for i from 0 to end do
	 (let ((threats (loop for y in config for j from 0 to end count
			     (threat i j x y))))
	   (if (> threats 0)
	       (incf acc threats))))
    acc))

(defun rand-config (n)
  (let ((start (apply #'vector (loop for i from 0 to (- n 1) collect i))))
    (loop for i from (- n 1) downto 1 do
	 (rotatef (svref start i) (svref start (random (1+ i)))))
    (coerce start 'list)))

(defun neighbors (config)
  (let ((res nil) (end (- (length config) 1)))
    (loop for el in config for i from 0 to end do
	 (if (> el 0) (setf res (cons
				 (append (subseq config 0 i) (list (- el 1)) (subseq config (1+ i))) res)))
	 (if (< el end) (setf res (cons
				   (append (subseq config 0 i) (list (+ el 1)) (subseq config (1+ i))) res))))
    res))

(defun goal-p (config)
  (zerop (conflicts config)))

(defun best (configs)
  ;; takes a list of configurations.
  ;; returns two values: the list with the least conflicts, and the number of conflicts it has.
  (let ((opt (reduce #'(lambda (a b)
			 (if (< (conflicts a) (conflicts b)) a b))
		     configs)))		       
    (values opt (conflicts opt))))

(defun best-next (config)
  (best (cons config (neighbors config))))

(defun hill-climb (n &optional (max-steps 100))
  (do ((curr (rand-config n) (best-next curr))
       (prev-best -1 (conflicts curr))
       (i 0 (1+ i)))
      ((or (goal-p curr)
	   (> i max-steps)
	   (if (> prev-best 0)
	       (< prev-best (conflicts curr))))
       (values curr (conflicts curr)))))

(defun show (config)
  (let ((n (- (length config) 1)))
    (loop for i from 0 to n do
	 (loop for j from 0 to n do
	      (if (zerop (nth j config))
		  (format t "Q ")
		  (format t "x "))
	      (decf (nth j config)))
	 (format t "~%"))))

(defun solve (n &optional (max-steps 100))
  (let ((result (do ((curr (hill-climb n) (hill-climb n))
		     (i 0 (1+ i)))
		    ((or (goal-p curr) (> i max-steps)) curr))))
    (show (copy-list result))
    result))