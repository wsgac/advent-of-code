(in-package #:advent-of-code-2019.day02)

(defun parse-input (input)
  (coerce (util:parse-integers input :sep '(#\,)) 'vector))

(define-condition incorrect-memory-address (error)
  ((address :initarg :address)))

(define-condition unknown-op (error)
  ((op :initarg :op)))

(defun %read-cell (memory position)
  (handler-case (aref memory position)
    (sb-int:invalid-array-index-error ()
        (error 'incorrect-memory-address :address position))))

(defun %exec-add (memory position)
  (let ((src1 (%read-cell memory (+ position 1)))
        (src2 (%read-cell memory (+ position 2)))
        (dst (%read-cell memory (+ position 3))))
    (setf (aref memory dst)
          (+ (%read-cell memory src1)
             (%read-cell memory src2)))))

(defun %exec-multiply (memory position)
  (let ((src1 (%read-cell memory (+ position 1)))
        (src2 (%read-cell memory (+ position 2)))
        (dst (%read-cell memory (+ position 3))))
    (setf (aref memory dst)
          (* (%read-cell memory src1)
             (%read-cell memory src2)))))

(defun step-program (memory position)
  (let ((op (%read-cell memory position)))
    (case op
      (1
       (%exec-add memory position)
       (values memory (+ position 4)))
      (2
       (%exec-multiply memory position)
       (values memory (+ position 4)))
      (99
       (values memory nil))
      (t (error 'unknown-op :op op)))))

(defun problem-1 (&key (input *input-test*))
  (loop
    :with memory := (parse-input input)
    :with pos := 0
      :initially (progn (setf (aref memory 1) 12)
                        (setf (aref memory 2) 2))
    :do (multiple-value-setq (memory pos) (step-program memory pos))
    :while pos
    :finally (return (aref memory 0))))

(defun problem-2 (&key (input *input-test*))
  nil)

(defparameter *input-test*
  "1,9,10,3,2,3,11,0,99,30,40,50")

(defparameter *input*
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,13,23,1,23,10,27,1,13,27,31,2,31,10,35,1,35,9,39,1,39,13,43,1,13,43,47,1,47,13,51,1,13,51,55,1,5,55,59,2,10,59,63,1,9,63,67,1,6,67,71,2,71,13,75,2,75,13,79,1,79,9,83,2,83,10,87,1,9,87,91,1,6,91,95,1,95,10,99,1,99,13,103,1,13,103,107,2,13,107,111,1,111,9,115,2,115,10,119,1,119,5,123,1,123,2,127,1,127,5,0,99,2,14,0,0")
