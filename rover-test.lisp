(load "rover.lisp")

; unit-test stuff
(defvar *test-name* nil)

(defmacro with-gensyms-1 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms-1 (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
                
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))


; rover stuff
; test helpers
(defun rotate-and-check (rover ticks expected)
  (progn 
    (rotate-rover rover ticks)
    (char= expected (get-rover-property rover 'bearing))))

(defun move-and-check (rover ticks expected-x expected-y)
  (progn
    (move-rover rover ticks)
    (and (= (get-rover-property rover 'x) expected-x)
         (= (get-rover-property rover 'y) expected-y))))

; tests
(deftest test-create-rover ()
  (let ((rover (create-rover 3 2 #\W)))
    (check
      (= (get-rover-property rover 'x) 3)
      (= (get-rover-property rover 'y) 2)
      (char= (get-rover-property rover 'bearing) #\W))))

(deftest test-rotate-rover ()
  (let ((rover (create-rover 0 0 #\N)))
    (check
      (rotate-and-check rover 1 #\E)
      (rotate-and-check rover 1 #\S)
      (rotate-and-check rover 1 #\W)
      (rotate-and-check rover 4 #\W)
      (rotate-and-check rover -3 #\N)
      (rotate-and-check rover -1 #\W))))

(deftest test-move-rover ()
  (let ((rover (create-rover 0 0 #\N)))
    (check
      (move-and-check rover 1 0 1)
      (move-and-check rover 3 0 4)
      (move-and-check rover -5 0 -1)
      (move-and-check rover 1 0 0)
      (rotate-and-check rover 1 #\E)
      (move-and-check rover 1 1 0)
      (move-and-check rover 3 4 0)
      (move-and-check rover -5 -1 0)
      (move-and-check rover 1 0 0)
      (rotate-and-check rover 1 #\S)
      (move-and-check rover 1 0 -1)
      (move-and-check rover 3 0 -4)
      (move-and-check rover -5 0 1)
      (move-and-check rover 1 0 0)
      (rotate-and-check rover 1 #\W)
      (move-and-check rover 1 -1 0)
      (move-and-check rover 3 -4 0)
      (move-and-check rover -5 1 0)
      (move-and-check rover 1 0 0))))

(deftest test-drive-rover ()
  (let ((rover1 (create-rover 1 2 #\N))
        (rover2 (create-rover 3 3 #\E)))
    (check
      (drive-rover rover1 "LMLMLMLMM")
      (= (get-rover-property rover1 'x) 1)
      (= (get-rover-property rover1 'y) 3)
      (char= (get-rover-property rover1 'bearing) #\N)
      (drive-rover rover2 "MMRMMRMRRM")
      (= (get-rover-property rover2 'x) 5)
      (= (get-rover-property rover2 'y) 1)
      (char= (get-rover-property rover2 'bearing) #\E))))

(deftest test-rover-all ()
  (combine-results
    (test-create-rover)
    (test-rotate-rover)
    (test-move-rover)
    (test-drive-rover)))
   







