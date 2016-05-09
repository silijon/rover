;Rover package contains all definitions and functions required to drive a rover
;TODO: create a package from this
(defvar *compass* #(#\N #\E #\S #\W))

; Rover Definition
(defclass rover ()
  ((x   :accessor x 
        :initarg :x)
   (y   :accessor y
        :initarg :y)
   (bearing :accessor bearing
            :initarg :bearing)))

(defun get-rover-property (rover name)
  "Generic rover property getter"
  (slot-value rover name))

(defun set-rover-property (rover name value)
  "Generic rover property setter"
  (setf (slot-value rover name) value))

(defun create-rover (x y bearing)
  "Rover constructor"
  (make-instance 'rover :x x :y y :bearing bearing))

(defun rotate-rover (rover ticks)
  "Rotate the rover [ticks] number of compass bearings from its current bearing"
   (set-rover-property rover 'bearing
      (elt *compass* (mod (+ (position (get-rover-property rover 'bearing) *compass*) ticks) 4))))

(defun move-rover (rover ticks)     
  "Move the rover [ticks] number of positions from its current position along its current bearing"
  (let* ((bearing-index (position (get-rover-property rover 'bearing) *compass*))
         (axis (if (oddp bearing-index) 'x 'y)))
    (set-rover-property rover axis
        (+ (get-rover-property rover axis) (if (< bearing-index 2) ticks (- ticks))))))

(defun drive-rover (rover instructions)
  "Drive the rover with standard sequence of instructional characters: e.g. 'MLMMRMLMMMR'"
  (map 'list 
       #'(lambda (i) (cond ((char= i #\M) (move-rover rover 1))
                           ((char= i #\L) (rotate-rover rover -1))
                           ((char= i #\R) (rotate-rover rover 1)))) 
       instructions))

(defun print-rover (rover)
  "Print the current state of the rover as space-delimited tuple of '[X-POSITION] [Y-POSITION] [BEARING]'"
  (format t "~D ~D ~C~%" 
          (get-rover-property rover 'x)
          (get-rover-property rover 'y)
          (get-rover-property rover 'bearing)))

