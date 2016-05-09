; Parses rover navigation input from 'input.txt' and prints out final rover locations
(load "rover.lisp")

; helper functions to parse space-delimited strings
(defun split (string delimiterp)
  (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))

(defun is-space (c) (char= c #\Space))

; parse input from 'input.txt' and drive the rover
; TODO: validate rover stays in the field
(let* ((in (open "input.txt" :if-does-not-exist :error))
       (field (split (read-line in) #'is-space)))
  (when in
    (loop for line = (read-line in nil)
          while line do 
          (let* ((def (split line #'is-space))
                 (instructions (read-line in))
                 (rover (create-rover (parse-integer (elt def 0)) 
                                      (parse-integer (elt def 1))
                                      (elt (elt def 2) 0))))
            (progn
                (drive-rover rover instructions)
                (print-rover rover))))
    (close in)))
