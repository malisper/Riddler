(defun single (xs)
  "Does this list contain only a single element?"
  (and xs (null (cdr xs))))

(defun know (beliefs)
  "Based on the possible numbers a person thinks they can have, do
   they know their number? Meaning is there only a single number they
   think they can have."
  (single beliefs))

(defun a1 (_ b c)
  "In the world with numbers (_,b,c) return the numbers that person a
   thinks they can have."
  (declare (ignore _))
  (remove-duplicates
    (remove-if-not #'plusp
      (list (+ b c) (abs (- b c))))))

(defun b (a _ c)
  "In the world with numbers (a,_,c) return the belief states of person
   b when they are asked."
  (declare (ignore _))
  (remove-if
    (lambda (b)
      (knows (a1 a b c)))
    (remove-duplicates
      (remove-if-not
        #'plusp
         (list (+ a c) (abs (- a c)))))))

(defun c (a b _)
  "In the world with numbers (a,b,_) return the belief states of
   person c when they are asked."
  (declare (ignore _))
  (remove-if
    (lambda (c)
      (knows (b a b c)))
    (remove-duplicates
      (remove-if-not
        #'plusp
        (list (+ a b) (abs (- a b)))))))

(defun a2 (_ b c)
  "In the world with numbers (_,b,c) return the belief states of
   person a when they are asked the second time around."
  (declare (ignore _))
  (remove-if
    (lambda (a)
      (knows (c a b c)))
    (remove-duplicates
      (remove-if-not #'plusp
        (list (+ b c) (abs (- b c)))))))

(defun solve ()
  "Solves the problem and returns the values for a, b, and c."
  (loop with a = 65
        for b from 1
        do (dolist (c (remove 0 (list (+ a b)
                                      (abs (- a b)))))
             (when (and (not (know (a1 a b c)))
                        (not (know (b  a b c)))
                        (not (know (c  a b c)))
                        (know (a2 a b c)))
               (return-from solve (values a b c))))))

; (solve) => 65 26 39
