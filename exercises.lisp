(defun hello(*name*)
    (format t "Hello ~a!" *name*)
)


(defun getSchool (age)
    (case age
        (5 (print "Young"))
        (10 (print "Not much young"))
        (15 (print "Teenager"))
        (otherwise "Old")
    )
    (when (= age 15)
        (print "Have a good day")
    )
    (terpri)
)


(defun myCondFunction (x)
    (cond ((< x 18)
            (print "Go to School")
            (print "Have a nice time")
        )
        (
            (>= x 18)
            (print "Get a job!")
            (print "You are old.")
        )
    )

)

(defun myLoops()

    (loop for x from 1 to 10
        do
            (print x)
            (print (* 2 x))
        
    )
    (loop for x in '(Ozan Argit Onceken)
        do
            (print x)
        
    )
    (dotimes (y 12)
    (print y))
)

(defvar *total* 0) ;;;;Global variable
(defun sum (&rest nums)
    (dolist (num nums)
    (setf *total* (+ num *total*) )
    )
    (format t "Sum= ~a~%" *total*)
)

(defun print-list (&optional &key x y z)
    (format t "List ~a~%" (list x y z))
)

(defun difference (num1 num2)
    (return-from difference (- num1 num2))
)

(defun squares(num)
    (
        values (expt num 2) (expt num 3)
    )
)
(defmacro ifit (condition &rest body)
    (if condition (progn @body)
        (format t "Can't Drive~%")
    )
)






(print(/ 34 2))
#|
 this is a comment line
|#
(format t "Hello World")
(format t "Hello World~%")
(format t "Hello World~%")
#|

(format t "What is your name?~%")
(defvar *name* (read))
(setq *print-case* :capitalize)
(hello *name*)

|#

;;;;~a shows value
;;;;~s shows quotes around the value
(format t "(/ 5 4) =~d~%" (/ 5 4))
(format t "(/ 5 4.0) =~d~%" (/ 5 4.0))


(defparameter name 'Ozan)
(format t "(eq name 'Ozan)=~d~%" (eq name 'Ozan))
(format t "(eq name 'SomethingElse)=~d~%" (eq name 'SomethingElse))
(format t "(equal \"string\" \"string\")=~d~%" (equal "string" "String"))
(format t "(eq \"string\" \"string\")=~d~%" (eq "string" "String"))
(format t "(equal \"string\" \"string\")=~d~%" (equal "String" "String"))
(format t "(eq \"string\" \"string\")=~d~%" (eq "String" "String"))
(format t "(equalp 1.0 1)=~d~%" (equalp 1.0 1))
(format t "(equalp \"Ozan\" \"ozan\")=~d~%" (equalp "Ozan" "ozan"))
(defvar age 22)
(if (<= age 18)
    (format t "You can't vote~%")
    (format t "You can vote~%")
)

(if (not (<= age 18))
    (format t "You are an adult~%")
    (format t "You are not an adult~%")
)

(if (and(>= age 18) (< age 70))
    (format t "Best times of your life~%")
    (format t "You are a good person but we should talk about sth...~%")
)
(defvar num 2)
(if (= num 2 )
    (progn
        (setq num (* num 2))
        (setq num (* num 2))
        (setq num (* num 2))
        (format t "Look line 53 for progn ,~d" num)
    )
    (format t "Nothing changed")
)
(setq age 15)
(getschool age)
(myCondFunction age)
(myloops)
(cons 'superman 'batman)
(list 'superman 'batman 'flash)
(cons 'aquaman '(superman batman))
(format t "First:~d~%" (car '(superman batman flash aquaman)))
(format t "Rest:~d~%" (cdr '(superman batman flash aquaman)))
(format t "First of rest:~d~%" (cadr '(superman batman flash aquaman)))
(format t "Second of rest:~d~%" (caddr '(superman batman flash aquaman)))
(format t "Third of rest:~d~%" (cadddr '(superman batman flash aquaman)))
(format t "Is 3 element of (2 4 6)=~d~%" (if (member 3 '(2 4 6)) t nil))
(defparameter nums '(2 4 6))
(push 1 nums)
(format t "first item is in list is:~d~%" (nth 0 nums))
(format t "first item with car in list is:~d~%" (car nums))
(format t "2nd item is in list is:~d~%" (nth 2 nums))
(format t "4th item is in list is:~d~%" (nth 4 nums))
(format t "9th item is in list is:~d~%" (nth 9 nums))
(defvar superman (list :name "Superman" :secret_id "Clark Kent"))
(defvar spiderman (list :name "Spiderman" :secret_id "Peter Parker"))
(defvar herolist nil)
(push superman herolist)
(push spiderman herolist)
(dolist(hero herolist)
    (format t "~{ ~a ~a ~}~%" hero)
)
#|

    (defparameter heroes (
        (Superman (Clark Kent))
        (Batman (Bruce Wayne))
        (Spiderman (Peter Parker))
        )
    )

|#
;;;; We have cadddr but no caddddr.


(sum 1 2 3 4 5 6 7 8)
(print-list :x 4 :z 2)
(format t "10-2=~a~%"(difference 10 2))
(format t "A number ~a~%" (mapcar #'numberp '(1 2 3 f g)))
#| 
    (flet
        (
            (double-it (num)
                (* num 2)
            )
            (triple-it (num)
                (* num 3)
            )
        )
    )

    (labels
        (
            (double-it (num)
                (* num 2)
            )
            (triple-it (num)
                (* (double-it num) 3)
            )
        )
    )
|#
(flet ((sin2x (x) (sin (* 2 x)))
       (cos2x (x) (cos (* 2 x))))
 (+ (sin2x 0.2) (cos2x 0.2)))




(multiple-value-bind 
    (a b) (squares 2)
    (format t "2^2=~d 2^3=~d~%" a b)
)

(mapcar (lambda(x) (print (* x 2))) '(2 3 4 5 6))
(setq age 19)
(quit)
(print "Here will not work.")
