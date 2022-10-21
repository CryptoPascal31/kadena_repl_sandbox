;SPDX-License-Identifier: MIT

; This modules provides some convenient math function for Pact.
;
; Be aware that this module is only in Beta and hasn't been audited:
;    --> BE CAREFUL if a security enforcement depends on one of theses functions
;
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib

(module util-math GOV
  "Module containing math utilities \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  (defconst VERSION:string "0.2")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (use util-lists [enforce-not-empty first enumerate-list remove-first])

  (defconst PI:decimal 3.141592653589793)

  (defconst E:decimal 2.718281828459045)

  (defconst GOLDEN-RATIO:decimal (/ (+ 1.0 (sqrt 5.0)) 2.0))

  (defun min:decimal (x:decimal y:decimal)
    "Returns the min of 2 values"
    (if (< x y) x y))

  (defun min3:decimal (x:decimal y:decimal z:decimal)
    "Returns the min of 3 values"
    (min x (min y z)))

  (defun min4:decimal (x:decimal y:decimal z:decimal zz:decimal)
    "Returns the min of 4 values"
    (min (min x y) (min z zz)))

  (defun min-list:decimal (x:[decimal])
    "Returns the min of a list"
    (enforce-not-empty x)
    (fold (min) (first x) (remove-first x))
  )

  (defun amin:integer (in:[decimal])
    "Returns the min index of a list"
    (enforce-not-empty in)
    (let ((in-enum (enumerate-list in))
          (cmp (lambda (x y) (if (< (at 'v x) (at 'v y)) x y))))
      (at 'i (fold (cmp) (first in-enum) (remove-first in-enum))))
  )

  (defun max:decimal (x:decimal y:decimal)
    "Returns the max of 2 values"
    (if (> x y) x y))

  (defun max3:decimal (x:decimal y:decimal z:decimal)
    "Returns the max of 3 values"
    (max x (max y z)))

  (defun max4:decimal (x:decimal y:decimal z:decimal zz:decimal)
    "Returns the max of 4 values"
    (max (max x y) (max z zz)))

  (defun max-list:decimal (x:[decimal])
    "Returns the max of a list"
    (enforce-not-empty x)
    (fold (max) (first x) (remove-first x))
  )

  (defun amax:integer (in:[decimal])
    "Returns the max index of a list"
    (enforce-not-empty in)
    (let ((in-enum (enumerate-list in))
          (cmp (lambda (x y) (if (> (at 'v x) (at 'v y)) x y))))
      (at 'i (fold (cmp) (first in-enum) (remove-first in-enum))))
  )

  (defun sum3:decimal (x:decimal y:decimal z:decimal)
    "Returns the sum of 3 values"
    (+ x (+ y z)))

  (defun sum4:decimal (x:decimal y:decimal z:decimal zz:decimal)
    "Returns the sum of 4 values"
    (+ (+ x y) (+ z zz)))

  (defun sum:decimal (x:[decimal])
    "Returns the sum of a list"
    (fold (+) 0.0 x))

  (defun prod3:decimal (x:decimal y:decimal z:decimal)
    "Returns the product of 3 values"
    (* x (* y z)))

  (defun prod4:decimal (x:decimal y:decimal z:decimal zz:decimal)
    "Returns the product of 4 values"
    (* (* x y) (* z zz)))

  (defun prod:decimal (x:[decimal])
    "Returns the product of a list"
    (fold (*) 1.0 x))

  (defun square:decimal (x:decimal)
    "Returns the square of x"
    (* x x))

  (defun safe-/ (x:decimal y:decimal default:decimal)
    "Divide x/y but returns default if y is 0.0"
    (if (= y 0.0) default (/ x y)))

  (defun avg:decimal (x:[decimal])
    "Returns the average of a list"
    (enforce-not-empty x)
    (/ (sum x) (length x)))

  (defun is-even:bool (x:integer)
    "Returns true if x is even"
    (= 0 (mod x 2)))

  (defun is-odd:bool (x:integer)
    "Returns true if x is odd"
    (= 1 (mod x 2)))

  (defun med:decimal (x:[decimal])
    "Returns the median of a list: if the length of the list is even return the (n/2 -1)th element"
    (enforce-not-empty x)
    (let* ((cnt (length x))
           (mid (/ cnt 2))
           (index (if (is-even cnt) (- mid 1) mid)))
      (at index (sort x)))
  )

  (defun med*:decimal (x:[decimal])
    "Returns the median of a list: if the length of the list is even return the average of the\
    \(n/2 -1)th and (n/2)th elements"
    (enforce-not-empty x)
    (let* ((cnt (length x))
           (mid (/ cnt 2))
           (mid-1 (- mid 1))
           (sorted-x (sort x)))
      (if (is-even cnt)
          (/ (+ (at mid sorted-x) (at mid-1 sorted-x)) 2)
          (at mid sorted-x)))
  )

  (defun between:bool (a:decimal b:decimal x:decimal)
    "Returns true is a <= x <= b"
    (and? (<= a) (>= b) x))

  (defun sign:decimal (x:decimal)
    "Returns 1.0 if x is positive, 0.0 if x is null, and -1.0 if x is negative"
    (if (= x 0.0)
        0.0
        (if (> x 0.0)
            1.0
            -1.0)))

  (defun pow10:decimal (x:integer)
    "Returns 10^x, rounded to 12 decimals (rounding is important when x is negative)"
    (round (^ 10.0 x) 12))

  (defun xEy (x:decimal y:integer)
    "Returns x.10^y, rounded to 12 decimals"
    (round (* x (pow10 y)) 12))

  ;;; Log functions
  (defun log10:decimal (x)
    "Returns the log of x base 10, rounded to 12 decimals"
    ; x can be decimal or an integer but the returned result is always a decimal
    (round (log 10 (* 1.0 x)) 12))

  (defun safe-log (x y default)
    "Log of Y base X, but returns default when y <= 0"
    (if (> (* 1.0 y) 0.0 ) (log x y) default))

  (defun safe-ln (x:decimal default:decimal)
    "Natural log of x, but returns default when x <= 0"
    (if (> x 0.0 ) (ln x) default))

  (defun safe-log10:decimal (x default:decimal)
    "Returns the log of x base 10, rounded to 12 decimals but returns default when x <= 0"
    ; x can be decimal or an integer but the returned result is always a decimal
    ; btw default has to be a decimal
    (if (> (* 1.0 x) 0.0 ) (log10 x) default))

)
