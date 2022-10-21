;SPDX-License-Identifier: MIT

; This module provides random generation functions.
;
; The random integers Xn are generated according to the following formula:
; Xn = HASH(Xn-1 + TxHash + HASH(block-time))
; Xn is stored in database for the next iteration
;
; The random string is derived from Xn with the following formula
; S = HASH( [Xn + 1] ) + HASH ( [Xn + 2] ) + HASH ( [Xn + 3] ) + ..........
;
; The result is hard (maybe impossible) to predict before mining since block-time includes microseconds.
;
; Several numbers can be generated in the same block or transaction. They will be all different, unpredictable and uncorrelated
;
; Be careful, the miner can control the generated numbers => Don't use for high stakes lottery or cryptography
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib

(module util-random GOV
  "This module provides pseudo-random numbers/string generation \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "
  (defconst VERSION:string "0.2")

  (bless "RBfxKPExaz5q6i64FLA_k7UVM9MaOO0UDJulfPFZBRA")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (use util-strings [join])
  (use util-math [pow10])

  (defschema state-schema
    state:string)

  (deftable state-table:{state-schema})

  (defun reset-state:string()
    "Reset the state of the generator. Should only be called when the contract is generated"
    (with-default-read state-table "" {'state:""} {"state":=s}
      (enforce (= s "") "Random generator already initialized" ))

    (write state-table ""{'state:(tx-hash)})
    (+ "Random Generator state resetted to " (tx-hash))
  )

  (defun --random-hash:string ()
    "Core private function which returns the 256 bits random number in base 64"
    (with-read state-table "" {"state":= old-state}
      (let* ((seed1 (tx-hash))
             (seed2 (hash (at 'block-time (chain-data))))
             (new-state (hash (concat [old-state seed1 seed2]))))
        (update state-table "" {'state:new-state})
        new-state))
  )

  (defun random-int:integer ()
    "Returns a 256 bit random integer"
    (str-to-int 64 (--random-hash)))

  (defun random-int-range:integer (min_:integer max_:integer)
    "Returns a random integer in range [min - max]"
    (enforce (and (>= min_ 0) (>= max_ 0)) "Min and Max must be positive")
    (enforce (> max_ min_) "Max must be > to min")
    (let ((remainder  (+ (- max_ min_) 1)))
      (+ (mod (random-int) remainder) min_))
  )

  (defun random-decimal-range:decimal (min_:decimal max_:decimal)
    "Returns a random decimal in range [min - max] with a precision of 12"
    (enforce (> max_ min_) "Max must be > to min")
    (let* ((to-int (lambda (x) (floor (* x (pow10 12)))))
           (to-decimal (lambda (x) (floor (* x (pow10 -12)) 12))))
      (+ min_ (to-decimal (random-int-range 0 (to-int (- max_ min_))))))
  )

  (defun random-string:string (len:integer)
    "Returns a random string whose length is given by the argument"
    (let* ((cnt (+ (/ len 43) 1))
           (rnd (random-int))
           (substrings (map (lambda (x) (hash (+ rnd x))) (enumerate 1 cnt))))
      (take len (concat substrings)))
  )

  (defun gen-uuid-rfc-4122-v4:string ()
    "Generate an UUID (Universal Unique ID) according to RFC 4122 v4"
    (let* ((set-bits (lambda (x bits mask) (int-to-str 16 (| bits (& mask (str-to-int 16 x))))))
           (uid-hex (int-to-str 16 (random-int)))
           (field-4 (take -12 uid-hex))
           (field-3 (take -4 (drop -12 uid-hex)))
           (field-3 (set-bits field-3 32768 16383))
           (field-2 (take -4 (drop -16 uid-hex)))
           (field-2 (set-bits field-2 16384 4095))
           (field-1 (take -4 (drop -20 uid-hex)))
           (field-0 (take -8 (drop -24 uid-hex))))
      (join "-" [field-0, field-1, field-2, field-3, field-4]))
  )
)
