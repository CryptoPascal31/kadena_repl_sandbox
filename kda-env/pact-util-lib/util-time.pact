;SPDX-License-Identifier: MIT

; This modules provides some convenient time management function for Pact.
;
; Be aware that this module is only in Beta and hasn't been audited:
;    --> BE CAREFUL if a security enforcement depends on one of theses functions
;
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib

(module util-time GOV
  "Module containing time utilities \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  (defconst VERSION:string "0.4")
  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (defconst EPOCH (time "1970-01-01T00:00:00Z"))

  (defconst GENESIS (time "2019-10-30T00:01:00Z"))

  (defconst BLOCK-TIME 30.0)


  ; General functions
  (defun epoch:time ()
    "Returns Unix EPOCH"
    EPOCH)

  (defun genesis:time ()
    "Returns Kadena Genesis time"
    GENESIS)

  (defun now:time ()
    "Returns the current time"
    (at 'block-time (chain-data)))

  (defun to-timestamp:decimal (in:time)
    "Computes an Unix timestamp of the input date"
    (diff-time in (epoch))
  )

  (defun from-timestamp:decimal (timestamp:decimal)
    "Computes a time from an Unix timestamp"
    (add-time (epoch) timestamp)
  )

  ;; Compare functions
  (defun earliest:time (time1:time time2:time)
    "Returns the earliest time between time1 and time2"
    (if (< time1 time2) time1 time2)
  )

  (defun latest:time (time1:time time2:time)
    "Returns the lastest time betwwen time1 and time2"
    (if (> time1 time2) time1 time2)
  )

  (defun time-between:bool (time1:time time2:time in:time)
    "Returns true if in is between time1 and time2"
    (let ((a (earliest time1 time2))
          (b (latest time1 time2)))
      (and? (<= a) (>= b) in))
  )

  ;; Block estimation function
  (defun est-height-at-time:integer (target-time:time)
    "Estimates the block height at a target-time"
    (let* ((delta (diff-time target-time (now)))
          (current-block (at 'block-height (chain-data)))
          (est-block (round (+ current-block (/ delta BLOCK-TIME)))))
      (if (> est-block 0 ) est-block 0))
  )

  (defun est-time-at-height:time (target-block:integer)
    "Estimates the time of the target-block height"
    (let* ((current-block (at 'block-height (chain-data)))
           (delta (- target-block current-block)))
      (add-time (now) (* BLOCK-TIME delta)))
  )

  ;; Diff time functions
  (defun diff-time-minutes:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in minutes"
    (/ (diff-time time1 time2) 60.0)
  )

  (defun diff-time-hours:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in hours"
    (/ (diff-time time1 time2) 3600.0)
  )

  (defun diff-time-days:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in days"
    (/ (diff-time time1 time2) 86400.0)
  )
)
