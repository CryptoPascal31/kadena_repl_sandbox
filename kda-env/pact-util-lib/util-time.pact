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

  (defconst VERSION:string "0.11")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (use util-chain-data [block-time block-height])
  (use util-math [between pow10])

  (defconst EPOCH:time (time "1970-01-01T00:00:00Z"))

  (defconst HASKELL-EPOCH:time (time "1858-11-17T00:00:00Z"))

  (defconst GENESIS:time (time "2019-10-30T00:01:00Z"))

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
    (block-time))

  ;; Safe time computation management
  ;
  ; (add-time) uses Haskell time library and can overflow
  ; Haskell computes time from the TAI EPOCH ("1858-11-17T00:00:00Z") is useconds.
  ;   in signed int64 (min = - 2^63, max = 2 ^63 -1)
  ;
  ; To be sure, we never overflowwe limits:
  ;   - Every usable time to (TAI EPOCH +/-  2^62/1e6 -1)
  ;   - Every usable offset to (+/-  2^62/1e6 -1)
  ;
  ; By enforcing such limits, we can guarantee that time functions never overflow.
  ;
  ; When a Smart contract developer uses (add-time), (diff-time), (time) or (parse-time) with
  ; user supplied inputs, he should preferably use safe counterparts to avoid non-expected
  ; behaviour that could yield to a security issue.
  ;
  ; For parsing functions: ie (time) and (parse-time), we compare the input string with
  ; the stringified parsed date. If there is a difference, it means that an overflow probably occured
  (defconst SAFE-DELTA:decimal (- (/ (^ 2.0 62.0) (pow10 6)) 1.0))

  (defconst MIN-SAFE-TIME:time (add-time HASKELL-EPOCH (- SAFE-DELTA)))

  (defconst MAX-SAFE-TIME:time (add-time HASKELL-EPOCH SAFE-DELTA))

  (defun --enforce-safe-time:bool (in:time)
    (enforce (time-between MIN-SAFE-TIME MAX-SAFE-TIME in) "Time out of safe bounds"))

  (defun --enforce-safe-delta:bool (in:decimal)
    (enforce (between (- SAFE-DELTA) SAFE-DELTA in) "Delta out of safe bounds"))

  (defun time-safe:time (in:string)
    "Do a (time) without any risk of overflow"
    (let ((t (time in)))
      (enforce (= in (format-time "%Y-%m-%dT%H:%M:%SZ" t)) "Unsafe time conversion")
      (--enforce-safe-time t)
      t)
  )

  (defun parse-time-safe:time (fmt:string in:string)
    "Do a (parse-time) without any risk of overflow"
    (let ((t (parse-time fmt in)))
      (enforce (= in (format-time fmt t)) "Unsafe time conversion")
      (--enforce-safe-time t)
      t)
  )

  (defun add-time-safe:time (in:time delta:decimal)
    "Do a (add-time) without any risk of overflow"
    (--enforce-safe-time in)
    (--enforce-safe-delta delta)
    (add-time in delta)
  )

  (defun diff-time-safe:decimal (x:time y:time)
    "Do a (diff-time) without any risk of overflow"
    (--enforce-safe-time x)
    (--enforce-safe-time y)
    (diff-time x y)
  )

  (defun tomorrow:time ()
    "Returns current time + 24 hours"
    (from-now (days 1))
  )

  (defun yesterday:time ()
    "Returns current time - 24 hours"
    (from-now (days -1))
  )

  (defun from-now:time (delta:decimal)
    "Returns the delta time taking now as a reference"
    (--enforce-safe-delta delta)
    (add-time (now) delta)
  )

  (defun today:string ()
    "Returns the current day"
    (format-time "%F" (now))
  )

  (defun to-timestamp:decimal (in:time)
    "Computes an Unix timestamp of the input date"
    (--enforce-safe-time in)
    (diff-time in (epoch))
  )

  (defun from-timestamp:time (timestamp:decimal)
    "Computes a time from an Unix timestamp"
    (--enforce-safe-delta timestamp)
    (add-time (epoch) timestamp)
  )

  ;; Compare functions
  (defun earliest:time (time1:time time2:time)
    "Returns the earliest time between time1 and time2"
    (if (< time1 time2) time1 time2)
  )

  (defun latest:time (time1:time time2:time)
    "Returns the latest time between time1 and time2"
    (if (> time1 time2) time1 time2)
  )

  (defun time-between:bool (time1:time time2:time in:time)
    "Returns true if in is between time1 and time2"
    (let ((a (earliest time1 time2))
          (b (latest time1 time2)))
      (and? (<= a) (>= b) in))
  )

  (defun is-past:bool (in:time)
    "Returns true if the date is in the past (before now)"
    (< in (now))
  )

  (defun is-future:bool (in:time)
    "Returns true if the date is in the future (after now)"
    (> in (now))
  )

  (defun is-today:bool (in:time)
    "Returns true if the time in is in the current day"
    (let ((in-day (format-time "%F" in)))
      (= (today) in-day))
  )


  (defun est-height-at-time:integer (target-time:time)
    "Estimates the block height at a target-time"
    (--enforce-safe-time target-time)
    (let ((delta (diff-time target-time (now)))
          (est-block (+ (block-height) (round (/ delta BLOCK-TIME)))))
      (if (> est-block 0 ) est-block 0))
  )

  (defun est-time-at-height:time (target-block:integer)
    "Estimates the time of the target-block height"
    (let* ((delta-blocks (- target-block (block-height)))
           (delta (* BLOCK-TIME (dec delta-blocks))))
      (--enforce-safe-delta delta)
      (add-time (now) delta))
  )

  ;; Diff time functions
  (defun diff-time-minutes:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in minutes"
    (/ (diff-time-safe time1 time2) 60.0)
  )

  (defun diff-time-hours:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in hours"
    (/ (diff-time-safe time1 time2) 3600.0)
  )

  (defun diff-time-days:decimal (time1:time time2:time)
    "Computes difference between TIME1 and TIME2 in days"
    (/ (diff-time-safe time1 time2) 86400.0)
  )
)
