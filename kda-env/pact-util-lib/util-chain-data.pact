;SPDX-License-Identifier: MIT
;
; This module maps env data to normal Pact functions.
;
; It helps to improve code readability
;
; Be aware that this module is only in Beta and hasn't been audited:
;    --> BE CAREFUL if a security enforcement depends on one of theses functions
;
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib
;
;
(module util-chain-data GOV
  "This module provides some helpers to retrieve env data \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  (defconst VERSION:string "0.7")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (defun chain-id:string ()
    (at 'chain-id (chain-data)))

  (defun block-height:integer ()
    (at 'block-height (chain-data)))

  (defun block-time:time ()
    (at 'block-time (chain-data)))

  (defun prev-block-hash:string ()
    (at 'prev-block-hash (chain-data)))

  (defun sender:string ()
    (at 'sender (chain-data)))

  (defun gas-limit:integer ()
    (at 'gas-limit (chain-data)))

  (defun gas-price:decimal ()
    (at 'gas-price (chain-data)))

  (defun total-gas-limit:decimal ()
    (* (dec (gas-limit)) (gas-price)))
)
