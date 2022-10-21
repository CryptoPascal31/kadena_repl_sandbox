;SPDX-License-Identifier: MIT

; This module provides some utilities to work with lists in Pact
; Be aware that this module is only in Beta and hasn't been audited:
;    --> BE CAREFUL if a security enforcement depends on one of these functions
;
; Remember that in Pact, all variables are immutable and no function can
; work in place. New lists are always returned.
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib

(module util-lists GOV
  "This module provides some lists management utilities \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  (defconst VERSION:string "0.2")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (defun enforce-not-empty:bool (x:list)
    "Verify and ENFORCES that a list is not empty"
    (enforce (> (length x) 0) "List cannot be empty"))

  (defun enforce-list-bounds:bool (x:list idx:integer)
    "Verify and ENFORCES that idx is in list bounds"
    (enforce (and? (<= 0) (> (length x)) idx) "Index out of bounds"))

  (defun chain:list (in:list)
    "Chain list of lists"
    (fold (+) [] in))

  (defun enumerate-list:[object] (in:list)
    "Returns a list of objects {'i:idx, 'v:value} where i is the index, and v the value"
    ; The enumerate should go from 0 to N-1, but since zip takes the shortest, and for clarity we go from 0 to N
    (let ((indexes (enumerate 0 (length in))))
      (zip (lambda (idx x) {'i:idx, 'v:x}) indexes in))
  )

  ;; Getter Funtcions
  (defun first (in:list)
    "Returns the first item of a list"
    (enforce-not-empty in)
    (at 0 in))

  (defun last (in:list)
    "Returns the last item of the list"
    (enforce-not-empty in)
    (at (- (length in) 1) in))

  (defun search:[integer] (in:list item)
    "Search an item into the list and returns a list of index"
    ; Save gas if item is not in list => use the native contains to return empty
    (if (contains item in)
        (let ((match-func (lambda (out-list x)
                                  (if (= (at 'v x) item)
                                      (append-last out-list (at 'i x))
                                      out-list))))
          (fold match-func [] (enumerate-list in)))
        [])
  )

  (defun count:integer (in:list item)
    "Returns the number of occurences of an item"
    (length (filter (= item) in))
  )

  ;; Insertion functions
  (defun insert-first:list (in:list item)
    "Insert an item at the left of the list"
    (+ [item] in))

  (defun append-last:list (in:list item)
    "Append an item at the end of the list"
    (+ in [item]))

  (defun insert-at:list (in:list idx:integer item)
    "Insert an item at position idx"
    (enforce (and? (<= 0) (>= (length in)) idx) "Index out of bounds")
    (chain [(take idx in),
            [item],
            (drop idx in)])
  )

  ;; Replacement functions
  (defun replace-first:list (in:list item)
    "Replace the first item of the list"
    (enforce-not-empty in)
    (insert-first (drop 1 in) item))

  (defun replace-last:list (in:list item)
    "Replace the last item of the list"
    (enforce-not-empty in)
    (append-last (drop -1 in) item))

  (defun replace-at:list (in:list idx:integer item)
    "Replace the item at position idx"
    (enforce (and? (<= 0) (> (length in)) idx) "Index out of bounds")
    (chain [(take idx in),
            [item],
            (drop (+ 1 idx) in)])
  )

  (defun replace-item:list (in:list old-item new-item)
    "Replace each occurrence of old-item by new-item"
    (map (lambda (x) (if (= x old-item) new-item x)) in)
  )

  (defun replace-item*:list (in:list old-item new-item)
    "Replace each occurrence of old-item by new-item but raises an error if old-item does not exist"
    (enforce (contains old-item in) "The item is not present in the list")
    (replace-item in old-item new-item)
  )

  ;; Removal functions
  (defun remove-first:list (in:list)
    "Remove first element from the list"
    (enforce-not-empty in)
    (drop 1 in)
  )

  (defun remove-last:list (in:list)
    "Remove last element from the list"
    (enforce-not-empty in)
    (drop -1 in)
  )

  (defun remove-at:list (in:list idx:integer)
    "Remove element at position idx"
    (enforce-list-bounds in idx)
    (+ (take idx in) (drop (+ 1 idx) in))
  )

  (defun remove-item:list (in:list item)
    "Remove an item from a list"
    (filter (!= item) in)
  )

  (defun remove-item*:list (in:list item)
    "Remove and item from the list but raises an error if it does not exist"
    (enforce (contains item in) "The item is not present in the list")
    (remove-item in item)
  )
)
