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

  (defconst VERSION:string "0.8")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (defun enforce-not-empty:bool (x:list)
    "Verify and ENFORCES that a list is not empty"
    (enforce (is-not-empty x) "List cannot be empty"))

  (defun is-empty:bool (x:list)
    "Return true if the list is empty"
    (= 0 (length x)))

  (defun is-not-empty:bool (x:list)
    "Return true if the list is not empty"
    (< 0 (length x)))

  (defun is-singleton:bool (x:list)
    "Return true if the list is a singleton"
    (= 1 (length x)))

  (defun is-pair:bool (x:list)
    "Return true if the list is a pair"
    (= 2 (length x)))

  (defun enforce-list-bounds:bool (x:list idx:integer)
    "Verify and ENFORCES that idx is in list bounds"
    (enforce (and? (<= 0) (> (length x)) idx) "Index out of bounds"))

  (defun chain:list (in:list)
    "Chain list of lists"
    (fold (+) [] in))

  (defschema list-enum
    "Object returned by enumerate-list"
    i:integer
    v)

  (defun enumerate-list:[object{list-enum}] (in:list)
    "Returns a list of objects {'i:idx, 'v:value} where i is the index, and v the value"
    ; The enumeration should go from 0 to N-1, but since zip takes the shortest, and for clarity we go from 0 to N
    (let ((indexes (enumerate 0 (length in))))
      (zip (lambda (idx:integer x) {'i:idx, 'v:x}) indexes in))
  )

  (defun contains*:bool (in:list item)
    "Starred version of contains for list => arguments inverted"
    (contains item in)
  )

  ;; Getter Functions
  (defun first (in:list)
    "Returns the first item of a list"
    (enforce-not-empty in)
    (at 0 in))

  (defun last (in:list)
    "Returns the last item of the list"
    (enforce-not-empty in)
    (at (- (length in) 1) in))

  (defun at* (in:list idx:integer default)
    "Returns the element at idx, but returns default if the list is too short"
    (enforce (>= idx 0) "Index cannot be negative")
    (if (>= idx (length in))
      default
      (at idx in))
  )

  (defun search:[integer] (in:list item)
    "Search an item into the list and returns a list of index"
    ; Save gas if item is not in list => use the native contains to return empty
    (if (contains item in)
        (let ((indexes (enumerate 0 (length in)))
              (match (lambda (v i) (if (= item v) i -1))))
          (remove-item (zip (match) in indexes) -1))
        [])
  )

  (defun count:integer (in:list item)
    "Returns the number of occurences of an item"
    (length (filter (= item) in))
  )

  ;; Creation and extension functions
  (defun make-list-like:list (in:list value)
    "Creates a new list whose size is the same as in, by repeating value"
    (make-list (length in) value)
  )

  (defun extend:list (in:list new-length:integer value)
    "Extends a list to new-length by repeating value"
    (let ((missing-items (- new-length (length in))))
      (if (<= missing-items 0)
          in
          (+ in (make-list missing-items value))))
  )

  (defun extend-like:list (in:list target:list value)
    "Extends a list to the same length as target, by repeating value"
    (extend in (length target) value)
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

  (defun insert-at*:list (in:list idx:integer item default)
    "Insert an item at position idx, extends the list if it is too short using the default value"
    (insert-at (extend in idx default) idx item)
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

  (defun replace-at*:list (in:list idx:integer item default)
    "Replace an item at position idx, extends the list if it is too short using the default value"
    (replace-at (extend in (+ idx 1) default) idx item)
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

  ;; Shift/Roll functions
  (defun shift-left:list (in:list item)
    "Shift a list to the left"
    (remove-first (append-last in item)))

  (defun shift-right:list (in:list item)
    "Shift a list to the right"
    (remove-last (insert-first in item)))

  (defun roll-left:list (in:list)
    "Roll a list to the left"
    (shift-left in (first in)))

  (defun roll-right:list (in:list)
    "Roll a list to the right"
    (shift-right in (last in)))

  (defun fifo-push:list (in:list fifo-size:integer item)
    "Append an item at the right, and shift left if the FIFO if full"
    (if (>= (length in) fifo-size)
      (shift-left in item)
      (append-last in item))
  )
)
