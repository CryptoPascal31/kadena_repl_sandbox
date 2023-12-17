(module governance GOVERNANCE

  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))

  ; Lock new modules in the namespace
  (defconst NAMESPACE-LOCK:bool (read-msg 'lock-namespace))

  (defcap GOVERNANCE ()
    (enforce-governance))

  (defun enforce-governance:bool ()
    (enforce-keyset ADMIN-KEYSET))

  (defun namespace-user-guard:bool ()
    (if NAMESPACE-LOCK
        (enforce false "Namespace locked")
        true)
  )
)
