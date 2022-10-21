(module basic-guards GOV
   (defcap GOV () true)

   (defconst GUARD_SUCCESS (create-user-guard (success)))
   (defconst GUARD_FAILURE (create-user-guard (failure)))

   (defun success () true)
   (defun failure () (enforce false "Disabled"))
)
