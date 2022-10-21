(module repl-coin-tools GOV
  (defcap GOV () true)
  (use coin)

  (defun fund-account (account-name:string key:string amount:decimal)
  "Fund a coin account from nothing"
    (env-data { "k": [key]})
    (with-applied-env
      (let ((ks:guard (read-keyset 'k)))
        (create-account account-name ks)
        (test-capability (CREDIT account-name))
        (credit account-name ks amount)))
  )

  (defun fund-accounts (account-names:[string] amount:decimal)
    "Fund a list of coin accounts with a constant amount. the key is dervied from the account name"
    (map (lambda (x) (fund-account x (+ x "-key") amount)) account-names)
  )

)
