(module repl-marmalade-tools GOV
  (defcap GOV () true)
  (use marmalade.ledger)

  (defun create-token 
    (
      token-id:string 
      precision:integer 
      scheme:string 
      data:string 
      datum:object 
      policy:module{kip.token-policy-v1}
    )
    (let* 
      (
        (uri (kip.token-manifest.uri scheme data))
        (datum (kip.token-manifest.create-datum uri datum))
        (manifest (kip.token-manifest.create-manifest uri [datum]))
      )
      
      (marmalade.ledger.create-token token-id precision manifest policy)
    )
  )

  (defun fund-account-with-token (account-name:string token-id:string key:string amount:decimal)
  "Fund a marmalade account from nothing"
    (env-data { "k": [key]})
    (with-applied-env
      (let ((ks:guard (read-keyset 'k)))
        (create-account token-id account-name ks)
        (test-capability (MINT token-id account-name amount))
        (mint token-id account-name ks amount)))
  )

  (defun fund-accounts-with-token (account-names:[string] token-id:string amount:decimal)
    "Fund a list of coin accounts with a constant amount. the key is dervied from the account name"
    (map (lambda (x) (fund-account-with-token x token-id (+ x "-key") amount)) account-names)
  )
)
