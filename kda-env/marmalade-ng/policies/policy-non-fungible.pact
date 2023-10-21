(module policy-non-fungible GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-unity:bool (amount:decimal)
    (enforce (= amount 1.0) "Non-fungible: amount can only be 1"))

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-non-fungible))
    (enforce (= 0 (at 'precision token)) "Precision must be 0")
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-non-fungible))
    (enforce (= (at 'supply token) 0.0) "Only one mint allowed")
    (enforce-unity amount)
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-BURN token policy-non-fungible))
    (enforce-unity amount)
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-non-fungible))
    (enforce-unity amount)
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-non-fungible))
    (enforce-unity amount)
    false
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)
)
