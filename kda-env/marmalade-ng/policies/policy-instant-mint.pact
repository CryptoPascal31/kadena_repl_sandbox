(module policy-instant-mint GOVERNANCE
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
  ; Tables and schema
  ;-----------------------------------------------------------------------------
  ; Store tokens data
  (defschema instant-mint-sch
    creation-tx:string
  )

  (deftable instant-mint:{instant-mint-sch})

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-instant-mint))
    (insert instant-mint (at 'id token)
            {'creation-tx:(tx-hash)})
    true
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-instant-mint))
    (with-read instant-mint (at 'id token)
               {'creation-tx:=creation-tx}
      (enforce (= creation-tx (tx-hash)) "Token must be minted in the same transaction"))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    false)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    false)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    false)

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    false)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    false)

  (defun enforce-sale-settle:bool (token:object{token-info})
    false)
)
