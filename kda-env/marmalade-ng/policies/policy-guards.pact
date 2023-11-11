(module policy-guards GOVERNANCE
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
  ; Same schema is used for guards creation and storage in database
  (defschema guards-sch
    mint:guard
    burn:guard
    sale:guard
    transfer:guard
  )

  (deftable guards:{guards-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  ; For this policy the input data schema is shared with the in-table schema
  (defun read-guards-msg:object{guards-sch} (token:object{token-info})
    (enforce-get-msg-data "guards" token))

  ;-----------------------------------------------------------------------------
  ; Constants functions and guards helpers
  ;-----------------------------------------------------------------------------
  (defun success:bool ()
    true)

  (defun failure:bool ()
    (enforce false "Disabled")
    true)

  (defconst GUARD_SUCCESS:guard (create-user-guard (success)))
  (defconst GUARD_FAILURE:guard (create-user-guard (failure)))

  (defcap GUARDS:bool (guards:object{guards-sch})
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Capabilities to enforce the guards
  ;-----------------------------------------------------------------------------
  (defcap MINT (token-id:string)
    (with-read guards token-id {'mint:=g}
      (enforce-guard g)))

  (defcap BURN (token-id:string)
    (with-read guards token-id {'burn:=g}
      (enforce-guard g)))

  (defcap SALE (token-id:string)
    (with-read guards token-id {'sale:=g}
      (enforce-guard g)))

  (defcap TRANSFER (token-id:string)
    (with-read guards token-id {'transfer:=g}
      (enforce-guard g)))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-guards))
    (let ((data (read-guards-msg token)))
      (insert guards (at 'id token) data)
      (emit-event (GUARDS data)))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-guards))
    (with-capability (MINT (at 'id token))
      true)
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-BURN token policy-guards))
    (with-capability (BURN (at 'id token))
      true)
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-guards))
    (with-capability (TRANSFER (at 'id token))
      true)
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-guards))
    (with-capability (SALE (at 'id token))
      false)
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)

  (defun get-guards:object{guards-sch} (token-id:string)
    (read guards token-id))
)
