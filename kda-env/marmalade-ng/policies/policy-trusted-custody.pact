(module policy-trusted-custody GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use ledger [key])
  (use util-policies)
  (use free.util-strings [starts-with])
  (use free.util-fungible [enforce-valid-account])
  (use free.util-lists [remove-item append-last])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema trusted-custody-token-sch
    token-id: string
    guard: guard ; Owned by the creator: used to protect the custodian list
    custodians:[string] ; List of allowed custodians prefixes
  )

  (defschema trusted-custody-account-sch
    balance:decimal
  )

  (deftable tokens-custody:{trusted-custody-token-sch})
  (deftable accounts-custody:{trusted-custody-account-sch})

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------
  (defcap UPDATE-CUSTODIAN (token-id:string)
    @doc "Capability add/remove a custodian"
    @event
    (with-read tokens-custody token-id {'guard:=g}
      (enforce-guard g))
  )

  (defcap UPDATE-BALANCES()
    @doc "Internal capability to protect custodians balance"
    true)

  ;-----------------------------------------------------------------------------
  ; Constants
  ;-----------------------------------------------------------------------------
  (defconst MAX-CUSTODIANS-COUNT:integer 32)

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema trusted-custody-msg
    guard:guard ; Owned by the creator: used to protect the custodian list
    custodians:[string] ; List of allowed custodians prefixes
  )

  (defun read-trusted-custody-msg:object{trusted-custody-msg} (token:object{token-info})
    (enforce-get-msg-data "trusted_custody" token))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun is-custodian:bool (token-id:string account:string)
    @doc "Return true if the account is a trusted custodian for this token"
    (with-read tokens-custody token-id {'custodians:=custodians}
      (fold (or) false
            (map (starts-with account) custodians))))

  (defun debit-balance:bool (key:string amount:decimal)
    @doc "Try to debit balance of a custodian account: return true in case of success"
    (require-capability (UPDATE-BALANCES))
    (with-default-read accounts-custody key {'balance:0.0} {'balance:=balance}
      (if (>= balance amount)
          (!= "" (update accounts-custody key {'balance:(- balance amount)}))
          false))
  )

  (defun credit-balance:bool (key:string amount:decimal)
    @doc "Credit balance of a custodian account only if the account is a custodian. \
        \ Always return true"
    (require-capability (UPDATE-BALANCES))
    (with-default-read accounts-custody key {'balance:0.0} {'balance:=balance}
      (write accounts-custody key {'balance:(+ balance amount)})
      true)
  )

  (defun check-receiver:bool (token-id:string account:string amount:decimal)
    (if (is-custodian token-id account)
        (credit-balance (key token-id account) amount)
        false)
  )

  (defun check-sender:bool (token-id:string account:string amount:decimal)
    (debit-balance (key token-id account) amount)
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-trusted-custody))
    (let ((token-id (at 'id token)))
      (bind (read-trusted-custody-msg token) {'guard:=g, 'custodians:=custodians}
        ; Check the that the custodian list is not too big
        (enforce (<= (length custodians) MAX-CUSTODIANS-COUNT) "Too many custodians")
        ; Check the that all the custodians are valid account names
        (map (enforce-valid-account) custodians)
        ; Insert the record into database
        (insert tokens-custody token-id {'token-id:token-id,
                                         'guard:g,
                                         'custodians:custodians})
        true))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-trusted-custody))
    (with-capability (UPDATE-BALANCES)
      (check-receiver (at 'id token) account amount))
    true
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-trusted-custody))
    (with-capability (UPDATE-BALANCES)
      (let* ((token-id (at 'id token))
             (sender-ok (check-sender token-id sender amount))
             (receiver-ok (check-receiver token-id receiver amount)))
        (enforce (or sender-ok receiver-ok) (format "Transfer not allowed to {} / {}" [sender receiver]))))
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    false)

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)

  ;-----------------------------------------------------------------------------
  ; Edit functions
  ;-----------------------------------------------------------------------------
  (defun add-custodian:string (token-id:string custodian-prefix:string)
    (enforce-valid-account custodian-prefix)
    (with-capability (UPDATE-CUSTODIAN token-id)
      (with-read tokens-custody token-id {'custodians:=current}
        (enforce (< (length current) MAX-CUSTODIANS-COUNT) "Too many custodians")
        (update tokens-custody token-id {'custodians: (distinct (append-last current custodian-prefix))})))
  )

  (defun remove-custodian:string (token-id:string custodian-prefix:string)
    (with-capability (UPDATE-CUSTODIAN token-id)
      (with-read tokens-custody token-id {'custodians:=current}
        (update tokens-custody token-id {'custodians: (remove-item current custodian-prefix)})))
  )

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-custodians-list:[string] (token-id:string)
    (with-read tokens-custody token-id {'custodians:=custodians}
      custodians)
  )

  (defun custodian-balance:decimal (token-id:string custodian-account:string)
    (with-default-read accounts-custody (key token-id custodian-account)
                       {'balance:0.0} {'balance:=balance}
      balance)
  )
)
