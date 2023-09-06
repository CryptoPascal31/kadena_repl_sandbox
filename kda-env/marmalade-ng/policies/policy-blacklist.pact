(module policy-blacklist GOVERNANCE
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
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema token-account-blacklist-sch
    token-id:string
    account:string
    blacklisted:bool
  )

  (deftable blacklist:{token-account-blacklist-sch})

  (defschema token-blacklist-guard-sch
    token-id:string
    guard:guard
  )

  (deftable tokens-blacklist:{token-blacklist-guard-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema blacklist-msg-sch
    guard:guard ; Admnistrative guard for blacklisting
  )

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------
  (defcap UPDATE-BLACKLIST (token-id:string account:string blacklist:bool)
    @doc "Capability to blacklist a token account"
    @event
    (with-read tokens-blacklist token-id {'guard:=g}
      (enforce-guard g))
  )

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun is-blacklisted:bool (token-id:string account:string)
    @doc "Return true if the account is blacklisted for a token"
    (let ((t-key (ledger.key token-id account)))
      (with-default-read blacklist t-key {'blacklisted:false} {'blacklisted:=x}
        x))
  )

  (defun enforce-not-blacklisted (token:object{token-info} account:string)
    @doc "Enforce that an account is not blacklisted for a token"
    (let* ((token-id (at 'id token))
           (is-bl (is-blacklisted token-id account)))
      ;(enforce false (format "{}/{}/{}" [token-id account is-bl]))
      (enforce (not is-bl) "Account in blacklist"))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    0)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-blacklist))
    (let* ((token-id:string  (at 'id token))
           (data:object{blacklist-msg-sch} (enforce-get-msg-data "blacklist" token)))
      (insert tokens-blacklist token-id {'token-id:token-id,
                                         'guard: (at 'guard data)}))
    true
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-blacklist))
    (enforce-not-blacklisted token account)
  )


  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-BURN token policy-blacklist))
    (enforce-not-blacklisted token account)
  )


  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-blacklist))
    (enforce-not-blacklisted token sender)
    (enforce-not-blacklisted token receiver)
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-blacklist))
    (enforce-not-blacklisted token seller)
    false
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)

  ;-----------------------------------------------------------------------------
  ; Blacklist functions
  ;-----------------------------------------------------------------------------
  (defun --update-blacklist:string (token-id:string account:string value:bool)
    (with-capability (UPDATE-BLACKLIST token-id account value)
      (let ((t-key (ledger.key token-id account)))
        (write blacklist t-key {'token-id:token-id,
                                'account:account,
                                'blacklisted:value})))
  )

  (defun add-account:string (token-id:string account:string)
    (--update-blacklist token-id account true))

  (defun remove-account:string (token-id:string account:string)
    (--update-blacklist token-id account false))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun list-blacklisted-accounts:[string] (token-id:string)
    @doc "Return all accounts blacklisted for a specific token"
    (map (at 'account)
         (select blacklist (and? (where 'token-id (= token-id))
                                 (where 'blacklisted (= true)))))
  )

  (defun list-blacklisted-tokens:[string] (account:string)
    @doc "Return all tokens blacklisted for a specific account"
    (map (at 'token-id)
         (select blacklist (and? (where 'account (= account))
                                 (where 'blacklisted (= true)))))
  )
)
