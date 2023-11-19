(module policy-fixed-issuance GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use free.util-fungible [enforce-precision])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema fixed-issuance-sch
    max-supply:decimal
    min-mint-amount:decimal
  )

  (deftable supplies:{fixed-issuance-sch})


  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema fixed-issuance-msg-sch
    max_supply:decimal ; Total allowed max supply of the token
    min_mint_amount:decimal ;Minimum allowed mint amount
    precision:integer ;Precision of the token
  )

  (defun read-fixed-supply-msg:object{fixed-issuance-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "fixed_supply" token))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;---------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-fixed-issuance))
    (bind (read-fixed-supply-msg token) {'max_supply:=max-supply,
                                         'min_mint_amount:=min-mint-amount,
                                         'precision:=precision}
      ; Check that min-mint-amount is compliant with precision and positive
      (enforce (and? (<= 0.0) (enforce-precision precision) min-mint-amount) "Invalid min-mint-amount")

      ; Check that max-supply is compliant with precision and positive
      (enforce (and? (< 0.0) (enforce-precision precision) max-supply) "Invalid max-supply")

      ; Check thet min-min-amount is less or equal than max-supply, otherwise it doesn't make sense
      (enforce (<= min-mint-amount max-supply) "Invalid min-mint-amount / max-supply")

      ; Check that the declared precision through the create api is consistent with the message data
      (enforce (= precision (at 'precision token)) "Invalid Precision")

      ; Store the specification for later usage during mint
      (insert supplies (at 'id token) {'min-mint-amount:min-mint-amount,
                                       'max-supply:max-supply})
      true)
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-fixed-issuance))
    (with-read supplies (at 'id token) {'min-mint-amount:=min-mint-amount,
                                        'max-supply:=max-supply}

      (enforce (>= amount min-mint-amount) "Mint amount must be higher than min-mint-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply"))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    false
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-issuance-spec:object{fixed-issuance-sch} (token-id:string)
    (read supplies token-id))
)
