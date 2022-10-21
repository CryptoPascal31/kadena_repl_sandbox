(namespace 'kip)

(interface token-policy-v1

  (defschema token-info
    id:string
    supply:decimal
    precision:integer
    manifest:object{kip.token-manifest.manifest})

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Minting policy for TOKEN to ACCOUNT for AMOUNT."
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    @doc "Burning policy for TOKEN to ACCOUNT for AMOUNT."
    @model [
      (property (!= account ""))
      (property (> amount 0.0))
    ]
  )

  (defun enforce-init:bool
    (token:object{token-info})
    @doc "Enforce policy on TOKEN initiation."
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    @doc "Offer policy of sale SALE-ID by SELLER of AMOUNT of TOKEN."
  )


  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    @doc "Buy policy on SALE-ID by SELLER to BUYER AMOUNT of TOKEN."
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    @doc " Enforce rules on transfer of TOKEN AMOUNT from SENDER to RECEIVER. \
         \ Also governs rotate of SENDER (with same RECEIVER and 0.0 AMOUNT). "
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    @doc " Enforce rules on crosschain transfer of TOKEN AMOUNT \
         \ from SENDER to RECEIVER on TARGET-CHAIN."
  )
)
