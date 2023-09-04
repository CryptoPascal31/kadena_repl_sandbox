(interface token-policy-ng-v1

  (defschema token-info
    id:string
    supply:decimal
    precision:integer
    uri:string
  )

  (defun rank:integer ()
    @doc "Returns the rank of the policy"
  )

  (defun enforce-init:bool (token:object{token-info})
    @doc "Enforce policy on TOKEN initiation."
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    @doc "Minting policy for TOKEN to ACCOUNT for AMOUNT."
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    @doc "Burning policy for TOKEN to ACCOUNT for AMOUNT."
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    @doc "Enforce rules on transfer of TOKEN AMOUNT from SENDER to RECEIVER"
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    @doc "Offer policy by SELLER of AMOUNT of TOKEN."
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    @doc "Withdraw policy  of TOKEN"
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    @doc "Buy policy to BUYER of TOKEN."
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    @doc "Settle of TOKEN."
  )
)
