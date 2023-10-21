(module policy-marketplace GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use free.util-math [clamp])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema marketplace-sale-sch
    sale-id:string
    token-id:string
    marketplace-fee:object{marketplace-fee-sch}
    marketplace-hash:string
    enabled:bool
  )

  (deftable marketplace-sales:{marketplace-sale-sch})

  ;-----------------------------------------------------------------------------
  ; Events
  ;-----------------------------------------------------------------------------
  (defcap MARKETPLACE-PAID (token-id:string marketplace-account:string marketplace-hash:string amount:decimal)
    @doc "Event emitted when a fee is paid to the marketplace"
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema marketplace-fee-sch
    marketplace-name:string ; Name of the marketplace : Informative only
    marketplace-account:string ; Marketplace account : where the funds must be sent
    currency:module{fungible-v2} ; Currency : must be the same as the sale structyre
    min-fee:decimal ; Minimum absolute fee
    fee-rate:decimal ; Fee rate
    max-fee:decimal ; Maximum absolute fee
  )

  (defconst DEFAULT-MARKETPLACE-FEE:object{marketplace-fee-sch} {'marketplace-name:"",
                                                                 'marketplace-account:"",
                                                                 'currency:coin,
                                                                 'min-fee:0.0,
                                                                 'fee-rate:0.0,
                                                                 'max-fee:0.0})

  (defun read-marketplace-msg:object{marketplace-fee-sch} (token:object{token-info})
    (get-msg-data "marketplace" token DEFAULT-MARKETPLACE-FEE))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun enforce-marketplace-input-valid:bool (input:object{marketplace-fee-sch})
    @doc "Check the validity of a market place input object \
        \  - Fees must be consistent                        \
        \  - Account must exist for the currency"
    (bind input {'marketplace-account:=account,
                 'currency:=currency:module{fungible-v2},
                 'min-fee:=min-fee,
                 'fee-rate:=fee-rate,
                 'max-fee:=max-fee}
      ; Are min-fee and max-fee are positive and max-fee > min-fee ?
      (enforce (and (and (<= 0.0 min-fee) (<= 0.0 max-fee))
                    (<= min-fee max-fee)) "Illegal Min/Max fee")

      ; Is fee-rate is between 0.0 and 1.0 ?
      (enforce-valid-rate fee-rate)

      ; Is the payment account OK ?
      (check-fungible-account currency account))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-SELLER-FEE)

  (defun enforce-init:bool (token:object{token-info})
    true)

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true)


  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-marketplace))
    (let ((sales-msg (read-sale-msg token))
          (marketplace-msg (read-marketplace-msg token)))

      ; If both messages are present
      (if (and (!= sales-msg DEFAULT-SALE-MSG) (!= marketplace-msg DEFAULT-MARKETPLACE-FEE))
        (let ((_ 0))
          ; Check that both currencies are the same
          (enforce (= (at 'currency sales-msg) (at 'currency marketplace-msg)) "Currency error")
          ; Check market-place object
          (enforce-marketplace-input-valid marketplace-msg)
          ; Store the data
          (insert marketplace-sales (pact-id) {'sale-id:(pact-id),
                                               'token-id: (at 'id token),
                                               'marketplace-fee:marketplace-msg,
                                               'marketplace-hash:(hash marketplace-msg),
                                               'enabled:true})
          false)
        false))
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-WITHDRAW token (pact-id) policy-marketplace))
    ; Check whether marketplace policy has been activated for this sale
    (with-default-read marketplace-sales (pact-id) {'marketplace-hash:"DEFAULT-HASH"} {'marketplace-hash:=mh}
      (if (!= mh "DEFAULT-HASH")
          ; If yes, flag the sale as disabled (=ended)
          (update marketplace-sales (pact-id) {'enabled: false})
          ""))
    true
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-marketplace))
    (with-default-read marketplace-sales (pact-id) {'marketplace-fee:DEFAULT-MARKETPLACE-FEE,
                                                    'marketplace-hash:"DEFAULT-HASH"}
                                                   {'marketplace-fee:=market-fee,
                                                    'marketplace-hash:=market-hash}
      (if (!= market-hash "DEFAULT-HASH")
          (bind market-fee {'marketplace-account:=account,
                            'currency:=currency:module{fungible-v2},
                            'min-fee:=min-fee,
                            'fee-rate:=fee-rate,
                            'max-fee:=max-fee}

            (let* ((escrow (ledger.escrow))
                   (escrow-balance (currency::get-balance escrow))
                   (amount (clamp min-fee max-fee  (* fee-rate escrow-balance)))
                   (amount (floor amount (currency::precision))))

              (if (> amount 0.0)
                  (let ((_ 0))
                    (install-capability (currency::TRANSFER escrow account escrow-balance))
                    (currency::transfer escrow account amount)
                    (emit-event (MARKETPLACE-PAID (at 'id token) account market-hash amount)))
                  true))
            (update marketplace-sales (pact-id) {'enabled: false})
            true)
          true))
    )

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-marketplace-fee:object{marketplace-sale-sch} (sale-id:string)
    @doc "Return the detail of the marketplace fee record for the given sale"
    (read marketplace-sales sale-id))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun get-active-sales-by-name:[object{marketplace-sale-sch}] (market-name:string)
    @doc "Return the list (with details) of the active sales for given marketplace"
    (select marketplace-sales (and? (where 'enabled (= true))
                                    (compose (at 'marketplace-fee)
                                             (where 'marketplace-name (= market-name)))
                               ))
  )

  (defun get-active-sales-by-market-hash:[object{marketplace-sale-sch}] (fee-hash:string)
      @doc "Return the list (with details) of the active sales for given fee hash"
      (select marketplace-sales (and? (where 'enabled (= true))
                                      (where 'marketplace-hash (= fee-hash))))
  )
)
