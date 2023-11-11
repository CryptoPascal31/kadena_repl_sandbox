(module policy-fixed-sale GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use ledger [NO-TIMEOUT account-guard])
  (use free.util-time [is-past is-future])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Capabilities and events
  ;-----------------------------------------------------------------------------
  (defcap FORCE-WITHDRAW (sale-id:string)
    @doc "Capability to scope a signature when withdrawing an infinite timeout sale"
    true)

  (defcap FIXED-SALE-OFFER (sale-id:string token-id:string price:decimal)
    @doc "Event sent when a fixed sale is started"
    @event
    true)

  (defcap FIXED-SALE-BOUGHT (sale-id:string token-id:string)
    @doc "Event sent when a fixed sale is bought"
    @event
    true)

  (defcap FIXED-SALE-WITHDRAWN (sale-id:string token-id:string)
    @doc "Event sent when a fixed sale is withdrawn"
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema quote-sch
    sale-id:string
    token-id:string
    seller:string
    seller-guard:guard
    amount:decimal
    escrow-account:string
    currency:module{fungible-v2}
    price:decimal
    recipient:string
    timeout:time
    enabled:bool
  )

  (deftable quotes:{quote-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema fixed-quote-msg-sch
    price:decimal ; Proposed price
    recipient:string ; Recipient account
  )

  (defun read-fixed-quote-msg:object{fixed-quote-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "fixed_quote" token))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun is-registered:bool ()
    (with-default-read quotes (pact-id) {'sale-id:""} {'sale-id:=sale-id}
      (= (pact-id) sale-id)))

  (defun sale-not-ended:bool ()
    (with-read quotes (pact-id) {'timeout:=timeout}
      (or? (= NO-TIMEOUT) (is-future) timeout)))

  (defun sale-ended:bool ()
    (with-read quotes (pact-id)  {'timeout:=timeout}
      (or? (= NO-TIMEOUT) (is-past) timeout)))

  (defun enforce-sale-ended:bool ()
    @doc "Enforce that the sale is ended"
    (let ((ended (sale-ended)))
      (enforce ended "Sale not ended")))

  (defun enforce-sale-not-ended:bool ()
    @doc "Enforce that the sale is not ended"
    (let ((not-ended (sale-not-ended)))
      (enforce not-ended "Sale ended")))

  (defun enforce-seller-guard:bool ()
    @doc "Enforce the seller in case of a NO-TIMEOUT sale"
    (with-read quotes (pact-id) {'seller-guard:=seller-g, 'timeout:=tmout}
      (with-capability (FORCE-WITHDRAW (pact-id))
        (enforce-one "Seller must sign forced-withdrawal"
                     [(enforce (!= NO-TIMEOUT tmout) "") (enforce-guard seller-g)])))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-SALE)

  (defun enforce-init:bool (token:object{token-info})
    false)

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    false)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    false)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    false)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-fixed-sale))
      (bind (read-sale-msg token) {'sale_type:=sale-type,
                                   'currency:=currency:module{fungible-v2}}
        (if (= sale-type "fixed")
          (bind (read-fixed-quote-msg token) {'price:=price, 'recipient:=recipient}
            ; Do some basic checks
            (check-price currency price)

            ; Check that the recipient account already exists in the currency
            (check-fungible-account currency recipient)

            (bind token {'id:=token-id}
              ; Insert the quote into the DB
              (insert quotes (pact-id) {'sale-id: (pact-id),
                                        'token-id: token-id,
                                        'seller: seller,
                                        'seller-guard: (account-guard token-id seller),
                                        'amount:amount,
                                        'escrow-account: (ledger.escrow),
                                        'currency: currency,
                                        'price: price,
                                        'recipient: recipient,
                                        'timeout: timeout,
                                        'enabled: true})
              ; Emit event always returns true
              (emit-event (FIXED-SALE-OFFER (pact-id) token-id price))))
          false))
  )

  (defun --enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-WITHDRAW token (pact-id) policy-fixed-sale))
    (enforce-sale-ended)
    (enforce-seller-guard)
    ; Disable the sale
    (update quotes (pact-id) {'enabled: false})
    ; Emit the corresponding event
    (emit-event (FIXED-SALE-WITHDRAWN (pact-id) (at 'id token)))
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (if (is-registered)
        (--enforce-sale-withdraw token)
        false))

  (defun --enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (ledger.POLICY-ENFORCE-BUY token (pact-id) policy-fixed-sale))
    (enforce-sale-not-ended)
    ; First step to handle the buying part => Transfer the amount to the escrow account
    (with-read quotes (pact-id) {'currency:=currency:module{fungible-v2},
                                 'price:=price}
      (currency::transfer-create buyer (ledger.escrow) (ledger.escrow-guard) price))
    ; Emit the corresponding event
    (emit-event (FIXED-SALE-BOUGHT (pact-id) (at 'id token)))
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (if (is-registered)
        (--enforce-sale-buy token buyer)
        false))

  (defun --enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-fixed-sale))
    (with-read quotes (pact-id) {'currency:=currency:module{fungible-v2},
                                 'recipient:=recipient}
      ; The (enforce-settle) handler is called in the same transaction
      ; as the (enforce-buy) handler
      ; => Checking the timeout is not necessary
      ; Transfer the remaining from the escrow account to the recipient
      (let* ((escrow (ledger.escrow))
             (escrow-total-bal (currency::get-balance escrow)))
        (install-capability (currency::TRANSFER escrow recipient escrow-total-bal))
        (currency::transfer escrow recipient escrow-total-bal)))
    ; Disable the sale
    (update quotes (pact-id) {'enabled: false})
    true
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    (if (is-registered)
        (--enforce-sale-settle token)
        false))

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-sale:object{quote-sch} (sale-id:string)
    @doc "Return the sale details of a given sale-id"
    (read quotes sale-id))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun get-all-active-sales:[object{quote-sch}] ()
    @doc "Return all currently active sales"
    (select quotes (where 'enabled (=  true))))

  (defun get-sales-from-account:[object{quote-sch}] (account:string)
    @doc "Return all currently active sales from an account"
    (select quotes (and? (where 'enabled (=  true))
                         (where 'seller (= account)))))

  (defun get-sales-for-token:[object{quote-sch}] (token-id:string)
    @doc "Return all currently actives sales details related to a token"
    (select quotes (and? (where 'enabled (=  true))
                         (where 'token-id (= token-id)))))
)
