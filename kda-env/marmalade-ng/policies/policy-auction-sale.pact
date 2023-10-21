(module policy-auction-sale GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use ledger [NO-TIMEOUT create-account account-exist account-guard])
  (use free.util-time [now is-past is-future])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Capabilities and events
  ;-----------------------------------------------------------------------------
  (defcap AUCTION-SALE-OFFER (sale-id:string token-id:string start-price:decimal)
    @doc "Event sent when an auction is started"
    @event
    true)

  (defcap PLACE-BID (sale-id:string token-id:string buyer:string price:decimal)
    @doc "Event emitted after an external bid"
    @event
    true)

  (defcap AUCTION-SALE-BOUGHT (sale-id:string token-id:string buy-price:decimal)
    @doc "Event sent when an auction has ended"
    @event
    true)

  (defcap AUCTION-SALE-WITHDRAWN (sale-id:string token-id:string)
    @doc "Event sent when an auction has been withdrawn because there is no bid"
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defconst EXTENSION-LIMIT:decimal (minutes 10))

  (defconst EXTENSION-TIME:decimal (minutes 10))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema auction-sch
    sale-id:string
    token-id:string
    seller:string
    amount:decimal
    escrow-account:string
    currency:module{fungible-v2}
    start-price:decimal
    current-price:decimal
    increment-ratio:decimal
    current-buyer:string
    recipient:string
    timeout:time
    enabled:bool
  )

  (deftable auctions:{auction-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema auction-msg-sch
    start_price:decimal ; start price of the auction
    recipient:string ; recipien account
    increment_ratio:decimal ;incrment ratio (multiplier) between each bid
  )

  (defun read-auction-msg:object{auction-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "auction" token))

  ;-----------------------------------------------------------------------------
  ; Escrow accounts
  ;-----------------------------------------------------------------------------

  ; The auction escrow account is different than the escrow account provided by the ledger.
  ;
  ; The auction escrow account has to be used during a bid: ie out of a defpact.
  ; The ledger escrow account is only available during the defpact execution and will be used
  ;    only for the final settlement
  (defcap AUCTION-ESCROW-ACCOUNT (sale-id:string)
    true)

  (defun auction-escrow-guard:guard (sale-id:string)
    (create-capability-guard (AUCTION-ESCROW-ACCOUNT sale-id)))

  (defun auction-escrow:string (sale-id:string)
    (create-principal (auction-escrow-guard sale-id)))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun is-registered:bool ()
    (with-default-read auctions (pact-id) {'sale-id:""} {'sale-id:=sale-id}
      (= (pact-id) sale-id)))

  (defun is-enabled:bool ()
    (with-default-read auctions (pact-id) {'enabled:false} {'enabled:=enabled}
      enabled))

  (defun enforce-enabled:bool ()
    (let ((ok (is-enabled)))
      (enforce ok "Sale disabled")))

  (defun sale-not-ended:bool ()
    (not (sale-ended)))

  (defun sale-ended:bool ()
    (with-read auctions (pact-id) {'timeout:=timeout}
      (is-past timeout)))

  (defun enforce-sale-ended:bool ()
    (let ((ended (sale-ended)))
      (enforce ended "Sale not ended")))

  (defun check-ledger-account:bool (token-id:string account:string guard:guard)
    @doc "Check an account on the Marmalade legder \
        \   - If it already exist, check that the guard does match \
        \   - If it doesn't exist, create it with the provided guard "
    (if (account-exist token-id account)
      (let ((current-guard (account-guard token-id account)))
        (enforce (= current-guard guard) "Guard doesn't match with the existing one"))
      (create-account token-id account guard))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-SALE)

  (defun enforce-init:bool (token:object{token-info})
    true)

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-auction-sale))
    (bind (read-sale-msg token) {'sale_type:=sale-type,
                                 'currency:=currency:module{fungible-v2}}
      (if (= sale-type "auction")
        (bind (read-auction-msg token) {'start_price:=start-price,
                                        'recipient:=recipient,
                                        'increment_ratio:=increment-ratio}
          ; Do some basic checks
          (check-price currency start-price)

          (enforce (> increment-ratio 1.0) "Increment must be > 1")
          ; Auction sale does not support no-timeout
          (enforce (!= NO-TIMEOUT timeout) "No timeout not supported for auction sale")
          ; Check that the recipient account is valid and already exists in the currency
          (check-fungible-account currency recipient)


          (bind token {'id:=token-id}
            ; Insert the quote into the DB
            (insert auctions (pact-id) {'sale-id: (pact-id),
                                        'token-id: token-id,
                                        'seller: seller,
                                        'amount: amount,
                                        'escrow-account: (auction-escrow (pact-id)),
                                        'currency: currency,
                                        'start-price: start-price,
                                        'current-price: 0.0,
                                        'increment-ratio: increment-ratio,
                                        'current-buyer:"",
                                        'recipient: recipient,
                                        'timeout: timeout,
                                        'enabled: true})
            ; Emit event always returns true
            (emit-event (AUCTION-SALE-OFFER (pact-id) token-id start-price))))
        false))
  )

  (defun --enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-WITHDRAW token (pact-id) policy-auction-sale))
    (enforce-enabled)
    (enforce-sale-ended)

    ; Check that nobody has placed a bid before withdrawing
    (with-read auctions (pact-id) {'current-buyer:=current-buyer}
      (enforce (= current-buyer "") "Bid active"))
    ; Disable the sale
    (update auctions (pact-id) {'enabled: false})
    (emit-event (AUCTION-SALE-WITHDRAWN (pact-id) (at 'id token)))
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (if (is-registered)
        (--enforce-sale-withdraw token)
        false))

  (defun --enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (ledger.POLICY-ENFORCE-BUY token (pact-id) policy-auction-sale))
    (enforce-enabled)
    (enforce-sale-ended)
    (with-read auctions (pact-id) {'token-id:=token-id,
                                   'currency:=currency:module{fungible-v2},
                                   'current-price:=buy-price,
                                   'current-buyer:=current-buyer}

      ; Check that the buyer is different from EMPTY. It means that someone has placed a bid
      (enforce (!= current-buyer "") "No bid")

      ; Check that the buyer that placed the bid is the same as the one that is currently
      ;    claiming the tokens
      (enforce (= buyer current-buyer) "Buyer does not match")

      ; Transfer the funds from the "Auction escrow account" -> "Sale escrow account"
      (with-capability (AUCTION-ESCROW-ACCOUNT (pact-id))
        (install-capability (currency::TRANSFER (auction-escrow (pact-id)) (ledger.escrow) buy-price))
        (currency::transfer-create (auction-escrow (pact-id)) (ledger.escrow) (ledger.escrow-guard) buy-price))
      ; Emit the event
      (emit-event (AUCTION-SALE-BOUGHT (pact-id) token-id buy-price)))
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (if (is-registered)
        (--enforce-sale-buy token buyer)
        false))

  (defun --enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-auction-sale))
    ; The (enforce-settle) handler is called in the same transaction
    ; as the (enforce-buy) handler
    ; Transfer the remaining from the escrow account to the recipient
    (with-read auctions (pact-id) {'currency:=currency:module{fungible-v2},
                                   'recipient:=recipient}
      (let* ((escrow (ledger.escrow))
             (amount (currency::get-balance escrow)))
        (install-capability (currency::TRANSFER escrow recipient amount))
        (currency::transfer escrow recipient amount))
      ; Disable the sale
      (update auctions (pact-id) {'enabled: false}))
    true
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    (if (is-registered)
        (--enforce-sale-settle token)
        false))

  ;-----------------------------------------------------------------------------
  ; Bid function
  ;-----------------------------------------------------------------------------
  (defun place-bid:string (sale-id:string buyer:string buyer-guard:guard new-price:decimal)
    @doc "Place a bid to the sale. We assume that the fungible account and ledger account \
        \ have the same name. Hence 'buyer' will be used for both purposes:               \
        \    - for refunding the fungible account in case of a future better bid          \
        \    - for the ledger account "
    (with-read auctions sale-id {'token-id:=token-id,
                                 'currency:=currency:module{fungible-v2},
                                 'start-price:=start-price,
                                 'current-price:=current-price,
                                 'increment-ratio:=increment-ratio,
                                 'current-buyer:=current-buyer,
                                 'timeout:=timeout,
                                 'enabled:=enabled}

      (enforce enabled "Sale disabled")
      (enforce (is-future timeout) "Sale has ended")
      (currency::enforce-unit new-price)

      ; Check that the proposed price is correct:
      ;   - If it is the first bid, it must be at least the starting price
      ;   - If not is should be at least more than previous_bid * increment-ratio
      (enforce (if (= 0.0 current-price)
                   (>= new-price start-price)
                   (>= new-price (* current-price increment-ratio)))
               "Price too low")

      ; The best way to ensure that everything will work as expected during
      ; settlement is to check that current ledger account already exists  with the declared
      ; guard or create-it. This prevents most risks of being front-runned, account squatting with
      ; token stolen.
      (check-ledger-account token-id buyer buyer-guard)

      ; Escrow the new bid
      (currency::transfer-create buyer (auction-escrow sale-id) (auction-escrow-guard sale-id) new-price)

      ; Refund the previous buyer
      (if (!= current-buyer "")
          (with-capability (AUCTION-ESCROW-ACCOUNT sale-id)
            (install-capability (currency::TRANSFER (auction-escrow sale-id) current-buyer current-price))
            (currency::transfer (auction-escrow sale-id) current-buyer current-price))
          "")

    ; Compute the new timeout
    ; extension-timeout is the time where a bid must trigger a time extension
    ; if the time extension is triggered, we calculate the new timeout by adding
    ;   EXTENSION-TIME to the current time
    (let* ((extension-timeout (add-time timeout (- EXTENSION-LIMIT)))
           (new-timeout (if (is-past extension-timeout)
                            (add-time (now) EXTENSION-TIME)
                            timeout)))
      ; Update the database with the new buyer and new price,
      ;  and extended timeout if necessary
      (update auctions sale-id {'current-price:new-price, 'current-buyer:buyer,
                                'timeout: new-timeout}))

    ; Emit the event
    (emit-event (PLACE-BID sale-id token-id buyer new-price))
    ; Return a nice looking string
    (+ "Bid placed for sale: " sale-id))
  )

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-sale:object{auction-sch} (sale-id:string)
    @doc "Return the details of a sale"
    (read auctions sale-id))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun get-all-active-sales:[object{auction-sch}] ()
    @doc "Return all active sales managed by this policy"
    (select auctions (and? (where 'timeout (is-future))
                           (where 'enabled (= true)))))

  (defun get-sales-from-account:[object{auction-sch}] (account:string)
   @doc "Return all currently active sales from an account"
   (select auctions (and? (where 'enabled (=  true))
                          (where 'seller (= account)))))

  (defun get-ended-sales:[object{auction-sch}] ()
    @doc "Return the ended sales (timeout elapsed), but not settled"
    (select auctions (and? (where 'timeout (is-past))
                           (where 'enabled (= true)))))

  (defun get-sales-for-token:[object{auction-sch}] (token-id:string)
    @doc "Return all active sales managed by this policy for a given token-id"
    (select auctions (and? (where 'enabled (= true))
                           (where 'token-id (= token-id)))))
)
