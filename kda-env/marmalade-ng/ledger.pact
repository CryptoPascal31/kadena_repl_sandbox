(module ledger GOVERNANCE
  (implements kip.poly-fungible-v3)
  (use kip.poly-fungible-v3 [account-details sender-balance-change receiver-balance-change])
  (use token-policy-ng-v1 [token-info])
  (use free.util-strings [to-string starts-with])
  (use free.util-time [time-between now from-now])
  (use free.util-fungible [enforce-precision enforce-reserved enforce-valid-account enforce-valid-transfer enforce-valid-amount])

  ;-----------------------------------------------------------------------------
  ; Tables and schema
  ;-----------------------------------------------------------------------------
  ; The ledger itself: Store Tokens - Accounts correspondance
  ; acount-details is define in the ledger interface
  (deftable ledger:{account-details})

  ; Store tokens data
  ; Only supply can be modified. All other fields of this stored object are immutable
  (defschema token-schema
    id:string ; Id of the token
    uri:string ; URI of the token
    precision:integer ; Precsion = Number of decimals of the token.
    supply:decimal ; Total supply of the token
    policies:[module{token-policy-ng-v1}] ; List of policies attached to a token
  )
  (deftable tokens:{token-schema})

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Some utility constants
  ;-----------------------------------------------------------------------------
  ; Constant used by the RECONCILE event for a mint or a burn
  (defconst NO-BALANCE-CHANGE {'account:"", 'previous: 0.0, 'current: 0.0})

  ; Constant used for sales, that represents a no timeout
  (defconst NO-TIMEOUT:time (time "0000-01-01T00:00:00Z"))

  ; Maximum tiemeout in days => 10 years
  (defconst MAXIMUM-TIMEOUT:decimal (days (* 10.0 365.25)))

  ;-----------------------------------------------------------------------------
  ; Events
  ;-----------------------------------------------------------------------------
  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc "Emitted when supply is updated"
    @event
    true)

  (defcap TOKEN-CREATE:bool (id:string uri:string precision:integer policies:[string])
    @doc "Emitted when a token is created"
    @event
    true)

  (defcap RECONCILE:bool (token-id:string amount:decimal
                          sender:object{sender-balance-change}
                          receiver:object{receiver-balance-change})
    @doc "For accounting via events"
    @event
    true)

  (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
    @doc "Deprecated... Not emitted anymore"
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Ledger Caps
  ;-----------------------------------------------------------------------------
  (defcap DEBIT (id:string sender:string)
    @doc "Internal capability to allow the debit of an account \
        \ Can only be acquired by composing"
    (enforce-guard (account-guard id sender)))

  (defcap CREDIT (id:string receiver:string)
    @doc "Internal capability to allow the credit of an account \
        \ Can only be acquired by composing"
    true)

  (defcap UPDATE_SUPPLY (id:string)
    @doc "Internal capability to allow updating the supply field of a token \
        \ Can only be acquired by composing (by MINT or BURN)"
    true)

  (defcap MINT (id:string account:string amount:decimal)
    @doc "Managed capability which must be installed externally to allow minting"
    @event
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY id))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @doc "Managed capability which must be installed externally to allow minting"
    @managed
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY id))
  )

  (defcap TRANSFER:bool (id:string sender:string receiver:string amount:decimal)
    @doc "Managed capability which must be installed externally to allow transfers"
    @managed amount TRANSFER-mgr
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defcap XTRANSFER:bool (id:string sender:string receiver:string target-chain:string amount:decimal)
    @doc "Not used, since X-chain transfers managed my the ledger are not supported"
    @managed amount TRANSFER-mgr
    (enforce false "cross chain not supported")
  )

  (defcap SALE:bool (id:string seller:string amount:decimal timeout:time sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (compose-capability (OFFER id seller amount))
  )

  (defcap OFFER:bool (id:string seller:string amount:decimal)
    @doc "Managed cap for SELLER offering AMOUNT of token ID"
    @managed
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (escrow)))
  )

  (defcap WITHDRAW:bool (id:string seller:string amount:decimal)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID"
    (compose-capability (ESCROW))
    (compose-capability (DEBIT id (escrow)))
    (compose-capability (CREDIT id seller))
  )

  (defcap BUY:bool (id:string seller:string buyer:string amount:decimal)
    @doc "Completes sale OFFER to BUYER."
    (compose-capability (ESCROW))
    (compose-capability (DEBIT id (escrow)))
    (compose-capability (CREDIT id buyer))
  )

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
               (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  ;-----------------------------------------------------------------------------
  ; Token IDs management and reservation
  ;-----------------------------------------------------------------------------
  (defcap ENFORCE-RESERVED ()
    @doc "Capability to scope the signature for token-id reservation"
    true)

  (defun create-token-id:string (creation-guard:guard token-uri:string)
    @doc "Create a token ID based on the creation guard and the URI"
    (+ "t:" (hash {'u:token-uri, 'g:creation-guard}))
  )

  (defun enforce-token-reserved:bool (id:string token-uri:string creation-guard:guard)
    @doc "Enforce reserved id name protocols."
    ; Enforce the creation guard
    (with-capability (ENFORCE-RESERVED)
      (enforce-guard creation-guard))

    ; Check that the token name starts with "t:", recomputes the token-id by
    ; ourselves and verify that it does match with one one given by the creator
    (if (starts-with id "t:")
        (enforce (= id (create-token-id creation-guard token-uri)) "Token protocol violation")
        (enforce false "Unrecognized reserved protocol"))
  )

  ;-----------------------------------------------------------------------------
  ; Tokens management
  ;-----------------------------------------------------------------------------
  (defun get-token-info:object{token-info} (id:string)
    @doc "Return the token-infos (policies) object of a token"
    (with-read tokens id {'uri:=uri, 'supply:=supply, 'precision:=precision}
      {'id:id, 'supply:supply, 'precision:precision, 'uri:uri}))

  (defun get-policies:[module{token-policy-ng-v1}] (id:string)
    @doc "Returns the list of policies of a token"
    (with-read tokens id {'policies:=x}
      x))

  (defun get-uri:string (id:string)
    @doc "Return the URI associated with a token"
    (with-read tokens id {'uri:=x}
      x))

  (defun precision:integer (id:string)
    @doc "Return the precision of a token"
    (with-read tokens id {'precision:=p}
      p))

  (defun enforce-unit:bool (id:string amount:decimal)
    @doc "Enforce that the amount does not violate the maximum precision of a token"
    (enforce-precision (precision id) amount))

  (defun truncate:decimal (id:string amount:decimal)
    @doc "Floor an amount to the precision of the given token"
    (floor amount (precision id)))

  (defun total-supply:decimal (id:string)
    @doc "Return the total precision of poly-fungible token"
    (with-default-read tokens id {'supply:0.0} {'supply:= s}
      s))

  (defun token-exist:bool (id:string)
    @doc "Return true if the token exists"
    (with-default-read tokens id {'id:""} {'id:=_id}
      (= id _id)))

  (defun update-supply:bool (id:string amount:decimal)
    @doc "Update the total supply of a given token \
       \  This function is intended to ba called internally since the \
       \  private cap UPDATE_SUPPLY must have been acquired"
    (require-capability (UPDATE_SUPPLY id))
    (with-default-read tokens id {'supply: 0.0}
                                 {'supply:=s}
      (let ((new-supply (+ s amount)))
        (update tokens id {'supply: new-supply})
        (emit-event (SUPPLY id new-supply)))))

  (defun list-holders:[object] (token-id:string)
    @doc "Return the lists of accounts holding the token id \
        \ Warning this function must be called in LOCAL ONLY"
    (select ledger ["account", "balance"] (and? (where 'id (= token-id))
                                           (where 'balance (< 0.0))))
  )

  ;-----------------------------------------------------------------------------
  ; Accounts management
  ;-----------------------------------------------------------------------------
  (defun key:string (id:string account:string)
    @doc "DB key for ledger account"
    (format "{}:{}" [id account]))

  (defun create-account:bool (id:string account:string guard:guard)
    @doc "Create an account for a given token"
    (enforce-reserved account guard)
    (enforce-valid-account account)
    (let ((exist (token-exist id)))
      (enforce exist "Token id does not exist"))
    (insert ledger (key id account) {'balance: 0.0,
                                     'guard:guard,
                                     'id:id,
                                     'account:account})
    true
  )

  (defun get-balance:decimal (id:string account:string)
    @doc "Return the balance of an account for the givent id"
    (with-default-read ledger (key id account) {'balance:0.0} {'balance:=b}
      b))

  (defun account-guard:guard (id:string account:string)
    @doc "Return the guard of an account for the given id"
    (with-read ledger (key id account) {'guard:=g}
      g))

  (defun account-exist:bool (id:string account:string)
    @doc "Return true is the account has already been created in the ledger"
    (with-default-read ledger (key id account) {'balance:-1.0} {'balance:=b}
      (!= b -1.0)))

  (defun details:object{account-details} (id:string account:string)
    @doc "Return the detail of an account for the given id"
    (read ledger (key id account)))

  (defun list-balances:[object] (account:string)
    @doc "Return the lists tokens owned by an account \
        \ Warning this function must be called in LOCAL ONLY"
    (select ledger ["id", "balance"] (and? (where 'account (= account))
                                           (where 'balance (< 0.0))))
  )

  ;-----------------------------------------------------------------------------
  ; Public Marmalade functions => Create Token
  ;-----------------------------------------------------------------------------
  (defcap POLICY-ENFORCE-INIT (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun sort-policies:[module{token-policy-ng-v1}] (in:[module{token-policy-ng-v1}])
    @doc "Technical function to deduplicate, and sort policies by rank"
    ; Processing:
    ;  - Remove duplicates (distinct)
    ;    |-> Convert to object list {'p:policy, 'r:rank}
    ;         |-> Sort by 'r
    ;             |-> Extract 'p from the object
    (let ((zip-rank (lambda (pol:module{token-policy-ng-v1}) {'r:(pol::rank), 'p:pol})))
      (map (at 'p)
           (sort ['r, 'p]
                 (map (zip-rank)
                      (distinct in)))))
  )


  (defun create-token:bool (id:string precision:integer uri:string
                            policies:[module{token-policy-ng-v1}]
                            creation-guard:guard)
    @doc "Create a token with a given token-id"
    ; First we verify that the token-id is correct, regarding the uti and the
    ; creation gyard
    (enforce-token-reserved id uri creation-guard)
    (let* ((_policies (sort-policies policies))
           (token-info {'id:id, 'uri:uri, 'precision:precision, 'supply:0.0})
           (call-policy (lambda (m:module{token-policy-ng-v1})
                                (with-capability (POLICY-ENFORCE-INIT token-info m)
                                  (m::enforce-init token-info)))))
      ; Call the creation policies
      (map (call-policy) _policies)
      ; Insert the token into the database
      (insert tokens id {'id: id,
                         'uri: uri,
                         'precision: precision,
                         'supply: 0.0,
                         'policies: _policies})
      ; And emit the corresponding event
      (emit-event (TOKEN-CREATE id uri precision (map (to-string) _policies))))
  )

  ;-----------------------------------------------------------------------------
  ; Public Marmalade functions => Transfer Token
  ;-----------------------------------------------------------------------------
  (defcap POLICY-ENFORCE-TRANSFER (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun transfer:bool (id:string sender:string receiver:string amount:decimal)
    @doc "Transfer a token amount from a sender to a receiver"
    ; We read first the current guard and then delegates evrything to transfer-create
    (with-read ledger (key id receiver) {'guard:=g}
      (transfer-create id sender receiver g amount)))

  (defun transfer-create:bool (id:string sender:string receiver:string receiver-guard:guard amount:decimal)
  @doc "Transfer a token amount from a sender to a receiver"
    ; Check that both accounts and the amount are valid.
    (enforce-valid-transfer sender receiver (precision id) amount)

    (let* ((policies (get-policies id))
           (token-info (get-token-info id))
           (call-policy (lambda (m:module{token-policy-ng-v1})
                                (with-capability (POLICY-ENFORCE-TRANSFER token-info m)
                                  (m::enforce-transfer token-info sender receiver amount)))))
      ; Call the policies
      (map (call-policy) policies))

    ; Do the transfer by debiting the sender and crediting the receiver
    (with-capability (TRANSFER id sender receiver amount)
      (let ((s-bal-change (debit id sender amount))
            (r-bal-change (credit id receiver receiver-guard amount)))
        ; Emit the RECONCILE event
        (emit-event (RECONCILE id amount s-bal-change r-bal-change))))
  )

  (defpact transfer-crosschain:bool (id:string sender:string receiver:string receiver-guard:guard
                                     target-chain:string amount:decimal)
    @doc "Not used, since X-chain transfers managed my the ledger are not supported"
    (step (enforce false "cross chain not supported")))

  ;-----------------------------------------------------------------------------
  ; Public Marmalade functions => Mint Token
  ;-----------------------------------------------------------------------------
  (defcap POLICY-ENFORCE-MINT (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun mint:bool (id:string account:string guard:guard amount:decimal)
    ; Check that the account is valid
    (enforce-valid-account account)
    ; Check that the amount is positive and check the decimals
    (enforce-valid-amount (precision id) amount)

    (let* ((policies (get-policies id))
           (token-info (get-token-info id))
           (call-policy (lambda (m:module{token-policy-ng-v1})
                                (with-capability (POLICY-ENFORCE-MINT token-info m)
                                  (m::enforce-mint token-info account amount)))))
      ; Call the policies
      (map (call-policy) policies))

      (with-capability (MINT id account amount)
        ; Update the supply
        (update-supply id amount)
        ; And emit the RECONCILE event
        (let ((bal-change (credit id account guard amount)))
          (emit-event (RECONCILE id amount NO-BALANCE-CHANGE bal-change))))
  )

  ;-----------------------------------------------------------------------------
  ; Public Marmalade functions => Burn Token
  ;-----------------------------------------------------------------------------
  (defcap POLICY-ENFORCE-BURN (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun burn:bool (id:string account:string amount:decimal)
    ; Check that the amount is positive and check the decimals
    (enforce-valid-amount (precision id) amount)

    (let* ((policies (get-policies id))
           (token-info (get-token-info id))
           (call-policy (lambda (m:module{token-policy-ng-v1})
                                (with-capability (POLICY-ENFORCE-BURN token-info m)
                                  (m::enforce-burn token-info account amount)))))
      ; Call the policies
      (map (call-policy) policies))

      (with-capability (BURN id account amount)
        ; Update the supply
        (update-supply id (- amount))
        ; And emit the RECONCILE event
        (let ((bal-change (debit id account amount)))
          (emit-event (RECONCILE id amount bal-change NO-BALANCE-CHANGE))))
  )

  ;-----------------------------------------------------------------------------
  ; Escrow accounts management
  ;-----------------------------------------------------------------------------

  ; Note that the escrow account is guarded by a capabilty-pact-guard.
  ; This capability can only be accessed with a specific defpact; and thus
  ; parameterizing the capability is not necessary
  (defcap ESCROW ()
    true)

  (defun escrow-guard:guard ()
    @doc "Return the guard of the escrow account"
    (create-capability-pact-guard (ESCROW)))

  (defun escrow:string ()
    @doc "Return the escrow account"
    (create-principal (escrow-guard)))

  ;-----------------------------------------------------------------------------
  ; Public Marmalade functions => Sale
  ;-----------------------------------------------------------------------------
  (defcap POLICY-ENFORCE-OFFER (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defcap POLICY-ENFORCE-WITHDRAW (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defcap POLICY-ENFORCE-BUY (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defcap POLICY-ENFORCE-SETTLE (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defpact sale:bool (id:string seller:string amount:decimal timeout:time)
    (step-with-rollback
      ;; Step 0: offer
      ;; -------------
      (let* ((policies (get-policies id))
             (token-info (get-token-info id))
             (call-policy (lambda (m:module{token-policy-ng-v1})
                                  (with-capability (POLICY-ENFORCE-OFFER token-info (pact-id) m)
                                    (m::enforce-sale-offer token-info seller amount timeout)))))

        ; We doesn't allow nested pacts =>  A parent Pact could manage a "side-contract" allowing
        ; the buyer and the seller to bypass fees and royalties
        ; In a nested pact, the pact-id is different than the Tx Hash
        (enforce (= (tx-hash) (pact-id)) "Nested pact not allowed")

        ; Check that the amount is positive and check the decimals
        (enforce-valid-amount (precision id) amount)

        ; Check that the account is valid
        (enforce-valid-account seller)

        ; Check that the timeout is NO-TIMEOUT or in the future
        ; A policy may do additional checks on this timeout
        (enforce (or? (time-between (now) (from-now MAXIMUM-TIMEOUT))
                      (= NO-TIMEOUT) timeout) "Invalid timeout")

        ; Call the policies => All the returns values are ORed using fold.
        ; This ensures that at least one policy has handled the sale.
        (let ((offer-handled (fold (or) false (map (call-policy) policies))))
          (enforce offer-handled "No policy to handle the offer"))

        ; Transfer the token to the escrow account
        (with-capability (SALE id seller amount timeout (pact-id))
          (let ((snd-bal (debit id seller amount))
                (rcv-bal (credit id (escrow) (escrow-guard) amount)))
            (emit-event (RECONCILE id amount snd-bal rcv-bal)))))
        ; The first part of the PACT is finished

      ;;Step 0: rollback
      ;; ---------------
      ; Withdraw: we have to refund the seller and give him back the token
      (let* ((policies (get-policies id))
             (token-info (get-token-info id))
             (call-policy (lambda (m:module{token-policy-ng-v1})
                                  (with-capability (POLICY-ENFORCE-WITHDRAW token-info (pact-id) m)
                                    (m::enforce-sale-withdraw token-info)))))
        ; Call the policies
        (map (call-policy) policies)

        ; And transfer back the token to the seller
        (with-capability (WITHDRAW id seller amount)
          (let ((snd-bal (debit id (escrow) amount))
                (rcv-bal (credit id seller (account-guard id seller) amount)))
            (emit-event (RECONCILE id amount snd-bal rcv-bal)))))
        ; The PACT is definitvely closed
    )
    (step
      ;; Step 1: buy and settle payments
      ;; -------------------------------
      (let* ((policies (get-policies id))
             (token-info (get-token-info id))
             (buyer:string (read-string "buyer"))
             (buyer-guard:guard (read-msg "buyer-guard"))
             (call-buy    (lambda (m:module{token-policy-ng-v1})
                                  (with-capability (POLICY-ENFORCE-BUY token-info (pact-id) m)
                                                   (m::enforce-sale-buy token-info buyer))))
             (call-settle (lambda (m:module{token-policy-ng-v1})
                                  (with-capability (POLICY-ENFORCE-SETTLE token-info (pact-id) m)
                                                   (m::enforce-sale-settle token-info)))))

            ; Check the validity of the buyer account
            (enforce-valid-account buyer)

            ; Call the sale-buy hooks of the policies
            ; The policies must check here that the buyer is OK and the sale
            ; ready to be settled
            (map (call-buy) policies)

            ; Transfer the token from the ecrow to the buyer;
            ; Here the BUY capability allows to access to the escrow account
            (with-capability (BUY id seller buyer amount)
              (let ((snd-bal (debit id (escrow) amount))
                    (rcv-bal (credit id buyer buyer-guard amount)))
                (emit-event (RECONCILE id amount snd-bal rcv-bal))))

            ; Now call the sale-setlle hooks of the policy to finish the job
            ; The capability ESCROW allow us to access the escrow account.
            (with-capability (ESCROW)
              (map (call-settle) policies))
            true)
      )
  )

  ;-----------------------------------------------------------------------------
  ; Private ledger functions
  ;-----------------------------------------------------------------------------
  (defun debit:object{sender-balance-change} (id:string account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"
    (require-capability (DEBIT id account))
    (with-read ledger (key id account)
                      {'balance:= old-bal}
      (enforce (<= amount old-bal) "Insufficient funds")

      (let ((new-bal (- old-bal amount)))
        (update ledger (key id account)  {'balance: new-bal})
        {'account: account, 'previous: old-bal, 'current: new-bal}))
  )

  (defun credit:object{receiver-balance-change} (id:string account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance"
    (enforce-reserved account guard)
    (require-capability (CREDIT id account))
    (with-default-read ledger (key id account) {'balance:0.0, 'guard:guard}
                                               {'balance:=old-bal, 'guard:=current-guard}
      (enforce (= guard current-guard) "Existing guard doesn't match")
      (let ((new-bal (+ old-bal amount)))
        (write ledger (key id account) {'balance:new-bal,
                                        'guard:guard,
                                        'id:id,
                                        'account:account})
        {'account: account, 'previous: old-bal, 'current: new-bal}))
  )

  ;; Note regarding accounts rotation
  ;;---------------------------------
  ; Guard rotation is intentionally not possible.
  ; Some fetatures in policies rely on the accounts immutability.
  ; Please do not try to restore a rotate function.
)
