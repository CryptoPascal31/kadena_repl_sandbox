(interface ng-poly-fungible-v1

  ;-----------------------------------------------------------------------------
  ; Schemas used in the API
  ;-----------------------------------------------------------------------------
  (defschema account-details
    @doc "Account details: token ID, account name, balance, and guard."
    id:string
    account:string
    balance:decimal
    guard:guard
  )

  (defschema sender-balance-change
    @doc "For use in RECONCILE events"
    account:string
    previous:decimal
    current:decimal
  )

  (defschema receiver-balance-change
    @doc "For use in RECONCILE events"
    account:string
    previous:decimal
    current:decimal
  )

  ;-----------------------------------------------------------------------------
  ; Capabilities and events
  ;-----------------------------------------------------------------------------
  (defcap TRANSFER:bool (id:string sender:string receiver:string amount:decimal)
    @doc "Manage transferring AMOUNT of ID from SENDER to RECEIVER. \
        \ As event, also used to notify burn (with \"\" RECEIVER) \
        \ and create (with \"\" SENDER)."
    @managed amount TRANSFER-mgr
  )

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages TRANSFER cap AMOUNT where MANAGED is the installed quantity \
        \ and REQUESTED is the quantity attempting to be granted."
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc  "Emitted when SUPPLY is updated, if supported."
    @event
  )

  (defcap RECONCILE:bool (token-id:string amount:decimal sender:object{sender-balance-change}
                          receiver:object{receiver-balance-change})
    @doc "For accounting via events. \
        \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
        \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
    @event
  )

  ;-----------------------------------------------------------------------------
  ; Tokens management
  ;-----------------------------------------------------------------------------
  (defun precision:integer (id:string)
    @doc "Return maximum decimal precision for ID."
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    @doc "Enforce that AMOUNT meets minimum precision allowed for ID."
  )

  (defun total-supply:decimal (id:string)
    @doc "Give total available quantity of ID. If not supported, return 0."
  )

  (defun get-uri:string (id:string)
    @doc "Give uri for ID."
  )

  ;-----------------------------------------------------------------------------
  ; Accounts management
  ;-----------------------------------------------------------------------------
  (defun create-account:bool (id:string account:string guard:guard)
    @doc "Create ACCOUNT for ID with 0.0 balance, with GUARD controlling access."
  )

  (defun get-balance:decimal (id:string account:string)
    @doc "Get balance of ID for ACCOUNT. Fails if account does not exist."
  )

  (defun details:object{account-details} (id:string account:string)
    @doc "Get details of ACCOUNT under ID. Fails if account does not exist."
  )

  ;-----------------------------------------------------------------------------
  ; Mint / Burn
  ;-----------------------------------------------------------------------------
  (defun mint:bool (id:string account:string guard:guard amount:decimal)
    @doc "Mint AMOUNT of ID to ACCOUNT with GUARD."
  )

  (defun burn:bool (id:string account:string amount:decimal)
    @doc "Burn AMOUNT of ID from ACCOUNT."
  )

  ;-----------------------------------------------------------------------------
  ; Transfers
  ;-----------------------------------------------------------------------------
  (defun transfer:bool (id:string sender:string receiver:string amount:decimal)
    @doc "Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
        \ Fails if SENDER does not exist. Managed by TRANSFER."
  )

  (defun transfer-create:bool (id:string sender:string receiver:string
                               receiver-guard:guard amount:decimal)
    @doc "Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
        \ If RECEIVER exists, RECEIVER-GUARD must match existing guard; \
        \ if RECEIVER does not exist, account is created. \
        \ Managed by TRANSFER."
  )

  ;-----------------------------------------------------------------------------
  ; Sales
  ;-----------------------------------------------------------------------------
  (defcap SALE:bool (id:string seller:string amount:decimal timeout:time sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until time TIMEOUT."
    @event
  )

  (defpact sale:bool (id:string seller:string amount:decimal timeout:time)
    @doc "Offer->buy escrow pact of AMOUNT of token ID by SELLER with TIMEOUT in blocks. \
        \ Step 1 is offer with withdraw rollback after timeout. \
        \ Step 2 is buy, which completes using 'buyer' and 'buyer-guard' payload values."
  )
)
