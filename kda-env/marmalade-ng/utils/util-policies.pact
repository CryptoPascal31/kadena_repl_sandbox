(module util-policies GOVERNANCE
  (use token-policy-ng-v1 [token-info])
  (use free.util-math [xEy between])
  (use free.util-fungible [enforce-valid-account])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Constants
  ;-----------------------------------------------------------------------------
  (defconst RANK-HIGH-PRIORITY:integer 0)

  (defconst RANK-SELLER-FEE:integer 10)

  (defconst RANK-ROYALTY:integer 20)

  (defconst RANK-SALE:integer 30)

  (defconst RANK-LOW-PRIORITY:integer 99)

  ;-----------------------------------------------------------------------------
  ; Read messages from data
  ;-----------------------------------------------------------------------------

  ; Encoding = All messages used by policies are typed objects.
  ;
  ; Their key in the data section can be either:
  ;   - marmalade_$domain$_$token_id$ (Syntax 1)
  ;   - marmalade_$domain$ (Syntax 2 = Global Fallback)
  ;
  ;
  ; Where $domain$ is specific to the policy
  ;
  ; In case of a single transaction handling several tokens, it's possible to
  ;   - use a global message for all tokens (Syntax 1)
  ;   - use a per-token message (Syntax 2)
  (defun global-key:string (domain:string)
    (concat ["marmalade" "_" domain]))

  (defun token-key:string (domain:string token:object{token-info})
    (concat [(global-key domain) "_" (at 'id token)]))

  (defun get-msg-data:object (domain:string token:object{token-info} default:object)
    @doc "Read an optional message from data, trying to read \
         \  - per-token message                               \
         \  - then fallback to a global message               \
         \  - and if nothing is available, returns the default as a fallback"
    (let ((token-msg-data:object (try default (read-msg (token-key domain token)))))
      (if (= token-msg-data default)
          (try default (read-msg (global-key domain)))
          token-msg-data))
  )

  (defun enforce-get-msg-data:object (domain:string token:object{token-info})
    @doc "Read a mandatory message from data => Make the transaction fail if not present"
    (let ((msg-data (get-msg-data domain token {})))
      (enforce (!= msg-data {}) (format "{} not present in data" [domain]))
      msg-data)
  )

  ;-----------------------------------------------------------------------------
  ; Sales common messages
  ;-----------------------------------------------------------------------------
  (defschema sale-msg-sch
    sale_type:string ; Type of sale
    currency:module{fungible-v2} ; Currency of sale
  )

  (defconst DEFAULT-SALE-MSG:object{sale-msg-sch} {'sale_type:"", 'currency:coin})

  (defun read-sale-msg:object{sale-msg-sch} (token:object{token-info})
    (get-msg-data "sale" token DEFAULT-SALE-MSG))

  (defun enforce-read-sale-msg:object{sale-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "sale" token))

  ;-----------------------------------------------------------------------------
  ; Sales common utils
  ;-----------------------------------------------------------------------------
  (defun check-fungible-account:bool (currency:module{fungible-v2} acct:string)
    @doc "Check an account against a specific currency: validity of the name + existence"
    (enforce-valid-account acct)
    (let ((bal (try -1.0 (currency::get-balance acct))))
      (enforce (>= bal 0.0)
               (format "Account {} does not exist" [acct])))
  )

  ; Price is limited to 1.10^12
  (defconst MAXIMUM-PRICE:decimal (xEy 1.0 12))

  (defun check-price:bool (currency:module{fungible-v2} price:decimal)
    (enforce (and? (< 0.0) (>= MAXIMUM-PRICE) price) "Price out of range")
    (currency::enforce-unit price)
  )

  (defun enforce-valid-rate:bool (rate:decimal)
    @doc "Enforce that a rate is between 0.0 and 1.0"
    (enforce (between 0.0 1.0 rate) "Rate must be between 0.0 and 1.0")
  )
)
