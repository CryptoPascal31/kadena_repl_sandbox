(module util-policies GOVERNANCE
  (use token-policy-ng-v1 [token-info])
  (use free.util-fungible [enforce-valid-account])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Read messages from data
  ;-----------------------------------------------------------------------------

  ; Encoding = All messages used by policies are typed objects
  ;
  ; Their key in the data section can be either:
  ;   - marmalade_$domain$_$token_id$ (Syntax 1)
  ;   - marmalade_$domain$ (Syntax 2 = Global Failback)
  ;
  ;
  ; Where $domain^ is specific to the policy
  ;
  ; In case a signle transaction would work with several tokens, it's possible to
  ;   - use a global messages for all tokens (Syntax 1)
  ;   - use a per token message (Syntax 2)
  (defun global-key:string (domain:string)
    (concat ["marmalade" "_" domain]))

  (defun token-key:string (domain:string token:object{token-info})
    (concat [(global-key domain) "_" (at 'id token)]))

  (defun get-msg-data:object (domain:string token:object{token-info} default:object)
    @doc "Read an optionnal message from data, trying to read \
         \  - per-token message                               \
         \  - then failback to a global messgae               \
         \  - and if nothing availaible returns the default as a failback"
    (let ((token-msg-data:object (try default (read-msg (token-key domain token)))))
      (if (= token-msg-data default)
          (try default (read-msg (global-key domain)))
          token-msg-data))
  )

  (defun enforce-get-msg-data:object (domain:string token:object{token-info})
    @doc "Read a mandatory message from data => Fail the transaction if not present"
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
    @doc "Check an account against a sepcific currency: validity of the name + existance"
    (enforce-valid-account acct)
    (let ((bal (try -1.0 (currency::get-balance acct))))
      (enforce (>= bal 0.0)
               (format "Account {} does not exist" [acct])))
  )

  (defun check-price:bool (currency:module{fungible-v2} price:decimal)
    (enforce (> price 0.0) "price must be positive")
    (currency::enforce-unit price)
  )


)
