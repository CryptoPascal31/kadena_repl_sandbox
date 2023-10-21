(module policy-extra-policies GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use free.util-lists)

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))

  (defconst EXTRA-POLICIES-KEYSET:string (read-string "extra_policies_admin_keyset"))

  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  (defcap EXTRA-POLICIES-GOVERNANCE ()
    (enforce-keyset EXTRA-POLICIES-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Capabilities and events
  ;-----------------------------------------------------------------------------
  (defcap UPDATE-EXTRA-POLICIES (token-id:string)
    @doc "Capability to update the guard or the blacklist of a token"
    @event
    (with-read tokens token-id {'guard:=g}
      (enforce-guard g))
  )

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema extra-policies-sch
    policies: [module{token-policy-ng-v1}]
  )

  (defschema extra-policies-token-sch
    token-id: string
    guard: guard
    blacklist: [module{token-policy-ng-v1}]
  )

  (defschema extra-policies-sale-sch
    blacklist: [module{token-policy-ng-v1}]
  )

  (deftable global:{extra-policies-sch})
  (deftable tokens:{extra-policies-token-sch})
  (deftable sales:{extra-policies-sale-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema extra-policies-msg-sch
    guard:guard ; Owned by the creator: used to protect blacklists
  )

  (defun read-extra-policies-msg:object{extra-policies-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "extra_policies" token))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun filter-from-list:[module{token-policy-ng-v1}] (in to-remove)
    @doc "Utility function to return elements of in, not present in to-remove"
    (if (is-empty to-remove) in
        (filter (compose (contains* to-remove) (not)) in))
  )


  (defun policies-list-for-token-id:[module{token-policy-ng-v1}]  (token-id:string)
    @doc "Return the list of policies associted to a given token-id \
      \ Global policies MINUS blacklisted policies"
    ; If the token is not registered (token doesn't implement extra-polices) we don't
    ;  throw an error but returns an empty list. ; This should simplify the processing in
    ;  the backends
    ; If the token is registered, we read the global list and the blacklist.
    ;  we return the difference between the two lists.
    (with-default-read tokens token-id {'token-id:"", 'blacklist:[]}
                                       {'token-id:=tid, 'blacklist:=blacklist}
      (if (= tid "") []
          (with-read global "" {'policies:=pols-list}
            (filter-from-list pols-list blacklist))))
  )

  (defun policies-list-for-token:[module{token-policy-ng-v1}] (token:object{token-info})
    @doc "Return the list of policies associted to a given token \
        \ Global policies  - blacklisted policies"
    (policies-list-for-token-id (at 'id token)))

  (defun policies-list-for-sale:[module{token-policy-ng-v1}] ()
    @doc "Return the list of policies associted to the current sale \
        \ Global policies  - blacklisted policies"
    (with-read global "" {'policies:=pols-list}
      (with-default-read sales (pact-id) {'blacklist:[]} {'blacklist:=blacklist}
        (filter-from-list pols-list blacklist)))
  )

  (defun get-guard-by-id:guard (token-id:string)
    @doc "Return the guard assiocated to a token-id"
    (with-read tokens token-id {'guard:=g}
      g))

  (defun get-guard:guard (token:object{token-info})
    @doc "Return the guard assiocated to a token"
    (get-guard-by-id (at 'id token)))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;---------------------------------------------------------------------------
  (defun rank:integer ()
    (/ (+ RANK-ROYALTY RANK-SALE) 2))

  (defun enforce-init:bool (token:object{token-info})
    ; Since the extra-policy can be added after init, there is no sense of calling
    ; init hooks; but we will store the guard for further blacklisting.
    (let ((token-id (at 'id token))
          (data (read-extra-policies-msg token)))
      (insert tokens token-id {'token-id: token-id, 'guard: (at 'guard data), 'blacklist:[]})
      true)
  )

  (defcap POLICY-ENFORCE-MINT (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-MINT token p) (p::enforce-mint token account amount)))
         (policies-list-for-token token))
    true
  )

  (defcap POLICY-ENFORCE-BURN (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-BURN token policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-BURN token p) (p::enforce-burn token account amount)))
         (policies-list-for-token token))
    true
  )

  (defcap POLICY-ENFORCE-TRANSFER (token:object{token-info} mod:module{token-policy-ng-v1})
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-TRANSFER token p) (p::enforce-transfer token sender receiver amount)))
         (policies-list-for-token token))
    true
  )

  (defcap POLICY-ENFORCE-OFFER (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-extra-policies))
    (let ((policies-lst (policies-list-for-token token)))
      ; We want to save the policies blacklist to prevent the creator to add a new one during a sale
      ;   During the subsequent steps of the sales, the saved policy list will be read by
      ;   (policies-list-for-sale)
      ; To avoid an unneeded write, we only create a record is the blacklist is not empty
      (with-read tokens (at 'id token) {'blacklist:=blacklist}
        (if (is-empty blacklist) ""
            (insert sales (pact-id) {'blacklist:blacklist})))
      (fold (or) false (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-OFFER token (pact-id) p) (p::enforce-sale-offer token seller amount timeout)))
                            policies-lst)))
  )

  (defcap POLICY-ENFORCE-WITHDRAW (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-WITHDRAW token (pact-id) policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-WITHDRAW token (pact-id) p) (p::enforce-sale-withdraw token)))
         (policies-list-for-sale))
    true
  )

  (defcap POLICY-ENFORCE-BUY (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (ledger.POLICY-ENFORCE-BUY token (pact-id) policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-BUY token (pact-id) p) (p::enforce-sale-buy token buyer)))
         (policies-list-for-sale))
    true
  )

  (defcap POLICY-ENFORCE-SETTLE (token:object{token-info} sale-id:string mod:module{token-policy-ng-v1})
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-extra-policies))
    (map (lambda (p:module{token-policy-ng-v1}) (with-capability (POLICY-ENFORCE-SETTLE token (pact-id) p) (p::enforce-sale-settle token)))
         (policies-list-for-sale))
    true
  )

  ;-----------------------------------------------------------------------------
  ; User functions
  ;-----------------------------------------------------------------------------
  (defun add-to-blacklist:string (token-id:string policy:module{token-policy-ng-v1})
    @doc "Add a new policy to the blacklist for the given token-id"
    (with-capability (UPDATE-EXTRA-POLICIES token-id)
      (with-read tokens token-id {'guard:=token-guard, 'blacklist:=old-bl}
        (update tokens token-id {'blacklist: (distinct (append-last old-bl policy))})))
  )

  (defun remove-from-blacklist:string (token-id:string policy:module{token-policy-ng-v1})
    @doc "Remove of policy from the blacklist for the given token-id"
    (with-capability (UPDATE-EXTRA-POLICIES token-id)
      (with-read tokens token-id {'blacklist:=old-bl}
        (update tokens token-id {'blacklist: (remove-item old-bl policy)})))
  )

  (defun get-blacklist:[module{token-policy-ng-v1}] (token-id:string)
    @doc "Get the list blacklisted policies for the given token-id"
    (with-read tokens token-id {'blacklist:=bl}
      bl)
  )

  (defun rotate:string (token-id:string new-guard:guard)
    @doc "Rotate the extra policy guard for the given token-id"
    (with-capability (UPDATE-EXTRA-POLICIES token-id)
        (update tokens token-id {'guard:new-guard}))
  )

  ;-----------------------------------------------------------------------------
  ; Admin functions
  ;-----------------------------------------------------------------------------
  (defun init ()
    @doc "Init the module"
    (with-capability (GOVERNANCE)
      (insert global "" {'policies:[]}))
  )

  (defun register-policy:string (policy:module{token-policy-ng-v1})
    @doc "Admin function to register a policy to the extra-policies list"
    (with-capability  (EXTRA-POLICIES-GOVERNANCE)
      ; We use the ledger's sort function. No need to rewrite it here
      (with-read global "" {'policies:=old-pols}
        (update global "" {'policies:(ledger.sort-policies (append-last old-pols policy))})))
  )

  (defun unregister-policy:string (policy:module{token-policy-ng-v1})
    @doc "Admin function to remove a policy from the extra-policies list"
    (with-capability  (EXTRA-POLICIES-GOVERNANCE)
      (with-read global "" {'policies:=old-pols}
        (update global "" {'policies:(remove-item old-pols policy)})))
  )

  (defun list-registered-policies:[module{token-policy-ng-v1}] ()
    @doc "List all global policies"
    (with-read global "" {'policies:=pols}
      pols)
  )
)
