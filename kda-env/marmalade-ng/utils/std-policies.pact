(module std-policies GOVERNANCE
  (use token-policy-ng-v1 [token-info])
  (use free.util-lists [first])
  (use free.util-strings [join split])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  (defschema entry
    name:string
    pol:module{token-policy-ng-v1}
  )

  (defconst POLICIES-LIST:[object{entry}] [{'name:"ADJUSTABLE-ROYALTY",'pol:policy-adjustable-royalty},
                                           {'name:"AUCTION-SALE",      'pol:policy-auction-sale},
                                           {'name:"BLACKLIST",         'pol:policy-blacklist},
                                           {'name:"COLLECTION",        'pol:policy-collection},
                                           {'name:"DISABLE-BURN",      'pol:policy-disable-burn},
                                           {'name:"DISABLE-TRANSFER",  'pol:policy-disable-transfer},
                                           {'name:"DISABLE-SALE",      'pol:policy-disable-sale},
                                           {'name:"DUTCH-AUCTION-SALE",'pol:policy-dutch-auction-sale},
                                           {'name:"EXTRA-POLICIES",    'pol:policy-extra-policies},
                                           {'name:"FIXED-SALE",        'pol:policy-fixed-sale},
                                           {'name:"FIXED-ISSUANCE",    'pol:policy-fixed-issuance},
                                           {'name:"GUARDS",            'pol:policy-guards},
                                           {'name:"INSTANT-MINT",      'pol:policy-instant-mint},
                                           {'name:"MARKETPLACE",       'pol:policy-marketplace},
                                           {'name:"NON-FUNGIBLE",      'pol:policy-non-fungible},
                                           {'name:"ROYALTY",           'pol:policy-royalty},
                                           {'name:"TRUSTED-CUSTODY",   'pol:policy-trusted-custody}])


  (defun default-name:string (pol:module{token-policy-ng-v1})
    (+ "UNKNOWN_" (take 16 (int-to-str 16 (str-to-int 64 (hash pol))))))

  (defun to-policy:module{token-policy-ng-v1} (name:string)
    @doc "Convert a name to a policy"
    (let ((match:[object{entry}] (filter (where 'name (= name)) POLICIES-LIST)))
      (enforce (!= 0 (length match)) (format "Policy not found: {}" [name]))
      (at 'pol (first match)))
  )

  (defun list-to-policies:[module{token-policy-ng-v1}] (names:[string])
    @doc "Convert a list of names to a list of policies"
    (map (to-policy) names))

  (defun to-policies:[module{token-policy-ng-v1}] (names:string)
    @doc "Convert a list of names concatened in a single string to a list of policies"
    (compose (split " ") (list-to-policies) names))

  (defun from-policy:string (mod:module{token-policy-ng-v1})
    @doc "Convert a policy to a string"
    (let ((match:[object{entry}] (filter (where 'pol (= mod)) POLICIES-LIST)))
      (if (!= 0 (length match))
          (at 'name (first match))
          (default-name mod)))
  )

  (defun policies-to-list:[string] (pols:[module{token-policy-ng-v1}])
    @doc "Convert a list of policies to a list of names"
    (map (from-policy) pols))

  (defun from-policies:string (pols:[module{token-policy-ng-v1}])
    @doc "Convert a list of policies to a concatened string"
    (compose (policies-to-list) (join " ") pols))
)
