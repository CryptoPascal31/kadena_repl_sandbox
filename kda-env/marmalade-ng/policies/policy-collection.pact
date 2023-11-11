(module policy-collection GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use free.util-math [++])
  (use free.util-fungible [enforce-reserved])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  (defschema collection-sch
    id:string
    name:string
    size:integer
    max-size:integer
    creator:string
    creator-guard:guard
  )

  (deftable collections:{collection-sch})

  (defschema token-collection-sch
    token-id:string
    collection-id:string
    rank:integer
  )

  (deftable tokens:{token-collection-sch})

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema collection-msg-sch
    id:string
  )

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------
  (defcap ADD-TO-COLLECTION (collection-id:string token-id:string)
    @doc "Capability to grant creation of a collection's token"
    @event
    (with-read collections collection-id {'creator-guard:=cg}
      (enforce-guard cg))
  true)

  (defcap CREATE-COLLECTION (collection-id:string collection-name:string collection-size:integer creator:string )
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Constants
  ;-----------------------------------------------------------------------------
  ; Maximum size of collections allowed by the contract
  (defconst MAXIMUM-SIZE:integer 10000000)

  ; Special size value to indicate that the collection can have an unlimited count
  ; of tokens.
  (defconst UNLIMITED-SIZE:integer 0)

  ;-----------------------------------------------------------------------------
  ; Collection creation
  ;-----------------------------------------------------------------------------
  (defun create-collection-id:string (name:string creator-guard:guard)
    (format "c_{}_{}" [name, (hash {'n:name, 'g:creator-guard})]))

  (defun create-collection:bool (id:string name:string size:integer creator:string creator-guard:guard)
    (enforce (or? (= UNLIMITED-SIZE)
                  (and? (< 0) (>= MAXIMUM-SIZE)) size)
             (format "Collection size must be positive and less than {}" [MAXIMUM-SIZE]))
    ; Validate the creator name if it's a principal
    (enforce-reserved creator creator-guard)
    ; Verify the creator guard / signature
    (enforce-guard creator-guard)

    (enforce (= id (create-collection-id name creator-guard)) "Collection ID does not match")
    (insert collections id {'id:id,
                            'name:name,
                            'max-size:size,
                            'size:0,
                            'creator:creator,
                            'creator-guard:creator-guard})

    (emit-event (CREATE-COLLECTION id name size creator))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-collection))

    (let* ((token-id:string  (at 'id token))
           (data:object{collection-msg-sch} (enforce-get-msg-data "collection" token))
           (collection-id:string (at 'id data)))
      (with-capability (ADD-TO-COLLECTION collection-id token-id)
        (with-read collections collection-id {'max-size:=max-size,
                                              'size:=current-size}
          ; Check that either:
          ;   - The collection is unlimited
          ;   - The current count of collections is less than the maximum size
          (enforce (or? (= UNLIMITED-SIZE) (< current-size) max-size) "Exceeds collection size")

          (update collections collection-id {'size:(++ current-size)})
          (insert tokens token-id {'token-id:token-id,
                                   'collection-id:collection-id,
                                   'rank: (++ current-size)}))
        true))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    false)

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-collection:object{collection-sch} (collection-id:string)
    @doc "Return the details of a given collection"
    (read collections collection-id))

  (defun get-token-collection:object{collection-sch} (token-id:string)
    @doc "Return the collection details of given token"
    (with-read tokens token-id {'collection-id:=collection-id}
      (get-collection collection-id)))

  (defun get-token-rank-in-collection:integer (token-id:string)
    @doc "Return the collection rank of a collection"
    (with-read tokens token-id {'rank:=r}
      r))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun get-all-collections:[string] ()
    @doc "Return the list of all collections"
    (keys collections))

  (defun get-collections-by-creator:[object{collection-sch}] (creator:string)
      @doc "Return the list of all collection objects owned by a creator"
      (select collections (where 'creator (= creator))))

  (defun list-tokens-of-collection:[string] (collection-id:string)
    @doc "Return the list of tokens that belong to a given collection"
    (map (at 'token-id)
         (sort ['rank]
               (select tokens ['token-id, 'rank]  (where 'collection-id (= collection-id)))))
  )

  (defun list-tokens-of-collections:[string] (collections-ids:[string])
    @doc "Return the list of tokens that belong to a list of collections"
    (let ((is-in-list (lambda (x:string) (contains x collections-ids))))
      (map (at 'token-id)
           (select tokens ['token-id]
                   (where 'collection-id (is-in-list)))))
  )
)
