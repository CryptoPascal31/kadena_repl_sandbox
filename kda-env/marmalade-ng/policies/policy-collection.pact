(module policy-collection GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use free.util-math [++])
  (use util-policies)

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
    creator-guard:guard
  )

  (deftable collections:{collection-sch})

  (defschema token-collection-sch
    token-id:string
    collection-id:string
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
    (with-read collections collection-id {'creator-guard:=cg}
      (enforce-guard cg))
  true)

  (defcap CREATE-COLLECTION (collection-id:string collection-name:string collection-size:integer)
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; Collection creation
  ;-----------------------------------------------------------------------------
  (defun create-collection-id:string (name:string creator-guard:guard)
    (format "c_{}_{}" [name, (hash {'n:name, 'g:creator-guard})]))

  (defun create-collection:bool (id:string name:string size:integer creator-guard:guard)
    (enforce (>= size 0) "Collection size must be positive")
    (enforce-guard creator-guard)
    (enforce (= id (create-collection-id name creator-guard)) "Collection ID does not match")
    (insert collections id {'id:id,
                            'name:name,
                            'max-size:size,
                            'size:0,
                            'creator-guard:creator-guard})

    (emit-event (CREATE-COLLECTION id name size))
  )

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer () 0)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-collection))

    (let* ((token-id:string  (at 'id token))
           (data:object{collection-msg-sch} (enforce-get-msg-data "collection" token))
           (collection-id:string (at 'id data)))
      (with-capability (ADD-TO-COLLECTION collection-id token-id)
        (with-read collections collection-id {'max-size:=max-size,
                                              'size:=current-size}
                                              ;'creator-guard:=cg}
          ;(enforce-guard cg)
          ; max-size=0 means unlimited collection
          (enforce (or? (= 0) (< current-size) max-size) "Exceeds collection size")

          (update collections collection-id {'size:(++ current-size)}))
        (insert tokens token-id {'token-id:token-id, 'collection-id:collection-id})
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
  (defun get-collection:object{collection-sch} (collection-id:string )
    (read collections collection-id))

  (defun get-all-collections:[string] ()
    (keys collections))

  (defun get-token-collection:object{collection-sch} (token-id:string)
    (with-read tokens token-id {'collection-id:=collection-id}
      (get-collection collection-id)))

  (defun list-tokens-of-collection:[string] (collection-id:string)
    (map (at 'token-id)
         (select tokens ['token-id]  (where 'collection-id (= collection-id)))))

)
