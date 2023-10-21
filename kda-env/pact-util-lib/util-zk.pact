;SPDX-License-Identifier: MIT

; This modules provides some convenient Zero Knowledge function for Pact.
;
; Be aware that this module is only in Beta and hasn't been audited:
;    --> BE CAREFUL if a security enforcement depends on one of theses functions
;
;
; Feel free to reuse, comment, review, fork, propose PRs, submit bugs:
; https://github.com/CryptoPascal31/pact-util-lib

(module util-zk GOV
  "Module containing time utilities \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  ; References:
  ;  - https://www.zeroknowledgeblog.com/index.php/groth16
  ;  - https://github.com/kadena-io/pact/blob/master/tests/pact/pairing.repl
  ;  - https://github.com/iden3/snarkjs/blob/master/templates/verifier_groth16.sol.ejs

  (defconst VERSION:string "0.8")

  (use util-strings [split-chunks])
  (use util-lists [remove-first first])
  (use util-math [++])

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (defconst BN128-GROUP-MODULUS:integer 21888242871839275222246405745257275088548364400416034343698204186575808495617)

  (defconst GEN-POINT-G1:object{point-G1} {'x:1, 'y:2})

  (defconst NULL-POINT-G1:object{point-G1} {'x:0, 'y:0})

  (defconst GEN-POINT-G2:object{point-G2} {'x:[11559732032986387107991004021392285783925812861821192530917403151452391805634,
                                               10857046999023057135944570762232829481370756359578518086990519993285655852781],
                                           'y:[4082367875863433681332203403145435568316851327593401208105741076214120093531,
                                               8495653923123431417604973247489272438418190587263600148770280649306958101930]})
  ; Point in G1
  (defschema point-G1
    x:integer
    y:integer
  )

  ; Point in G2 (extended field)
  (defschema point-G2
      x:[integer]
      y:[integer]
  )

  ; Groph16 Proof (2 point in G1, and 1 point in G2)
  (defschema groth16-proof
    A:object{point-G1}
    B:object{point-G2}
    C:object{point-G1}
  )

  (defschema groth16-verify-key
    alpha:object{point-G1}
    beta:object{point-G2}
    gamma:object{point-G2}
    delta:object{point-G2}
    ic:[object{point-G1}]
  )

  (defun int256-to-b64  (x:integer)
    "Convert a 256 bits integer to base 64"
    (drop 4 (int-to-str 64 (| x (shift 1 272))))
  )

  (defun serialize-proof:string (proof:object{groth16-proof})
    "Serialiaze an object proof to its base64 representation (344 octets)"
    (bind proof {'A:=A, 'B:=B, 'C:=C}
      (concat (map (int256-to-b64) [ (at 'x A), (at 'y A),
                                     (at 0 (at 'x B)), (at 1 (at 'x B)),
                                     (at 0 (at 'y B)), (at 1 (at 'y B)),
                                     (at 'x C), (at 'y C)
                                    ])))
  )

  (defun deserialize-proof:object{groth16-proof} (proof-str:string)
    "Deserialize a base64 proof string to its object representation"
    (enforce (= 344 (length proof-str)) "The base64 proof must have 344 characters")
    (let ((proof-lst (map (str-to-int 64) (split-chunks 43 proof-str))))
      {'A: {'x:  (at 0 proof-lst),
            'y:  (at 1 proof-lst)},
       'B: {'x: [(at 2 proof-lst) (at 3 proof-lst)],
            'y: [(at 4 proof-lst) (at 5 proof-lst)]},
       'C: {'x:  (at 6 proof-lst),
            'y:  (at 7 proof-lst)}
      })
  )


  (defun neg-G1:object{point-G1} (in:object{point-G1})
    "Returns the negative of a point in G1"
    (bind in {'x := x, 'y := y}
      {'x:x, 'y: (- y)})
  )


  (defun verify-groth16-proof:bool (key:object{groth16-verify-key}
                                    pub-inputs:[integer]
                                    proof:object{groth16-proof})
    "Verify a groth16 proof against a list of public inputs and proof object"

    ; Check that all public inputs are in the group
    (enforce (fold (and) true
                   (map (> BN128-GROUP-MODULUS) pub-inputs))
             "Invalid public inputs")

    (bind key {'alpha:= alpha, 'beta:=beta, 'gamma:=gamma, 'delta:=delta, 'ic:=ic}
      (enforce (= (++ (length pub-inputs )) (length ic)) "Bad number of inputs")
      (bind proof {'A:=A, 'B:=B, 'C:=C}
        ; Compute The linear combinations of inputs and IC
        (let* ((vk_0 (point-add 'g1 NULL-POINT-G1 (first ic)))
               (vk_n (fold (point-add 'g1) vk_0 (zip (scalar-mult 'g1) (remove-first ic) pub-inputs))))
          (pairing-check [(neg-G1 A)  alpha  vk_n   C]
                         [B           beta   gamma  delta]))))
  )

)
