(defun fund-account (account-name:string key:string amount:decimal)
  "Fund a coin account from nothing"
  (env-data { "k": [key]})
    (let ((ks:guard (read-keyset 'k)))
      (coin.create-account account-name ks)
      (test-capability (coin.CREDIT account-name))
      (coin.credit account-name ks amount))
)

(defun fund-accounts (account-names:[string] amount:decimal)
  "Fund a list of coin accounts with a constant amount. the key is derived from the account name"
  (map (lambda (x) (fund-account x (+ x "-key") amount)) account-names)
)
