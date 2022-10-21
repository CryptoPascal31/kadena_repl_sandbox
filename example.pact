(module hello GOV

  (defcap GOV()
    true)

  (defun say-hello(name:string)
    (let ((bal (coin.get-balance name)))
      (format "Hello {}: You have currently {} KDA"
              [name bal])))
)
