;; Emacs font-lock mode for the Sentient language
;; https://github.com/sentient-lang
;; Roberto DeFeo (rdefeo@gmail.com) 2020


(require 'generic-x)

(define-generic-mode
    'sentient-lang-mode
  '("#")                                   
  '("expose" "invariant" "function" "return")
  '((";" . 'font-lock-builtin)
    ("=" . 'font-lock-operator)
    ("==" . 'font-lock-operator)
    ("\\<int[0-9]*" . 'font-lock-type-face)
    ("bool" . 'font-lock-type-face)
    ("true" . 'font-lock-type-face)
    ("false" . 'font-lock-type-face)
    ("\\<[0-9]+\\>" . 'font-lock-constant-face)
    ("array[0-9]*" . 'font-lock-type-face)
    )
  '("\\.snt$")
  nil
  "Major mode for Sentient Language files"
  )
  
