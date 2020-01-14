;; allow people to define a hook for the mode
;; necessary?
(defvar sentient-lang-mode-hook nil)

;; do we need key binding?
(defvar sentient-lang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Sentient Lang major mode")

;; bind the mode to .snt files
(add-to-list 'auto-mode-alist '("\\.snt" . sentient-lang-mode))

;; keywords
(defconst sentient-lang-mode-font-lock-keywords-1
  (list
   '("\\<\\(expose\\|invariant\\|function\\|return\\)\\>" . font-lock-function-name-face)
   '(";" . font-lock-builtin-face)
   '("true\\|false" . font-lock-constant-face)
   '("\\(\\<int[0-9]*\\|\\<array[0-9]*\\)" . font-lock-type-face)
   '("[^a-z]\\([0-9]+\\)" . (1 font-lock-constant-face))
   )
  "Minimal highlighting expressions for Sentient Lang mode")

(defconst sentient-lang-mode-font-lock-keywords-2
  (append sentient-lang-mode-font-lock-keywords-1
	  (list
	   '()
	   ))
  "More highlighted expressions for Sentient Lang mode")

(defvar sentient-lang-mode-font-lock-keywords sentient-lang-mode-font-lock-keywords-1 "Default highlighing expressions for Senitent Lang mode")

;; syntax
(defvar sentient-lang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for sentient-lang-mode")


(define-derived-mode sentient-lang-mode fundamental-mode "Sentient"
  "Major mode for editing Sentient Language files"
  :syntax-table sentient-lang-mode-syntax-table
  (setq-local comment-start "#")
  (setq font-lock-defaults '(sentient-lang-mode-font-lock-keywords))

  (setq major-mode 'sentient-lang-mode)
  (setq mode-name "Sentient")
;  (run-hooks 'sentient-lang-mode-hook)
  )

(provide 'sentient-lang-mode)
