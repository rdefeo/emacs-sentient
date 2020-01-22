;; Emacs font-lock mode for Sentient language
;; Roberto DeFeo (rdefeo@gmail.com) 2020
;; https://github.com/rdefeo/emacs-sentient

;; Provide a hook to allow custom code when mode is invoked
(defvar sentient-lang-mode-hook nil)

;; Bind the mode to .snt files
(add-to-list 'auto-mode-alist '("\\.snt" . sentient-lang-mode))

;; Define basic keywords, constants, and types
(defconst sentient-lang-mode-font-lock-keywords-1
  (list
   '("\\(expose\\|invariant\\|function[\\?!]?\\|return\\|buildArray\\)" . font-lock-function-name-face)
   '("\\<function\\>" "\\(.*\\)(" nil nil (1 font-lock-variable-name-face))
   '(";" . font-lock-builtin-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
   '("\\<int[0-9]*\\|\\<array[0-9]*" . font-lock-type-face)
   '("[^a-z]\\([0-9]+\\)" . (1 font-lock-constant-face))
   )
  "Minimal highlighting expressions for Sentient Lang mode")

;; Define operators and built-in methods on integers and arrays
(defconst sentient-lang-mode-font-lock-keywords-2
  (append sentient-lang-mode-font-lock-keywords-1
	  (list
	   ;; operators
	   '("\\_<\\(==?\\|\\+\\|\\-\\|/\\|\\?\\|:\\|\\*\\|||?\\|&&?\\|\\!\\)\\_>" . font-lock-builtin-face)
	   
	   '("\\.?\\(if\\)(" . (1 font-lock-keyword-face))

	   ;; method used by all types
	   '("\\.self" . font-lock-keyword-face)

	   ;; integer type methods
	   '("\\.\\(abs\\|between\\?\\|cube\\|divmod\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(downto\\|even\\?\\|negative\\?\\|next\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(odd\\?\\|positive\\?\\|pred\\|prev\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(square\\|succ\\?\\|times\\|upto\\|zero\\?\\)" . (1 font-lock-keyword-face))

	   ;; array type methods
	   '("\\.\\(fetch\\|all\\?\\|any\\?\\|bounds\\?\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(collect\\|count\\|countBy\\|eachCombination\\|eachCons\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(eachSlice\\|each\\|first\\|get\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(include\\?\\|last\\|length\\|map\\|none\\?\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(one\\?\\|push\\|reduce\\|reject\\|reverse\\|select\\)" . (1 font-lock-keyword-face))
	   '("\\.\\(size\\|sum\\|transpose\\|uniqBy\\?\\|uniq\\?\\)" . (1 font-lock-keyword-face))
	   ))
  "More highlighted expressions for Sentient Lang mode")

(defvar sentient-lang-mode-font-lock-keywords sentient-lang-mode-font-lock-keywords-2 "Default highlighing expressions for Senitent Lang mode")

;; Define comment style
(defvar sentient-lang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for sentient-lang-mode")

;; Defind the mode
(define-derived-mode sentient-lang-mode prog-mode "Sentient"
  "Major mode for editing Sentient Language files"
  (kill-all-local-variables)

  (set-syntax-table sentient-lang-mode-syntax-table)

  (setq-local font-lock-defaults '(sentient-lang-mode-font-lock-keywords))

  (setq major-mode 'sentient-lang-mode)
  (setq mode-name "Sentient")
  (run-hooks 'sentient-lang-mode-hook))

(provide 'sentient-lang-mode)
