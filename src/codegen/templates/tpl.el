(require 'generic)

(define-generic-mode 'tpl
  '("@//")
  ;; outermost template language keywords
  (list  
   )
  (list
   (list "@nl\\|@tab"
	 '(0 font-lock-builtin-face))
   (list "\\W\\(@if\\|@elsif\\)[ \t]*\\([a-zA-Z_-]*\\)\\W"
	 '(1 font-lock-keyword-face)
	 '(2 font-lock-variable-name-face))
   (list "\\W\\(@foreach\\)[ \t]*\\([a-zA-Z_-]*\\)\\W"
	 '(1 font-lock-keyword-face)
	 '(2 font-lock-variable-name-face))
   (list "\\W\\(@openfile\\|@else\\|@end\\)\\W"
	 '(0 font-lock-keyword-face))
   ;; variables
   (list "\\$[a-zA-Z_-]*\\$" 
         '(0 font-lock-variable-name-face))
   ;; string constants
   (list "'[^']+'" 
         '(0 font-lock-string-face))
   )
  (list "\\.tpl\\'")
  nil
  "Generic mode for GCG template files.")
(provide 'tpl)
