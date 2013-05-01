
;; @ R4RS macro system

;; Implemented by Will Clinger

(define-interface macros-interface
  (export macro-expand
	  define-syntax-scope))

(define-structure macros macros-interface
  (open scheme signals)
  (files prefs 
	 expand
	 misc
	 syntaxenv
	 syntaxrules
	 usual)
  (optimize auto-integrate))

