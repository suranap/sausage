; Copyright 1992 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; Implementation-dependent parameters and preferences that determine
; how identifiers are represented in the output of the macro expander.
;
; The basic problem is that there are no reserved words, so the
; syntactic keywords of core Scheme that are used to express the
; output need to be represented by data that cannot appear in the
; input.  This file defines those data.

; The following definitions assume that identifiers of mixed case
; cannot appear in the input.

;(define begin1  (string->symbol "Begin"))
;(define define1 (string->symbol "Define"))
;(define quote1  (string->symbol "Quote"))
;(define lambda1 (string->symbol "Lambda"))
;(define case-lambda1 (string->symbol "Case-Lambda"))
;(define if1     (string->symbol "If"))
;(define set!1   (string->symbol "Set!"))
;(define let1   (string->symbol "Let"))	; not used

;; NOTE: For now, I'll just reuse the keywords from core Scheme 
(define begin1  'begin)
(define define1 'define)
(define quote1  'quote)
(define lambda1 'lambda)
(define case-lambda1 'case-lambda)
(define if1     'if)
(define set!1   'set!)
(define letrec1 'letrec)
(define let1   'let)

; The following defines an implementation-dependent expression
; that evaluates to an undefined (not unspecified!) value, for
; use in expanding the (define x) syntax.

(define undefined1 (list (string->symbol "Undefined")))

; A variable is renamed by suffixing a vertical bar followed by a unique
; integer.  In IEEE and R4RS Scheme, a vertical bar cannot appear as part
; of an identifier, but presumably this is enforced by the reader and not
; by the compiler.  Any other character that cannot appear as part of an
; identifier may be used instead of the vertical bar.

(define suffix-character #\|)
