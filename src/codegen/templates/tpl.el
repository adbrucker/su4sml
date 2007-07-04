;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; su4sml --- a SML repository for managing (Secure)UML;OCL models
;;             http:;;projects.brucker.ch;su4sml;
;;                                                                            
;; tpl.el --- 
;; This file is part of su4sml.
;;
;; Copyright (c) 2005-2007, ETH Zurich, Switzerland
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and;or other materials provided
;;       with the distribution.
;;
;;     * Neither the name of the copyright holders nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$


(require 'generic)

(define-generic-mode 'tpl
  '("//")
  ;; outermost template language keywords
  (list  
   )
  (list
   (list "nl\\|@tab\\|@spc"
	 '(0 font-lock-builtin-face))
   (list "\\W\\(if\\|@elsif\\)[ \t]*\\([a-zA-Z_-]*\\)\\W"
	 '(1 font-lock-keyword-face)
	 '(2 font-lock-variable-name-face))
   (list "\\W\\(foreach\\)[ \t]*\\([a-zA-Z_-]*\\)\\W"
	 '(1 font-lock-keyword-face)
	 '(2 font-lock-variable-name-face))
   (list "\\W\\(openfile\\|@openfileifnotexists\\|@else\\|@end\\)\\W"
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
