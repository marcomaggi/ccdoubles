;;; -*- coding: utf-8 -*-
;;;
;;;Part of: R6RS SOFA
;;;Contents: Guile specific functions
;;;Date: Wed Jan 16, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (sofa compat)
  (export
    with-local-storage
    define-c-function
    local-storage-ref-c-double)
  (import (rnrs)
    (prefix (only (guile)
		  dynamic-link
		  dynamic-func)
	    ffi.)
    (prefix (system foreign)
	    ffi.)
    (for (sofa syntax-helpers)
	 expand))


(define-syntax define-c-function
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?retval-type ?c-function-name (?arg-type ...))
	 (let ((arg-types (syntax->list #'(?arg-type ...))))
	   (if (<= 10 (length arg-types))
	       #'(define ?c-function-name
		   (lambda args
		     (error (quote ?c-function-name)
		       "too many arguments to foreign function, unsupported by Guile")))
	     (with-syntax
		 ((RETVAL-TYPE    (%external-type-id->internal-type-id #'?retval-type))
		  (FUNC-NAME      (symbol->string (syntax->datum #'?c-function-name)))
		  ((ARG-TYPE ...) (map %external-type-id->internal-type-id arg-types)))
	       #'(define ?c-function-name
		   (ffi.pointer->procedure RETVAL-TYPE
					   (ffi.dynamic-func FUNC-NAME libsofac)
					   (list ARG-TYPE ...)))
	       ))))))

    (define (%external-type-id->internal-type-id type-id)
      (case (syntax->datum type-id)
	((int)		#'ffi.int)
	((int*)		#'(quote *))
	((char)		#'ffi.int8)
	((char*)	#'(quote *))
	((double*)	#'(quote *))
	((void*)	#'(quote *))
	((double)	#'ffi.double)
	((void)		#'ffi.void)
	(else
	 (synner "unsupported C language type" type-id))))

    (define (synner message subform)
      (syntax-violation 'define-c-function message stx subform))

    (main stx)))


(define-syntax with-local-storage
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ((?var ?type-id) ...) ?body0 ?body ...)
	 (with-syntax
	     (((TYPE ...)	(map %external-type-id->internal-type-id
				  (syntax->list #'(?type-id ...))))
	      ((VALUE ...)	(map %external-type-id->zero-value
				  (syntax->list #'(?type-id ...)))))
	   #'(let ((?var (ffi.make-c-struct (list TYPE) (list VALUE)))
		   ...)
	       ?body0 ?body ...)))))

    (define (%external-type-id->internal-type-id type-id)
      (case (syntax->datum type-id)
	((int)		#'ffi.int)
	((int*)		#'(quote *))
	((char)		#'ffi.int8)
	((char*)	#'(quote *))
	((double*)	#'(quote *))
	((double)	#'ffi.double)
	((void)		#'ffi.void)
	(else
	 (%error-invalid-C-type type-id))))

    (define (%external-type-id->zero-value type-id)
      (case (syntax->datum type-id)
	((int)		0)
	((int*)		#'ffi.%null-pointer)
	((char)		0)
	((char*)	#'ffi.%null-pointer)
	((double*)	#'ffi.%null-pointer)
	((double)	0.0)
	(else
	 (%error-invalid-C-type type-id))))

    (define (%error-invalid-C-type type-id)
      (synner "unsupported C language type" type-id))

    (define (synner message subform)
      (syntax-violation 'with-local-storage message stx subform))

    (main stx)))

(define (local-storage-ref-c-double loc)
  (car (ffi.parse-c-struct loc (list ffi.double))))


(define libsofac
  (ffi.dynamic-link "libsofac.so"))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.with-local-storage 'scheme-indent-function 1)
;; End:
