;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: R6RS SOFA
;;;Contents: Sagittarius Scheme specific functions
;;;Date: Tue Jan 15, 2013
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
    (for (sofa syntax-helpers)
	 expand)
    (prefix (sagittarius ffi)
	    ffi.))


(define-syntax define-c-function
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?retval-type ?c-function-name (?arg-type ...))
	 (with-syntax
	     ((RETVAL-TYPE	(%external-type-id->internal-type-id #'?retval-type))
	      ((ARG-TYPES ...)	(map %external-type-id->internal-type-id
				  (syntax->list #'(?arg-type ...)))))
	   #'(define ?c-function-name
	       (ffi.c-function libsofac
	   		       RETVAL-TYPE
	   		       ?c-function-name
	   		       (ARG-TYPES ...)))
	   ;;This is another  version that also works, but  the above is
	   ;;preferred.  (Marco Maggi; Sun Jan 20, 2013)
	   ;;
	   ;; #'(define ?c-function-name
	   ;;     (ffi.make-c-function libsofac
	   ;; 			    (quote RETVAL-TYPE)
	   ;; 			    (quote ?c-function-name)
	   ;; 			    (quote (ARG-TYPES ...))))
	   ))))

    (define (%external-type-id->internal-type-id type-id)
      (case (syntax->datum type-id)
      	((int)		#'ffi.int)
      	((int*)		#'ffi.void*)
      	((char)		#'ffi.char)
      	((char*)	#'ffi.void*)
      	((double*)	#'ffi.void*)
      	((double)	#'ffi.double)
      	((void*)	#'ffi.void*)
      	((void)		#'ffi.void)
      	(else		type-id)))

    (main stx)))


(define-syntax with-local-storage
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ((?var ?size) ...) ?body0 ?body ...)
	 (with-syntax
	     (((SIZE ...) (map %external-type-id->internal-type-size
			    (syntax->list #'(?size ...)))))
	   #'(let ((?var (make-bytevector SIZE))
		   ...)
	       ?body0 ?body ...)))))

    (define (%external-type-id->internal-type-size type-id)
      (case (syntax->datum type-id)
	((int)		#'ffi.size-of-int)
	((double)	#'ffi.size-of-double)
	(else
	 (synner "unknown type identifier" type-id))))

    (define (synner message subform)
      (syntax-violation 'with-local-storage message stx subform))

    (main stx)))

(define-syntax local-storage-ref-c-double
  (syntax-rules ()
    ((_ ?bv)
     (bytevector-ieee-double-native-ref ?bv 0))))


(define libsofac
  (ffi.open-shared-library "libsofac.so.1"))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'ffi.with-local-storage 'scheme-indent-function 1)
;; End:
