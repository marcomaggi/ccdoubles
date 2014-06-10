;;;
;;;Part of: CCDoubles
;;;Contents: convert table into FFI stuff
;;;Date: Tue Jun 10, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (rnrs (6)))

(define sexp
  ;; We expect SEXP to have the format:
  ;;
  ;;   (foreign-library
  ;;     (name "ccdoubles")
  ;;     (version
  ;;       (major	?major-number)
  ;;       (minor	?minor-number)
  ;;       (patch	?patch-number))
  ;;     (functions ?func-spec ...))
  ;;
  (let* ((P (open-input-file "table.scm"))
	 (D (read P)))
    (close-port P)
    D))

(define name
  ;;A symbol representing the library identifier name.
  ;;
  (cadr (list-ref sexp 1)))

(define libname
  ;;A string representing the Unix-style shared object file name.
  ;;
  (string-append "lib" name ".so"))

(define version-spec
  ;;A list of version numbers.
  ;;
  (map cadr (cdr (list-ref sexp 2))))

(define func-spec*
  ;;We expect each FUNC-SPEC to have the format:
  ;;
  ;;   (?retval ?func-name (?arg-type ?arg-name ...))
  ;;
  (cdr (list-ref sexp 3)))

(define func-name*
  ;;List of strings representing the C function names.
  ;;
  (map (lambda (func-spec)
	 (symbol->string (list-ref func-spec 1)))
    func-spec*))

;;; --------------------------------------------------------------------

(display (string-append "(library (" name " "))
(display version-spec)
(display ")\n")
(display "  (export")
(map (lambda (func-name)
       (display (string-append "\n    " func-name "")))
  func-name*)
(display ")\n")
(display "  (import (rnrs (6)) (ccdoubles compat))\n")
(display (string-append "  (define-shared-object \"" libname "\")\n"))

(map (lambda (func-spec)
       ;;We expect FUNC-SPEC to have the format:
       ;;
       ;;   (?retval ?func-name (?arg-type ?arg-name ...))
       ;;
       (let ((retval     (list-ref func-spec 0))
	     (func-name  (list-ref func-spec 1))
	     (arg*       (list-ref func-spec 2)))
	 (display (string-append "  (define-c-function "
				 (symbol->string retval) " "
				 (symbol->string func-name)
				 " ("))
	 (if (equal? arg* '(void))
	     (display 'void)
	   ;;Here we know ARG* holds an even number of items.
	   (let loop ((arg* arg*))
	     (unless (null? arg*)
	       (display (symbol->string (car arg*)))
	       (display " ")
	       (loop (cddr arg*)))))
	 (display "))\n")))
  func-spec*)

(display "  #| end of library |#)\n\n;;; end of file\n")
(flush-output-port (current-output-port))

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
