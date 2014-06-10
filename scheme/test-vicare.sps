;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: CCDoubles
;;;Contents: test file for Vicare Scheme template binding
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


#!vicare
(import (vicare)
  (ccdoubles)
  (prefix (vicare platform words) words.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing CCDoubles\n")


;;;; helpers

(define-constant sizeof-double
  words.SIZEOF_DOUBLE)

(define-constant sizeof-double-complex
  (* 2 sizeof-double))

;;; --------------------------------------------------------------------

(define* (ccdoubles-real-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (vector-set! V i (array-ref-c-double P i)))))

(define* (ccdoubles-cplx-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let ((rep (array-ref-c-double P j))
	    (imp (array-ref-c-double P (fxadd1 j))))
	(vector-set! V i (make-rectangular rep imp))))))

;;; --------------------------------------------------------------------

(define* (scheme-vector->ccdoubles-real-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (array-set-c-double! P i (vector-ref V i)))))

(define* (scheme-vector->ccdoubles-cplx-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double-complex))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let* ((Z   (vector-ref V i))
	     (rep (real-part Z))
	     (imp (imag-part Z)))
	(array-set-c-double! P j          rep)
	(array-set-c-double! P (fxadd1 j) imp)))))

;;; --------------------------------------------------------------------

(define-constant EPSILON
  1e-6)

(define* (double-vector=? {O1 vector?} {O2 vector?})
  (let loop ((i 0))
    (or (fx=? i (vector-length O1))
	(and (let ((X (vector-ref O1 i))
		   (Y (vector-ref O2 i)))
	       (< (magnitude (- X Y)) EPSILON))
	     (loop (fxadd1 i))))))


(parametrise ((check-test-name	'real-vectors-helpers))

  (check
      (let* ((N 3)
	     (P (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
	     (V (ccdoubles-real-vector->scheme-vector N P)))
	V)
    => '#(1.2 3.4 5.6))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-helpers))

  (check
      (let* ((N 3)
	     (P (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
	     (V (ccdoubles-cplx-vector->scheme-vector N P)))
	V)
    => '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))

  (collect))


(parametrise ((check-test-name	'real-vectors-basic))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles_real_vector_clear N P)
  	(ccdoubles-real-vector->scheme-vector N P))
    => '#(0.0 0.0 0.0))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles_real_vector_set N P 1.2)
  	(ccdoubles-real-vector->scheme-vector N P))
    => '#(1.2 1.2 1.2))

  (check
      (let* ((N 3)
  	     (S (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (D (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles_real_vector_copy N D S)
  	(ccdoubles-real-vector->scheme-vector N D))
    => '#(1.2 3.4 5.6))

  (collect))


(parametrise ((check-test-name	'real-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles_real_vector_add N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map +
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles_real_vector_sub N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map -
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles_real_vector_mul N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map *
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles_real_vector_div N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map /
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R (guarded-malloc (* N sizeof-double))))
        (ccdoubles_real_vector_neg N R O)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map -
		       '(7.8 8.9 9.0))))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-basic))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles_cplx_vector_clear N P)
  	(ccdoubles-cplx-vector->scheme-vector N P))
    => '#(0.0+0.0i 0.0+0.0i 0.0+0.0i))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles_cplx_vector_set_split N P 1.2 3.4)
  	(ccdoubles-cplx-vector->scheme-vector N P))
    => '#(1.2+3.4i 1.2+3.4i 1.2+3.4i))

  (check
      (let* ((N 3)
  	     (S (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (D (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles_cplx_vector_copy N D S)
  	(ccdoubles-cplx-vector->scheme-vector N D))
    => '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles_cplx_vector_add N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> double-vector=?)
    (list->vector (map +
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles_cplx_vector_sub N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> double-vector=?)
    (list->vector (map -
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles_cplx_vector_mul N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> double-vector=?)
    (list->vector (map *
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles_cplx_vector_div N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> double-vector=?)
    (list->vector (map /
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles_cplx_vector_neg N R O)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> double-vector=?)
    (list->vector (map -
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (collect))


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
