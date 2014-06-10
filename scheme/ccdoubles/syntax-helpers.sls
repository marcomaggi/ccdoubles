;;; -*- coding: utf-8 -*-
;;;
;;;Part of: R6RS SOFA
;;;Contents: common helper functions for syntax writing
;;;Date: Mon Jan 14, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (sofa syntax-helpers)
  (export syntax->list)
  (import (rnrs))


(define (syntax->list stx)
  ;;Given  a syntax  object STX  holding a  list, return  a proper  list
  ;;holding the component syntax objects.
  ;;
  (syntax-case stx ()
    ((?car . ?cdr)
     ;;Yes, it  is not tail recursive;  "they" say it is  efficient this
     ;;way.
     (cons #'?car (syntax->list #'?cdr)))
    (()
     '())))


;;;; done

)

;;; end of file
