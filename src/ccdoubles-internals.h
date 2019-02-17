/*
  Part of: CCDoubles
  Contents: private header file
  Date: Sat Jun  7, 2014

  Abstract

	This header file is for internal definitions.  It must be included by all the
	source files in this package.

  Copyright (C) 2014, 2015, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is free  software: you can redistribute it and/or  modify it under the
  terms of the  GNU Lesser General Public  License as published by  the Free Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
  WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Lesser Public License along with
  this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef CCDOUBLES_INTERNALS_H
#define CCDOUBLES_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <string.h>
#include <assert.h>

#define CCDOUBLES_ENABLE_SHORT_MACROS	1
#include "ccdoubles.h"


/** --------------------------------------------------------------------
 ** Preprocessor macros.
 ** ----------------------------------------------------------------- */

/* Build and return a "double complex". */
#define CPLX(REAL,IMAG)		(CCDOUBLES_CPLX((REAL),(IMAG)))

#define INT_SWAP(A,B)			\
  do {					\
    int	tmp = A;			\
    A = B;				\
    B = tmp;				\
  } while (0);

#define REAL_SWAP(A,B)			\
  do {					\
    double	tmp = A;		\
    A = B;				\
    B = tmp;				\
  } while (0);

#define COMPLEX_SWAP(A,B)		\
  do {					\
    double complex	tmp = A;	\
    A = B;				\
    B = tmp;				\
  } while (0);

#define COMPLEX_SWAP_CONJ(A,B)		\
  do {					\
    double complex	tmp = A;	\
    A = conj(B);			\
    B = conj(tmp);			\
  } while (0);


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */




/** --------------------------------------------------------------------
 ** Function prototypes.
 ** ----------------------------------------------------------------- */



/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CCDOUBLES_INTERNALS_H */

/* end of file */
