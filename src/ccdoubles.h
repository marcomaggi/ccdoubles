/*
  Part of: CCDoubles
  Contents: public header file
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef CCDOUBLES_H
#define CCDOUBLES_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <complex.h>


/** --------------------------------------------------------------------
 ** Basic preprocessor macros.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
extern "C" {
#endif

/* The  macro  CCDOUBLES_UNUSED  indicates  that  a  function,  function
   argument or variable may potentially be unused. Usage examples:

   static int unused_function (char arg) CCDOUBLES_UNUSED;
   int foo (char unused_argument CCDOUBLES_UNUSED);
   int unused_variable CCDOUBLES_UNUSED;
*/
#ifdef __GNUC__
#  define CCDOUBLES_UNUSED		__attribute__((unused))
#else
#  define CCDOUBLES_UNUSED		/* empty */
#endif

#ifndef __GNUC__
#  define __attribute__(...)	/* empty */
#endif

/* I found  the following chunk on  the Net.  (Marco Maggi;  Sun Feb 26,
   2012) */
#if defined _WIN32 || defined __CYGWIN__
#  ifdef BUILDING_DLL
#    ifdef __GNUC__
#      define ccdoubles_decl		__attribute__((dllexport))
#    else
#      define ccdoubles_decl		__declspec(dllexport)
#    endif
#  else
#    ifdef __GNUC__
#      define ccdoubles_decl		__attribute__((dllimport))
#    else
#      define ccdoubles_decl		__declspec(dllimport)
#    endif
#  endif
#  define ccdoubles_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define ccdoubles_decl		__attribute__((visibility ("default")))
#    define ccdoubles_private_decl	__attribute__((visibility ("hidden")))
#  else
#    define ccdoubles_decl		extern
#    define ccdoubles_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */




/** --------------------------------------------------------------------
 ** Real vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_vector_clear (size_t nitems,
						 double * __restrict__ vector);
ccdoubles_decl void ccdoubles_real_vector_set   (size_t nitems,
						 double * __restrict__ vector,
						 double value);

ccdoubles_decl void ccdoubles_real_vector_add (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand1,
					       double * __restrict__ operand2);
ccdoubles_decl void ccdoubles_real_vector_sub (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand1,
					       double * __restrict__ operand2);
ccdoubles_decl void ccdoubles_real_vector_mul (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand1,
					       double * __restrict__ operand2);
ccdoubles_decl void ccdoubles_real_vector_div (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand1,
					       double * __restrict__ operand2);
ccdoubles_decl void ccdoubles_real_vector_neg (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand);
ccdoubles_decl double ccdoubles_real_vector_scalar_product (size_t nitems,
							    const double * __restrict__ operand1,
							    const double * __restrict__ operand2);
ccdoubles_decl void ccdoubles_real_vector_scalar_mul (size_t nitems,
						      double * __restrict__ result,
						      double lambda,
						      const double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_linear_combination (size_t nitems,
							      double * __restrict__ result,
							      double alpha,
							      double * __restrict__ operand1,
							      double beta,
							      double * __restrict__ operand2);

ccdoubles_decl void ccdoubles_real_vector_sin (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_cos (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_tan (size_t nitems,
					       double * __restrict__ result,
					       double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_asin (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_acos (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_atan (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_atan2 (size_t nitems,
						 double * __restrict__ result,
						 double * __restrict__ operand1,
						 double * __restrict__ operand2);

ccdoubles_decl void ccdoubles_real_vector_sinh (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_cosh (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_tanh (size_t nitems,
						double * __restrict__ result,
						double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_asinh (size_t nitems,
						 double * __restrict__ result,
						 double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_acosh (size_t nitems,
						 double * __restrict__ result,
						 double * __restrict__ operand);
ccdoubles_decl void ccdoubles_real_vector_atanh (size_t nitems,
						 double * __restrict__ result,
						 double * __restrict__ operand);


/** --------------------------------------------------------------------
 ** Complex vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_vector_clear (size_t nitems,
						 _Complex * __restrict__ vector);
ccdoubles_decl void ccdoubles_cplx_vector_set   (size_t nitems,
						 _Complex * __restrict__ vector,
						 _Complex value);

ccdoubles_decl void ccdoubles_cplx_vector_add (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand1,
					       _Complex * __restrict__ operand2);
ccdoubles_decl void ccdoubles_cplx_vector_sub (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand1,
					       _Complex * __restrict__ operand2);
ccdoubles_decl void ccdoubles_cplx_vector_mul (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand1,
					       _Complex * __restrict__ operand2);
ccdoubles_decl void ccdoubles_cplx_vector_div (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand1,
					       _Complex * __restrict__ operand2);
ccdoubles_decl void ccdoubles_cplx_vector_neg (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand);
ccdoubles_decl _Complex ccdoubles_cplx_vector_scalar_product (size_t nitems,
							      const _Complex * __restrict__ operand1,
							      const _Complex * __restrict__ operand2);
ccdoubles_decl void ccdoubles_cplx_vector_scalar_mul (size_t nitems,
						      _Complex * __restrict__ result,
						      _Complex lambda,
						      const _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_linear_combination (size_t nitems,
							      _Complex * __restrict__ result,
							      _Complex alpha,
							      _Complex * __restrict__ operand1,
							      _Complex beta,
							      _Complex * __restrict__ operand2);

ccdoubles_decl void ccdoubles_cplx_vector_sin (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_cos (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_tan (size_t nitems,
					       _Complex * __restrict__ result,
					       _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_asin (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_acos (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_atan (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);

ccdoubles_decl void ccdoubles_cplx_vector_sinh (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_cosh (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_tanh (size_t nitems,
						_Complex * __restrict__ result,
						_Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_asinh (size_t nitems,
						 _Complex * __restrict__ result,
						 _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_acosh (size_t nitems,
						 _Complex * __restrict__ result,
						 _Complex * __restrict__ operand);
ccdoubles_decl void ccdoubles_cplx_vector_atanh (size_t nitems,
						 _Complex * __restrict__ result,
						 _Complex * __restrict__ operand);


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl const char *	ccd_version_string		(void);
ccdoubles_decl int		ccd_version_interface_current	(void);
ccdoubles_decl int		ccd_version_interface_revision	(void);
ccdoubles_decl int		ccd_version_interface_age	(void);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CCDOUBLES_H */

/* end of file */
