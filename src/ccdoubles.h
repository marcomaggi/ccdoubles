/*
  Part of: CCDoubles
  Contents: public header file
  Date: Sat Jun  7, 2014

  Abstract

	This  is  the  public  header  file of  CCDoubles,  a  C11  language  library
	implementing numeric  routines.  The library is  meant to be a  companion for
	more advanced libraries like CBLAS and LAPACKE.

  Copyright (C) 2014, 2015, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is free  software: you can redistribute it and/or  modify it under the
  terms of the  GNU Lesser General Public  License as published by  the Free Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
  WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License along with
  this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef CCDOUBLES_H
#define CCDOUBLES_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <stdio.h>
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
#  define CCDOUBLES_UNUSED		__attribute__((__unused__))
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
#      define ccdoubles_decl		__attribute__((__dllexport__))
#    else
#      define ccdoubles_decl		__declspec(dllexport)
#    endif
#  else
#    ifdef __GNUC__
#      define ccdoubles_decl		__attribute__((__dllimport__))
#    else
#      define ccdoubles_decl		__declspec(dllimport)
#    endif
#  endif
#  define ccdoubles_private_decl	extern
#else
#  if __GNUC__ >= 4
#    define ccdoubles_decl		__attribute__((__visibility__ ("default")))
#    define ccdoubles_private_decl	__attribute__((__visibility__ ("hidden")))
#  else
#    define ccdoubles_decl		extern
#    define ccdoubles_private_decl	extern
#  endif
#endif


/** --------------------------------------------------------------------
 ** Preprocessor macros.
 ** ----------------------------------------------------------------- */

#define CCDOUBLES_CPLX(REAL,IMAG)	(((double)(REAL)) + ((double)(IMAG)) * ((double complex)(_Complex_I)))

#ifdef CCDOUBLES_ENABLE_SHORT_MACROS
#  define Z(REAL,IMAG)		(CCDOUBLES_CPLX((REAL),(IMAG)))
#  define Re(Z)			(creal(Z))
#  define Im(Z)			(cimag(Z))

#  define MREF(M)		(&M[0][0])
#endif


/** --------------------------------------------------------------------
 ** Constants.
 ** ----------------------------------------------------------------- */




/** --------------------------------------------------------------------
 ** Type definitions.
 ** ----------------------------------------------------------------- */

typedef int * restrict			ccdoubles_int_result_t;
typedef const int * restrict		ccdoubles_int_operand_t;

typedef double * restrict		ccdoubles_real_result_t;
typedef const double * restrict		ccdoubles_real_operand_t;

typedef double complex * restrict	ccdoubles_cplx_result_t;
typedef const double complex * restrict	ccdoubles_cplx_operand_t;


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl char const *	ccdoubles_version_string		(void);
ccdoubles_decl int		ccdoubles_version_interface_current	(void);
ccdoubles_decl int		ccdoubles_version_interface_revision	(void);
ccdoubles_decl int		ccdoubles_version_interface_age		(void);


/** --------------------------------------------------------------------
 ** Complex scalar functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl double complex ccdoubles_cplx_mul (double complex O1, double complex O2)
  __attribute__((__pure__));

ccdoubles_decl double complex ccdoubles_cplx_div (double complex O1, double complex O2)
  __attribute__((__pure__));

ccdoubles_decl double complex ccdoubles_cplx_neg (double complex O)
  __attribute__((__pure__));


/** --------------------------------------------------------------------
 ** Real vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_vector_clear (unsigned nslots, ccdoubles_real_result_t R)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_real_vector_set   (unsigned nslots, ccdoubles_real_result_t R, double value)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_real_vector_copy (unsigned nslots, ccdoubles_real_result_t  dst, ccdoubles_real_operand_t src)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_add (unsigned nslots, ccdoubles_real_result_t  result,
					       ccdoubles_real_operand_t operand1, ccdoubles_real_operand_t operand2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_sub (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_mul (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_div (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_neg (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_abs (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_fmod (unsigned nslots, ccdoubles_real_result_t R,
						ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_drem (unsigned nslots, ccdoubles_real_result_t R,
						ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_remainder (unsigned nslots, ccdoubles_real_result_t R,
						     ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_ceil (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_floor (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_trunc (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_round (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_rint (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_isgreater (unsigned nslots, ccdoubles_int_result_t result,
						     ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_isgreaterequal (unsigned nslots, ccdoubles_int_result_t result,
							  ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_isless (unsigned nslots, ccdoubles_int_result_t result,
						  ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_islessequal (unsigned nslots, ccdoubles_int_result_t result,
						       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_islessgreater (unsigned nslots, ccdoubles_int_result_t result,
							 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_isunordered (unsigned nslots, ccdoubles_int_result_t result,
						       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_min (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_max (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_fpclassify (unsigned nslots, ccdoubles_int_result_t result, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_isfinite (unsigned nslots, ccdoubles_int_result_t result, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_isinfinite (unsigned nslots, ccdoubles_int_result_t result, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_isnormal (unsigned nslots, ccdoubles_int_result_t result, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_isnan (unsigned nslots, ccdoubles_int_result_t result, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl double ccdoubles_real_vector_scalar_product (unsigned nslots, ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_scalar_mul (unsigned nslots, ccdoubles_real_result_t R, double lambda, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,4)));

ccdoubles_decl void ccdoubles_real_vector_linear_combination (unsigned nslots, ccdoubles_real_result_t R,
							      double alpha, ccdoubles_real_operand_t O1,
							      double beta, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,4,6)));

ccdoubles_decl void ccdoubles_real_vector_linspace (unsigned nslots, ccdoubles_real_result_t R, double start, double past)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_real_vector_logspace (unsigned nslots, ccdoubles_real_result_t R, double start, double past)
  __attribute__((__nonnull__(2)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_exp (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_exp10 (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_exp2 (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_log (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_log10 (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_log2 (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_logb (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_pow (unsigned nslots, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_sqrt (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_cbrt (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_hypot (unsigned nslots, ccdoubles_real_result_t R,
						 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_real_vector_expm1 (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_log1p (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_sin (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_cos (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_tan (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_asin (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_acos (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_atan (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_atan2 (unsigned nslots, ccdoubles_real_result_t R,
						 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_sinh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_cosh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_tanh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_asinh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_acosh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_real_vector_atanh (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(2,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_copy_forward (unsigned dst_start, unsigned src_start, unsigned nslots,
							ccdoubles_real_result_t dst, ccdoubles_real_operand_t src)
  __attribute__((__nonnull__(4,5)));

ccdoubles_decl void ccdoubles_real_vector_copy_backward (unsigned dst_start, unsigned src_start, unsigned nslots,
							 ccdoubles_real_result_t dst, ccdoubles_real_operand_t src)
  __attribute__((__nonnull__(4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_print_display (FILE * f, const char * name, unsigned nslots, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,2,4)));

ccdoubles_decl void ccdoubles_real_vector_print_brackets (FILE * f, unsigned nslots, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,3)));

ccdoubles_decl void ccdoubles_real_vector_print_braces (FILE * f, unsigned nslots, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,3)));


/** --------------------------------------------------------------------
 ** Real matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_matrix_clear (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_real_matrix_set   (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, double value)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_real_matrix_copy (unsigned nrows, unsigned ncols, ccdoubles_real_result_t dst, ccdoubles_real_operand_t src)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_add (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_sub (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_mul (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_div (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_neg (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_abs (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_fmod (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_drem (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_remainder (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						     ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_ceil (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_floor (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_trunc (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_round (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_rint (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_isgreater (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
						     ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_isgreaterequal (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
							  ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_isless (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
						  ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_islessequal (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
						       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_islessgreater (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
							 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_isunordered (unsigned nrows, unsigned ncols, ccdoubles_int_result_t result,
						       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_min (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_max (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_fpclassify (unsigned nrows, unsigned ncols, ccdoubles_int_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_isfinite (unsigned nrows, unsigned ncols, ccdoubles_int_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_real_matrix_isinfinite (unsigned nrows, unsigned ncols, ccdoubles_int_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_isnormal (unsigned nrows, unsigned ncols, ccdoubles_int_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_isnan (unsigned nrows, unsigned ncols, ccdoubles_int_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_scalar_mul (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						      double lambda, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,5)));

ccdoubles_decl void ccdoubles_real_matrix_linear_combination (unsigned nrows, unsigned ncols,
							      ccdoubles_real_result_t R,
							      double alpha, ccdoubles_real_operand_t O1,
							      double beta, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,5,7)));

ccdoubles_decl void ccdoubles_real_matrix_transpose (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_rowcol_mul (unsigned R_nrows, unsigned operand_n, unsigned R_ncols,
						      ccdoubles_real_result_t R, ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(4,5,6)));

ccdoubles_decl void ccdoubles_real_matrix_linspace (unsigned nrows, unsigned ncols,
						    ccdoubles_real_result_t R, double start, double row_past, double col_past)
  __attribute__((__nonnull__(3)));

#if 0
ccdoubles_decl void ccdoubles_real_matrix_logspace (unsigned nrows, unsigned ncols,
						    ccdoubles_real_result_t R, double start,
						    double row_past, double col_past)
  __attribute__((__nonnull__(3)));
#endif

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_exp (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_exp10 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_exp2 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_log (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_log10 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_log2 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_logb (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_pow (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
					       ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_sqrt (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_cbrt (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_hypot (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_expm1 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_log1p (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_sin (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_cos (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_tan (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_asin (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_acos (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_atan (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_atan2 (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R,
						 ccdoubles_real_operand_t O1, ccdoubles_real_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_real_matrix_sinh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_cosh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_tanh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_asinh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_acosh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_real_matrix_atanh (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_row_to_row (unsigned dst_nrows, unsigned dst_ncols, ccdoubles_real_result_t  dst,
						      unsigned src_nrows, unsigned src_ncols, ccdoubles_real_operand_t src,
						      unsigned dst_row, unsigned dst_col,
						      unsigned src_row, unsigned src_col,
						      unsigned nslots)
  __attribute__((__nonnull__(3,6)));

ccdoubles_decl void ccdoubles_real_matrix_col_to_col (unsigned dst_nrows, unsigned dst_ncols, ccdoubles_real_result_t  dst,
						      unsigned src_nrows, unsigned src_ncols, ccdoubles_real_operand_t src,
						      unsigned dst_row, unsigned dst_col,
						      unsigned src_row, unsigned src_col,
						      unsigned nslots)
  __attribute__((__nonnull__(3,6)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_print_display (FILE * f, const char * name, unsigned nrows, unsigned ncols,
							 ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,2,5)));

ccdoubles_decl void ccdoubles_real_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,4)));

ccdoubles_decl void ccdoubles_real_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_real_operand_t O)
  __attribute__((__nonnull__(1,4)));


/** --------------------------------------------------------------------
 ** Complex vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_vector_clear (unsigned nslots, ccdoubles_cplx_result_t R)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_cplx_vector_set   (unsigned nslots, ccdoubles_cplx_result_t R, double complex value)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_cplx_vector_set_split (unsigned nslots, ccdoubles_cplx_result_t R, double value_re, double value_im)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_cplx_vector_copy (unsigned nslots, ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_real (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_imag (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_magnitude (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_angle (unsigned nslots, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_conj (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_from_rect (unsigned nslots, ccdoubles_cplx_result_t R,
						     ccdoubles_real_operand_t real, ccdoubles_real_operand_t imag)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_from_polar (unsigned nslots, ccdoubles_cplx_result_t R,
						      ccdoubles_real_operand_t magnitude, ccdoubles_real_operand_t angle)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_add (unsigned nslots, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_sub (unsigned nslots, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_mul (unsigned nslots, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_div (unsigned nslots, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_neg (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl double complex ccdoubles_cplx_vector_scalar_product (unsigned nslots, ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_scalar_product_split (unsigned nslots,double complex * result,
								ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

ccdoubles_decl void ccdoubles_cplx_vector_scalar_mul (unsigned nslots, ccdoubles_cplx_result_t R,
						      double complex lambda, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,4)));

ccdoubles_decl void ccdoubles_cplx_vector_scalar_mul_split (unsigned nslots, ccdoubles_cplx_result_t R,
							    double lambda_re, double lambda_im, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,5)));

ccdoubles_decl void ccdoubles_cplx_vector_linear_combination (unsigned nslots, ccdoubles_cplx_result_t R,
							      double complex alpha, ccdoubles_cplx_operand_t O1,
							      double complex beta, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,4,6)));

ccdoubles_decl void ccdoubles_cplx_vector_linear_combination_split (unsigned nslots, ccdoubles_cplx_result_t R,
								    double alpha_re, double alpha_im, ccdoubles_cplx_operand_t O1,
								    double beta_re, double beta_im, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,5,8)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_exp (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_log (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_log10 (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_sqrt (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_pow (unsigned nslots, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(2,3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_sin (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_cos (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_tan (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_asin (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_acos (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_atan (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_sinh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_cosh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_tanh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_asinh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_acosh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_cplx_vector_atanh (unsigned nslots, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(2,3)));


/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_copy_forward (unsigned dst_start, unsigned src_start, unsigned nslots,
							ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
  __attribute__((__nonnull__(4,5)));

ccdoubles_decl void ccdoubles_cplx_vector_copy_backward (unsigned dst_start, unsigned src_start, unsigned nslots,
							 ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
  __attribute__((__nonnull__(4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_print_display  (FILE * f, const char * name, unsigned nslots, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,2,4)));

ccdoubles_decl void ccdoubles_cplx_vector_print_brackets (FILE * f, unsigned nslots, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,3)));

ccdoubles_decl void ccdoubles_cplx_vector_print_braces   (FILE * f, unsigned nslots, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,3)));


/** --------------------------------------------------------------------
 ** Complex matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_matrix_clear (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_cplx_matrix_set   (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, double complex value)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_cplx_matrix_set_split (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
						     double value_re, double value_im)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_cplx_matrix_copy (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_real (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_imag (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_magnitude (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_angle (unsigned nrows, unsigned ncols, ccdoubles_real_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_conj (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_from_rect (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
						     ccdoubles_real_operand_t real, ccdoubles_real_operand_t imag)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_from_polar (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
						      ccdoubles_real_operand_t magnitude, ccdoubles_real_operand_t angle)
  __attribute__((__nonnull__(3,4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_add (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_sub (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_mul (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_div (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_neg (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_scalar_mul (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
						      double complex lambda, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_scalar_mul_split (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
							    double lambda_re, double lambda_im, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,6)));

ccdoubles_decl void ccdoubles_cplx_matrix_linear_combination (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
							      double complex alpha, ccdoubles_cplx_operand_t O1,
							      double complex beta, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,5,7)));

ccdoubles_decl void ccdoubles_cplx_matrix_linear_combination_split (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
								    double alpha_re, double alpha_im, ccdoubles_cplx_operand_t O1,
								    double beta_re, double beta_im, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,6,9)));

ccdoubles_decl void ccdoubles_cplx_matrix_transpose (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_conjugate_transpose (unsigned operand_nrows, unsigned operand_ncols,
							       ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_rowcol_mul (unsigned result_nrows, unsigned operand_n, unsigned result_ncols,
						      ccdoubles_cplx_result_t R,
						      ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(4,5,6)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_exp (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_log (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_log10 (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_sqrt (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_pow (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R,
					       ccdoubles_cplx_operand_t O1, ccdoubles_cplx_operand_t O2)
  __attribute__((__nonnull__(3,4,5)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_sin (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_cos (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_tan (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_asin (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_acos (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_atan (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_sinh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_cosh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_tanh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_asinh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_acosh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_atanh (unsigned nrows, unsigned ncols, ccdoubles_cplx_result_t R, ccdoubles_cplx_operand_t O)
    __attribute__((__nonnull__(3,4)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_print_display (FILE * f, const char * name, unsigned nrows, unsigned ncols,
							 ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,2,5)));

ccdoubles_decl void ccdoubles_cplx_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,4)));

ccdoubles_decl void ccdoubles_cplx_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_cplx_operand_t O)
  __attribute__((__nonnull__(1,4)));


/** --------------------------------------------------------------------
 ** Arrays of integers.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_int_vector_clear (unsigned nslots, ccdoubles_int_result_t vector)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_int_vector_set   (unsigned nslots, ccdoubles_int_result_t vector, int value)
  __attribute__((__nonnull__(2)));

ccdoubles_decl void ccdoubles_int_vector_copy  (unsigned nslots, ccdoubles_int_result_t dst, ccdoubles_int_operand_t src)
  __attribute__((__nonnull__(2,3)));

ccdoubles_decl void ccdoubles_int_vector_print_display  (FILE * f, const char * name, unsigned nslots, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,2,4)));

ccdoubles_decl void ccdoubles_int_vector_print_brackets (FILE * f, unsigned nslots, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,3)));

ccdoubles_decl void ccdoubles_int_vector_print_braces   (FILE * f, unsigned nslots, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,3)));

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_int_matrix_clear (unsigned nrows, unsigned ncols, ccdoubles_int_result_t matrix)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_int_matrix_set   (unsigned nrows, unsigned ncols, ccdoubles_int_result_t matrix, int value)
  __attribute__((__nonnull__(3)));

ccdoubles_decl void ccdoubles_int_matrix_copy  (unsigned nrows, unsigned ncols, ccdoubles_int_result_t dst, ccdoubles_int_operand_t src)
  __attribute__((__nonnull__(3,4)));

ccdoubles_decl void ccdoubles_int_matrix_print_display  (FILE * f, const char * name,
							 unsigned nrows, unsigned ncols, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,2,5)));

ccdoubles_decl void ccdoubles_int_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,4)));

ccdoubles_decl void ccdoubles_int_matrix_print_braces   (FILE * f, unsigned nrows, unsigned ncols, ccdoubles_int_operand_t O)
  __attribute__((__nonnull__(1,4)));


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CCDOUBLES_H */

/* end of file */
