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
 ** Preprocessor macros.
 ** ----------------------------------------------------------------- */

/* Build and return a "double complex". */
#define CCDOUBLES_CPLX(REAL,IMAG)	\
  (((double)(REAL)) + ((double)(IMAG)) * _Complex_I)

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
 ** Complex scalar functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl double complex ccdoubles_cplx_mul (double complex O1, double complex O2);
ccdoubles_decl double complex ccdoubles_cplx_div (double complex O1, double complex O2);
ccdoubles_decl double complex ccdoubles_cplx_neg (double complex O);


/** --------------------------------------------------------------------
 ** Real vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_vector_clear (unsigned nslots,
						 double * restrict vector);
ccdoubles_decl void ccdoubles_real_vector_set   (unsigned nslots,
						 double * restrict vector,
						 double value);
ccdoubles_decl void ccdoubles_real_vector_copy (unsigned nslots,
						double * restrict dst,
						double * restrict src);

ccdoubles_decl void ccdoubles_real_vector_add (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_sub (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_mul (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_div (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_neg (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_abs (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_fmod (unsigned nslots,
						double * restrict result,
						double * restrict operand1,
						double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_drem (unsigned nslots,
						double * restrict result,
						double * restrict operand1,
						double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_remainder (unsigned nslots,
						     double * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_ceil (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_floor (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_trunc (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_round (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_rint (unsigned nslots,
						double * restrict result,
						double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_isgreater (unsigned nslots,
						     int * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isgreaterequal (unsigned nslots,
							  int * restrict result,
							  double * restrict operand1,
							  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isless (unsigned nslots,
						  int * restrict result,
						  double * restrict operand1,
						  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_islessequal (unsigned nslots,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_islessgreater (unsigned nslots,
							 int * restrict result,
							 double * restrict operand1,
							 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isunordered (unsigned nslots,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_min (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_max (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_fpclassify (unsigned nslots,
						      int * restrict result,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_isfinite (unsigned nslots,
						    int * restrict result,
						    double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_isinfinite (unsigned nslots,
						      int * restrict result,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_isnormal (unsigned nslots,
						    int * restrict result,
						    double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_isnan (unsigned nslots,
						 int * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl double ccdoubles_real_vector_scalar_product (unsigned nslots,
							    const double * restrict operand1,
							    const double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_scalar_mul (unsigned nslots,
						      double * restrict result,
						      double lambda,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_linear_combination (unsigned nslots,
							      double * restrict result,
							      double alpha,
							      double * restrict operand1,
							      double beta,
							      double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_linspace (unsigned nslots,
						    double * restrict result,
						    double start, double past);
ccdoubles_decl void ccdoubles_real_vector_logspace (unsigned nslots,
						    double * restrict result,
						    double start, double past);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_exp (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_exp10 (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_exp2 (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_log (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_log10 (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_log2 (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_logb (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_pow (unsigned nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_sqrt (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_cbrt (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_hypot (unsigned nslots,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_expm1 (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_log1p (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_sin (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_cos (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_tan (unsigned nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_asin (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_acos (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atan (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atan2 (unsigned nslots,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_sinh (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_cosh (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_tanh (unsigned nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_asinh (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_acosh (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atanh (unsigned nslots,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_print_display (FILE * f, const char * name,
							 unsigned nslots,
							 double * operand);

ccdoubles_decl void ccdoubles_real_vector_print_brackets (FILE * f, unsigned nslots,
							  double * operand);

ccdoubles_decl void ccdoubles_real_vector_print_braces (FILE * f, unsigned nslots,
							double * operand);


/** --------------------------------------------------------------------
 ** Real matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_matrix_clear (unsigned nrows, unsigned ncols,
						 double * restrict matrix);
ccdoubles_decl void ccdoubles_real_matrix_set   (unsigned nrows, unsigned ncols,
						 double * restrict matrix,
						 double value);
ccdoubles_decl void ccdoubles_real_matrix_copy (unsigned nrows, unsigned ncols,
						double * restrict dst,
						double * restrict src);

ccdoubles_decl void ccdoubles_real_matrix_add (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_sub (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_mul (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_div (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_neg (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_abs (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_fmod (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand1,
						double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_drem (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand1,
						double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_remainder (unsigned nrows, unsigned ncols,
						     double * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_ceil (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_floor (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_trunc (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_round (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_rint (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_isgreater (unsigned nrows, unsigned ncols,
						     int * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isgreaterequal (unsigned nrows, unsigned ncols,
							  int * restrict result,
							  double * restrict operand1,
							  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isless (unsigned nrows, unsigned ncols,
						  int * restrict result,
						  double * restrict operand1,
						  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_islessequal (unsigned nrows, unsigned ncols,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_islessgreater (unsigned nrows, unsigned ncols,
							 int * restrict result,
							 double * restrict operand1,
							 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isunordered (unsigned nrows, unsigned ncols,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_min (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_max (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_fpclassify (unsigned nrows, unsigned ncols,
						      int * restrict result,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_isfinite (unsigned nrows, unsigned ncols,
						    int * restrict result,
						    double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_isinfinite (unsigned nrows, unsigned ncols,
						      int * restrict result,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_isnormal (unsigned nrows, unsigned ncols,
						    int * restrict result,
						    double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_isnan (unsigned nrows, unsigned ncols,
						 int * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_scalar_mul (unsigned nrows, unsigned ncols,
						      double * restrict result,
						      double lambda,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_linear_combination (unsigned nrows, unsigned ncols,
							      double * restrict result,
							      double alpha,
							      double * restrict operand1,
							      double beta,
							      double * restrict operand2);

ccdoubles_decl void ccdoubles_real_matrix_transpose (unsigned nrows, unsigned ncols,
						     double * restrict result,
						     double * restrict operand);

ccdoubles_decl void ccdoubles_real_matrix_rowcol_mul (unsigned result_nrows,
						      unsigned operand_n,
						      unsigned result_ncols,
						      double * restrict result,
						      double * restrict operand1,
						      double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_exp (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_exp10 (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_exp2 (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_log (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_log10 (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_log2 (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_logb (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_pow (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_sqrt (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_cbrt (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_hypot (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_expm1 (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_log1p (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_sin (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_cos (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_tan (unsigned nrows, unsigned ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_asin (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_acos (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atan (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atan2 (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);

ccdoubles_decl void ccdoubles_real_matrix_sinh (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_cosh (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_tanh (unsigned nrows, unsigned ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_asinh (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_acosh (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atanh (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_print_display (FILE * f, const char * name,
							 unsigned nrows, unsigned ncols,
							 double * operand);

ccdoubles_decl void ccdoubles_real_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
							  double * operand);

ccdoubles_decl void ccdoubles_real_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
							double * operand);


/** --------------------------------------------------------------------
 ** Complex vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_vector_clear (unsigned nslots,
						 double complex * restrict vector);
ccdoubles_decl void ccdoubles_cplx_vector_set   (unsigned nslots,
						 double complex * restrict vector,
						 double complex value);
ccdoubles_decl void ccdoubles_cplx_vector_copy (unsigned nslots,
						double complex * restrict dst,
						double complex * restrict src);

ccdoubles_decl void ccdoubles_cplx_vector_real (unsigned nslots,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_imag (unsigned nslots,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_magnitude (unsigned nslots,
						     double * restrict result,
						     double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_angle (unsigned nslots,
						 double * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_conj (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_from_rect (unsigned nslots,
						     double complex * restrict result,
						     double * restrict real,
						     double * restrict imag);
ccdoubles_decl void ccdoubles_cplx_vector_from_polar (unsigned nslots,
						      double complex * restrict result,
						      double * restrict magnitude,
						      double * restrict angle);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_add (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_sub (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_mul (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_div (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_neg (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl double complex ccdoubles_cplx_vector_scalar_product (unsigned nslots,
							      const double complex * restrict operand1,
							      const double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_scalar_mul (unsigned nslots,
						      double complex * restrict result,
						      double complex lambda,
						      double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_linear_combination (unsigned nslots,
							      double complex * restrict result,
							      double complex alpha,
							      double complex * restrict operand1,
							      double complex beta,
							      double complex * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_exp (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_log (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_log10 (unsigned nslots,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_sqrt (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_pow (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_sin (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_cos (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_tan (unsigned nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_asin (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_acos (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_atan (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_sinh (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_cosh (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_tanh (unsigned nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_asinh (unsigned nslots,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_acosh (unsigned nslots,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_atanh (unsigned nslots,
						 double complex * restrict result,
						 double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_print_display (FILE * f, const char * name,
							 unsigned nslots,
							 double complex * operand);

ccdoubles_decl void ccdoubles_cplx_vector_print_brackets (FILE * f, unsigned nslots,
							  double complex * operand);

ccdoubles_decl void ccdoubles_cplx_vector_print_braces (FILE * f, unsigned nslots,
							double complex * operand);


/** --------------------------------------------------------------------
 ** Complex matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_matrix_clear (unsigned nrows, unsigned ncols,
						 double complex * restrict matrix);
ccdoubles_decl void ccdoubles_cplx_matrix_set   (unsigned nrows, unsigned ncols,
						 double complex * restrict matrix,
						 double complex value);
ccdoubles_decl void ccdoubles_cplx_matrix_copy (unsigned nrows, unsigned ncols,
						double complex * restrict dst,
						double complex * restrict src);

ccdoubles_decl void ccdoubles_cplx_matrix_real (unsigned nrows, unsigned ncols,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_imag (unsigned nrows, unsigned ncols,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_magnitude (unsigned nrows, unsigned ncols,
						     double * restrict result,
						     double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_angle (unsigned nrows, unsigned ncols,
						 double * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_conj (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_from_rect (unsigned nrows, unsigned ncols,
						     double complex * restrict result,
						     double * restrict real,
						     double * restrict imag);
ccdoubles_decl void ccdoubles_cplx_matrix_from_polar (unsigned nrows, unsigned ncols,
						      double complex * restrict result,
						      double * restrict magnitude,
						      double * restrict angle);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_add (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_sub (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_mul (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_div (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_neg (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_scalar_mul (unsigned nrows, unsigned ncols,
						      double complex * restrict result,
						      double complex lambda,
						      double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_linear_combination (unsigned nrows, unsigned ncols,
							      double complex * restrict result,
							      double complex alpha,
							      double complex * restrict operand1,
							      double complex beta,
							      double complex * restrict operand2);

ccdoubles_decl void ccdoubles_cplx_matrix_transpose (unsigned nrows, unsigned ncols,
						     double complex * restrict result,
						     double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_matrix_conjugate_transpose (unsigned operand_nrows, unsigned operand_ncols,
							       double complex * restrict result,
							       double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_matrix_rowcol_mul (unsigned result_nrows,
						      unsigned operand_n,
						      unsigned result_ncols,
						      double complex * restrict result,
						      double complex * restrict operand1,
						      double complex * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_exp (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_log (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_log10 (unsigned nrows, unsigned ncols,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_sqrt (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_pow (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_sin (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_cos (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_tan (unsigned nrows, unsigned ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_asin (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_acos (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_atan (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_sinh (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_cosh (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_tanh (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_asinh (unsigned nrows, unsigned ncols,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_acosh (unsigned nrows, unsigned ncols,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_atanh (unsigned nrows, unsigned ncols,
						 double complex * restrict result,
						 double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_print_display (FILE * f, const char * name,
							 unsigned nrows, unsigned ncols,
							 double complex * operand);

ccdoubles_decl void ccdoubles_cplx_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
							  double complex * operand);

ccdoubles_decl void ccdoubles_cplx_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
							double complex * operand);


/** --------------------------------------------------------------------
 ** Arrays of integers.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_int_vector_clear (unsigned nslots, int * restrict vector);
ccdoubles_decl void ccdoubles_int_vector_set (unsigned nslots, int * restrict vector, int value);
ccdoubles_decl void ccdoubles_int_vector_copy (unsigned nslots, int * restrict dst, int * restrict src);

ccdoubles_decl void ccdoubles_int_vector_print_display (FILE * f, const char * name,
						     unsigned nslots,
						     int * operand);
ccdoubles_decl void ccdoubles_int_vector_print_brackets (FILE * f, unsigned nslots, int * operand);
ccdoubles_decl void ccdoubles_int_vector_print_braces (FILE * f, unsigned nslots, int * operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_int_matrix_clear (unsigned nrows, unsigned ncols, int * restrict matrix);
ccdoubles_decl void ccdoubles_int_matrix_set (unsigned nrows, unsigned ncols, int * restrict matrix, int value);
ccdoubles_decl void ccdoubles_int_matrix_copy (unsigned nrows, unsigned ncols, int * restrict dst, int * restrict src);

ccdoubles_decl void ccdoubles_int_matrix_print_display (FILE * f, const char * name,
						     unsigned nrows, unsigned ncols,
						     int * operand);
ccdoubles_decl void ccdoubles_int_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols, int * operand);
ccdoubles_decl void ccdoubles_int_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols, int * operand);


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl const char *	ccdoubles_version_string		(void);
ccdoubles_decl int		ccdoubles_version_interface_current	(void);
ccdoubles_decl int		ccdoubles_version_interface_revision	(void);
ccdoubles_decl int		ccdoubles_version_interface_age		(void);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CCDOUBLES_H */

/* end of file */
