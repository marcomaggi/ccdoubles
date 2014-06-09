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

ccdoubles_decl void ccdoubles_real_vector_clear (size_t nslots,
						 double * restrict vector);
ccdoubles_decl void ccdoubles_real_vector_set   (size_t nslots,
						 double * restrict vector,
						 double value);
ccdoubles_decl void ccdoubles_real_vector_copy (size_t nslots,
						double * restrict dst,
						double * restrict src);

ccdoubles_decl void ccdoubles_real_vector_add (size_t nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_sub (size_t nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_mul (size_t nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_div (size_t nslots,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_neg (size_t nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_abs (size_t nslots,
					       double * restrict result,
					       double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_ceil (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_floor (size_t nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_trunc (size_t nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_round (size_t nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_rint (size_t nslots,
						double * restrict result,
						double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_isgreater (size_t nslots,
						     int * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isgreaterequal (size_t nslots,
							  int * restrict result,
							  double * restrict operand1,
							  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isless (size_t nslots,
						  int * restrict result,
						  double * restrict operand1,
						  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_islessequal (size_t nslots,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_islessgreater (size_t nslots,
							 int * restrict result,
							 double * restrict operand1,
							 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_isunordered (size_t nslots,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl double ccdoubles_real_vector_scalar_product (size_t nslots,
							    const double * restrict operand1,
							    const double * restrict operand2);
ccdoubles_decl void ccdoubles_real_vector_scalar_mul (size_t nslots,
						      double * restrict result,
						      double lambda,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_linear_combination (size_t nslots,
							      double * restrict result,
							      double alpha,
							      double * restrict operand1,
							      double beta,
							      double * restrict operand2);

ccdoubles_decl void ccdoubles_real_vector_sin (size_t nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_cos (size_t nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_tan (size_t nslots,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_asin (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_acos (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atan (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atan2 (size_t nslots,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);

ccdoubles_decl void ccdoubles_real_vector_sinh (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_cosh (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_tanh (size_t nslots,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_asinh (size_t nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_acosh (size_t nslots,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_vector_atanh (size_t nslots,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_vector_print_display (FILE * f, const char * name,
							 size_t nslots,
							 double * operand);

ccdoubles_decl void ccdoubles_real_vector_print_brackets (FILE * f, size_t nslots,
							  double * operand);

ccdoubles_decl void ccdoubles_real_vector_print_braces (FILE * f, size_t nslots,
							double * operand);


/** --------------------------------------------------------------------
 ** Real matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_real_matrix_clear (size_t nrows, size_t ncols,
						 double * restrict matrix);
ccdoubles_decl void ccdoubles_real_matrix_set   (size_t nrows, size_t ncols,
						 double * restrict matrix,
						 double value);
ccdoubles_decl void ccdoubles_real_matrix_copy (size_t nrows, size_t ncols,
						double * restrict dst,
						double * restrict src);

ccdoubles_decl void ccdoubles_real_matrix_add (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_sub (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_mul (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_div (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand1,
					       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_neg (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_abs (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_ceil (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_floor (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_trunc (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_round (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_rint (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_isgreater (size_t nrows, size_t ncols,
						     int * restrict result,
						     double * restrict operand1,
						     double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isgreaterequal (size_t nrows, size_t ncols,
							  int * restrict result,
							  double * restrict operand1,
							  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isless (size_t nrows, size_t ncols,
						  int * restrict result,
						  double * restrict operand1,
						  double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_islessequal (size_t nrows, size_t ncols,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_islessgreater (size_t nrows, size_t ncols,
							 int * restrict result,
							 double * restrict operand1,
							 double * restrict operand2);
ccdoubles_decl void ccdoubles_real_matrix_isunordered (size_t nrows, size_t ncols,
						       int * restrict result,
						       double * restrict operand1,
						       double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_scalar_mul (size_t nrows, size_t ncols,
						      double * restrict result,
						      double lambda,
						      double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_linear_combination (size_t nrows, size_t ncols,
							      double * restrict result,
							      double alpha,
							      double * restrict operand1,
							      double beta,
							      double * restrict operand2);

ccdoubles_decl void ccdoubles_real_matrix_transpose (size_t nrows, size_t ncols,
						     double * restrict result,
						     double * restrict operand);

ccdoubles_decl void ccdoubles_real_matrix_rowcol_mul (size_t result_nrows,
						      size_t operand_n,
						      size_t result_ncols,
						      double * restrict result,
						      double * restrict operand1,
						      double * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_sin (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_cos (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_tan (size_t nrows, size_t ncols,
					       double * restrict result,
					       double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_asin (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_acos (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atan (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atan2 (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand1,
						 double * restrict operand2);

ccdoubles_decl void ccdoubles_real_matrix_sinh (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_cosh (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_tanh (size_t nrows, size_t ncols,
						double * restrict result,
						double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_asinh (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_acosh (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);
ccdoubles_decl void ccdoubles_real_matrix_atanh (size_t nrows, size_t ncols,
						 double * restrict result,
						 double * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_real_matrix_print_display (FILE * f, const char * name,
							 size_t nrows, size_t ncols,
							 double * operand);

ccdoubles_decl void ccdoubles_real_matrix_print_brackets (FILE * f, size_t nrows, size_t ncols,
							  double * operand);

ccdoubles_decl void ccdoubles_real_matrix_print_braces (FILE * f, size_t nrows, size_t ncols,
							double * operand);


/** --------------------------------------------------------------------
 ** Complex vector functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_vector_clear (size_t nslots,
						 double complex * restrict vector);
ccdoubles_decl void ccdoubles_cplx_vector_set   (size_t nslots,
						 double complex * restrict vector,
						 double complex value);
ccdoubles_decl void ccdoubles_cplx_vector_copy (size_t nslots,
						double complex * restrict dst,
						double complex * restrict src);

ccdoubles_decl void ccdoubles_cplx_vector_real (size_t nslots,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_imag (size_t nslots,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_magnitude (size_t nslots,
						     double * restrict result,
						     double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_angle (size_t nslots,
						 double * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_conj (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_from_rect (size_t nslots,
						     double complex * restrict result,
						     double * restrict real,
						     double * restrict imag);
ccdoubles_decl void ccdoubles_cplx_vector_from_polar (size_t nslots,
						      double complex * restrict result,
						      double * restrict magnitude,
						      double * restrict angle);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_add (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_sub (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_mul (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_div (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_neg (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl double complex ccdoubles_cplx_vector_scalar_product (size_t nslots,
							      const double complex * restrict operand1,
							      const double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_vector_scalar_mul (size_t nslots,
						      double complex * restrict result,
						      double complex lambda,
						      double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_linear_combination (size_t nslots,
							      double complex * restrict result,
							      double complex alpha,
							      double complex * restrict operand1,
							      double complex beta,
							      double complex * restrict operand2);

ccdoubles_decl void ccdoubles_cplx_vector_sin (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_cos (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_tan (size_t nslots,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_asin (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_acos (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_atan (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_vector_sinh (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_cosh (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_tanh (size_t nslots,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_asinh (size_t nslots,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_acosh (size_t nslots,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_vector_atanh (size_t nslots,
						 double complex * restrict result,
						 double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_vector_print_display (FILE * f, const char * name,
							 size_t nslots,
							 double complex * operand);

ccdoubles_decl void ccdoubles_cplx_vector_print_brackets (FILE * f, size_t nslots,
							  double complex * operand);

ccdoubles_decl void ccdoubles_cplx_vector_print_braces (FILE * f, size_t nslots,
							double complex * operand);


/** --------------------------------------------------------------------
 ** Complex matrix functions.
 ** ----------------------------------------------------------------- */

ccdoubles_decl void ccdoubles_cplx_matrix_clear (size_t nrows, size_t ncols,
						 double complex * restrict matrix);
ccdoubles_decl void ccdoubles_cplx_matrix_set   (size_t nrows, size_t ncols,
						 double complex * restrict matrix,
						 double complex value);
ccdoubles_decl void ccdoubles_cplx_matrix_copy (size_t nrows, size_t ncols,
						double complex * restrict dst,
						double complex * restrict src);

ccdoubles_decl void ccdoubles_cplx_matrix_real (size_t nrows, size_t ncols,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_imag (size_t nrows, size_t ncols,
						double * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_magnitude (size_t nrows, size_t ncols,
						     double * restrict result,
						     double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_angle (size_t nrows, size_t ncols,
						 double * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_conj (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_from_rect (size_t nrows, size_t ncols,
						     double complex * restrict result,
						     double * restrict real,
						     double * restrict imag);
ccdoubles_decl void ccdoubles_cplx_matrix_from_polar (size_t nrows, size_t ncols,
						      double complex * restrict result,
						      double * restrict magnitude,
						      double * restrict angle);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_add (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_sub (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_mul (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_div (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand1,
					       double complex * restrict operand2);
ccdoubles_decl void ccdoubles_cplx_matrix_neg (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_scalar_mul (size_t nrows, size_t ncols,
						      double complex * restrict result,
						      double complex lambda,
						      double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_linear_combination (size_t nrows, size_t ncols,
							      double complex * restrict result,
							      double complex alpha,
							      double complex * restrict operand1,
							      double complex beta,
							      double complex * restrict operand2);

ccdoubles_decl void ccdoubles_cplx_matrix_transpose (size_t nrows, size_t ncols,
						     double complex * restrict result,
						     double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_matrix_conjugate_transpose (size_t operand_nrows, size_t operand_ncols,
							       double complex * restrict result,
							       double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_matrix_rowcol_mul (size_t result_nrows,
						      size_t operand_n,
						      size_t result_ncols,
						      double complex * restrict result,
						      double complex * restrict operand1,
						      double complex * restrict operand2);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_sin (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_cos (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_tan (size_t nrows, size_t ncols,
					       double complex * restrict result,
					       double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_asin (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_acos (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_atan (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);

ccdoubles_decl void ccdoubles_cplx_matrix_sinh (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_cosh (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_tanh (size_t nrows, size_t ncols,
						double complex * restrict result,
						double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_asinh (size_t nrows, size_t ncols,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_acosh (size_t nrows, size_t ncols,
						 double complex * restrict result,
						 double complex * restrict operand);
ccdoubles_decl void ccdoubles_cplx_matrix_atanh (size_t nrows, size_t ncols,
						 double complex * restrict result,
						 double complex * restrict operand);

/* ------------------------------------------------------------------ */

ccdoubles_decl void ccdoubles_cplx_matrix_print_display (FILE * f, const char * name,
							 size_t nrows, size_t ncols,
							 double complex * operand);

ccdoubles_decl void ccdoubles_cplx_matrix_print_brackets (FILE * f, size_t nrows, size_t ncols,
							  double complex * operand);

ccdoubles_decl void ccdoubles_cplx_matrix_print_braces (FILE * f, size_t nrows, size_t ncols,
							double complex * operand);


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
