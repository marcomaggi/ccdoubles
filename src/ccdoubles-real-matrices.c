/*
  Part of: CCDoubles
  Contents: routines for matrices of real numbers
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ccdoubles-internals.h"
#include <string.h>


/** --------------------------------------------------------------------
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_clear (size_t nrows, size_t ncols,
			     double * restrict matrix)
{
  ccdoubles_real_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_real_matrix_set (size_t nrows, size_t ncols,
			   double * restrict matrix,
			   double value)
{
  ccdoubles_real_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_real_matrix_copy (size_t nrows, size_t ncols,
			    double * restrict dst,
			    double * restrict src)
{
  ccdoubles_real_vector_copy(nrows * ncols, dst, src);
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_add (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_add(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_sub (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_sub(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_mul (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_mul(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_div (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_div(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_neg (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand)
{
  ccdoubles_real_vector_neg(nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_abs (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand)
{
  ccdoubles_real_vector_abs(nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Rounding.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_ceil (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_ceil(nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_floor (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_floor(nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_trunc (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_trunc(nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_round (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_round(nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_rint (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_rint(nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Matrix operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_scalar_mul (size_t nrows, size_t ncols,
				  double * restrict result,
				  double lambda,
				  double * restrict operand)
{
  ccdoubles_real_vector_scalar_mul(nrows * ncols, result, lambda, operand);
}
void
ccdoubles_real_matrix_linear_combination (size_t nrows, size_t ncols,
					  double * restrict result,
					  double alpha,
					  double * restrict operand1,
					  double beta,
					  double * restrict operand2)
{
  ccdoubles_real_vector_linear_combination(nrows * ncols,
					   result, alpha, operand1, beta, operand2);
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_sin (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand)
{
  ccdoubles_real_vector_sin (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_cos (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand)
{
  ccdoubles_real_vector_cos (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_tan (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand)
{
  ccdoubles_real_vector_tan (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_asin (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_asin (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_acos (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_acos (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_atan (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_atan (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_atan2 (size_t nrows, size_t ncols,
			     double * restrict result,
			     double * restrict operand1,
			     double * restrict operand2)
{
  ccdoubles_real_vector_atan2 (nrows * ncols, result, operand1, operand2);
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_sinh (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_sinh (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_cosh (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_cosh (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_tanh (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand)
{
  ccdoubles_real_vector_tanh (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_asinh (size_t nrows, size_t ncols,
			     double * restrict result,
			     double * restrict operand)
{
  ccdoubles_real_vector_asinh (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_acosh (size_t nrows, size_t ncols,
			     double * restrict result,
			     double * restrict operand)
{
  ccdoubles_real_vector_acosh (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_atanh (size_t nrows, size_t ncols,
			     double * restrict result,
			     double * restrict operand)
{
  ccdoubles_real_vector_atanh (nrows * ncols, result, operand);
}

/* end of file */
