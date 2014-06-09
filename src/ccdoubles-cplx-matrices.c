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
ccdoubles_cplx_matrix_clear (size_t nrows, size_t ncols,
			     double complex * restrict matrix)
{
  ccdoubles_cplx_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_cplx_matrix_set (size_t nrows, size_t ncols,
			   double complex * restrict matrix,
			   double complex value)
{
  ccdoubles_cplx_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_cplx_matrix_copy (size_t nrows, size_t ncols,
			    double complex * restrict dst,
			    double complex * restrict src)
{
  ccdoubles_cplx_vector_copy(nrows * ncols, dst, src);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_real (size_t nrows, size_t ncols,
			    double * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_real (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_imag (size_t nrows, size_t ncols,
			    double * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_imag (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_magnitude (size_t nrows, size_t ncols,
				 double * restrict result,
				 double complex * restrict operand)
{
  ccdoubles_cplx_vector_magnitude (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_angle (size_t nrows, size_t ncols,
			     double * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_angle (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_conj (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_conj (nrows * ncols, result, operand);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_from_rect (size_t nrows, size_t ncols,
				 double complex * restrict result,
				 double * restrict real,
				 double * restrict imag)
{
  ccdoubles_cplx_vector_from_rect (nrows * ncols, result, real, imag);
}
void
ccdoubles_cplx_matrix_from_polar (size_t nrows, size_t ncols,
				  double complex * restrict result,
				  double * restrict magnitude,
				  double * restrict angle)
{
  ccdoubles_cplx_vector_from_polar (nrows * ncols, result, magnitude, angle);
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_add (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_add(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_sub (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_sub(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_mul (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_mul(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_div (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_div(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_neg (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_neg(nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Matrix operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_scalar_mul (size_t nrows, size_t ncols,
				  double complex * restrict result,
				  double complex lambda,
				  double complex * restrict operand)
{
  ccdoubles_cplx_vector_scalar_mul(nrows * ncols, result, lambda, operand);
}
void
ccdoubles_cplx_matrix_linear_combination (size_t nrows, size_t ncols,
					  double complex * restrict result,
					  double complex alpha,
					  double complex * restrict operand1,
					  double complex beta,
					  double complex * restrict operand2)
{
  ccdoubles_cplx_vector_linear_combination(nrows * ncols,
					   result, alpha, operand1, beta, operand2);
}
void
ccdoubles_cplx_matrix_transpose (size_t operand_nrows, size_t operand_ncols,
				 double complex * restrict result,
				 double complex * restrict operand)
/* To call this function we are meant to do:
 *
 *    double complex	O[2][3];
 *    double complex	R[3][2];
 *    ccdoubles_cplx_matrix_transpose (2, 3, &R[0][0], &O[0][0]);
 */
{
  for (size_t i=0; i<operand_nrows; ++i) {
    for (size_t j=0; j<operand_ncols; ++j) {
      result[j * operand_nrows + i] = operand[i * operand_ncols + j];
    }
  }
}
void
ccdoubles_cplx_matrix_conjugate_transpose (size_t operand_nrows, size_t operand_ncols,
					   double complex * restrict result,
					   double complex * restrict operand)
/* To call this function we are meant to do:
 *
 *    double complex	O[2][3];
 *    double complex	R[3][2];
 *    ccdoubles_cplx_matrix_conjugate_transpose (2, 3, &R[0][0], &O[0][0]);
 */
{
  for (size_t i=0; i<operand_nrows; ++i) {
    for (size_t j=0; j<operand_ncols; ++j) {
      if (i != j) {
	result[j * operand_nrows + i] = conj(operand[i * operand_ncols + j]);
      } else {
	result[j * operand_nrows + i] = operand[i * operand_ncols + j];
      }
    }
  }
}
void
ccdoubles_cplx_matrix_rowcol_mul (size_t result_nrows,
				  size_t operand_n,
				  size_t result_ncols,
				  double complex * restrict result,
				  double complex * restrict operand1,
				  double complex * restrict operand2)
/* To call this function we are meant to do:
 *
 *
 *    double complex	R[result_nrows][result_ncols];
 *    double complex	O1[result_nrows][operand_n];
 *    double complex	O2[operand_n][result_ncols];
 *    ccdoubles_cplx_matrix_rowcol_mul(result_nrows, operand_n, result_ncols,
 *                                     &R[0][0], &O1[0][0], &O2[0][0]);
 */
{
  for (size_t i=0; i<result_nrows; ++i) {
    for (size_t j=0; j<result_ncols; ++j) {
      double complex *	R = &(result[i * result_ncols + j]);
      *R = 0.0;
      for (size_t k=0; k<operand_n; ++k) {
	*R += operand1[i * operand_n + k] * operand2[k * result_ncols + j];
      }
    }
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sin (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_sin (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_cos (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_cos (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_tan (size_t nrows, size_t ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_tan (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asin (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_asin (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_acos (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_acos (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_atan (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_atan (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sinh (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_sinh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_cosh (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_cosh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_tanh (size_t nrows, size_t ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_tanh (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asinh (size_t nrows, size_t ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_asinh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_acosh (size_t nrows, size_t ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_acosh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_atanh (size_t nrows, size_t ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_atanh (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_print_display (FILE * f, const char * name,
				     size_t nrows, size_t ncols,
				     double complex * operand)
{
  size_t	i, j;
  fprintf(f, "Row-major matrix %s (dimension %ld x %ld) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    j = 0;
    fprintf(f, "| (%ld,%ld) %+lf%-+lfi ", 1+i, 1+j,
	    creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%ld,%ld) %+lf%-+lfi ", 1+i, 1+j,
	      creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    }
    fprintf(f, " |\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_cplx_matrix_print_brackets (FILE * f, size_t nrows, size_t ncols,
				      double complex * operand)
{
  size_t	i, j;
  fprintf(f, "[[%+lf%-+lfi", creal(operand[0]), cimag(operand[0]));
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+lf%-+lfi", creal(operand[j]), cimag(operand[j]));
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+lf%-+lfi", creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+lf%-+lfi", creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_cplx_matrix_print_braces (FILE * f, size_t nrows, size_t ncols,
				    double complex * operand)
{
  size_t	i, j;
  fprintf(f, "{{%+lf%-+lfi", creal(operand[0]), cimag(operand[0]));
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+lf%-+lfi", creal(operand[j]), cimag(operand[j]));
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+lf%-+lfi", creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+lf%-+lfi", creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */
