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
ccdoubles_cplx_matrix_clear (unsigned nrows, unsigned ncols,
			     double complex * restrict matrix)
{
  ccdoubles_cplx_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_cplx_matrix_set (unsigned nrows, unsigned ncols,
			   double complex * restrict matrix,
			   double complex value)
{
  ccdoubles_cplx_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_cplx_matrix_set_split (unsigned nrows, unsigned ncols,
				 double complex * restrict matrix,
				 double value_re, double value_im)
{
  ccdoubles_cplx_vector_set_split(nrows * ncols, matrix, value_re, value_im);
}
void
ccdoubles_cplx_matrix_copy (unsigned nrows, unsigned ncols,
			    double complex * restrict dst,
			    double complex * restrict src)
{
  ccdoubles_cplx_vector_copy(nrows * ncols, dst, src);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_real (unsigned nrows, unsigned ncols,
			    double * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_real (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_imag (unsigned nrows, unsigned ncols,
			    double * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_imag (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_magnitude (unsigned nrows, unsigned ncols,
				 double * restrict result,
				 double complex * restrict operand)
{
  ccdoubles_cplx_vector_magnitude (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_angle (unsigned nrows, unsigned ncols,
			     double * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_angle (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_conj (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_conj (nrows * ncols, result, operand);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_from_rect (unsigned nrows, unsigned ncols,
				 double complex * restrict result,
				 double * restrict real,
				 double * restrict imag)
{
  ccdoubles_cplx_vector_from_rect (nrows * ncols, result, real, imag);
}
void
ccdoubles_cplx_matrix_from_polar (unsigned nrows, unsigned ncols,
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
ccdoubles_cplx_matrix_add (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_add(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_sub (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_sub(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_mul (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_mul(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_div (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_div(nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_cplx_matrix_neg (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_neg(nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Matrix operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_scalar_mul (unsigned nrows, unsigned ncols,
				  double complex * restrict result,
				  double complex lambda,
				  double complex * restrict operand)
{
  ccdoubles_cplx_vector_scalar_mul(nrows * ncols, result, lambda, operand);
}
void
ccdoubles_cplx_matrix_scalar_mul_split (unsigned nrows, unsigned ncols,
					double complex * restrict result,
					double lambda_re, double lambda_im,
					double complex * restrict operand)
{
  ccdoubles_cplx_vector_scalar_mul_split(nrows * ncols, result, lambda_re, lambda_im, operand);
}
void
ccdoubles_cplx_matrix_linear_combination (unsigned nrows, unsigned ncols,
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
ccdoubles_cplx_matrix_linear_combination_split (unsigned nrows, unsigned ncols,
						double complex * restrict result,
						double alpha_re, double alpha_im,
						double complex * restrict operand1,
						double beta_re, double beta_im,
						double complex * restrict operand2)
{
  ccdoubles_cplx_vector_linear_combination_split(nrows * ncols, result,
						 alpha_re, alpha_im, operand1,
						 beta_re, beta_im, operand2);
}
void
ccdoubles_cplx_matrix_transpose (unsigned operand_nrows, unsigned operand_ncols,
				 double complex * restrict result,
				 double complex * restrict operand)
/* To call this function we are meant to do:
 *
 *    #define Onrows	2
 *    #define Oncols	3
 *    #define Rnrows	Oncols
 *    #define Rncols	Onrows
 *    double complex	O[Onrows][Oncols];
 *    double complex	R[Rnrows][Rncols];
 *    ccdoubles_cplx_matrix_transpose (Onrows, Oncols, &R[0][0], &O[0][0]);
 */
{
  if ((result == operand) && (operand_nrows == operand_ncols)) {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=i+1; j<operand_ncols; ++j) {
	COMPLEX_SWAP(result[j * operand_nrows + i], operand[i * operand_ncols + j]);
      }
    }
  } else {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=0; j<operand_ncols; ++j) {
	result[j * operand_nrows + i] = operand[i * operand_ncols + j];
      }
    }
  }
}
void
ccdoubles_cplx_matrix_conjugate_transpose (unsigned operand_nrows, unsigned operand_ncols,
					   double complex * restrict result,
					   double complex * restrict operand)
/* To call this function we are meant to do:
 *
 *    double complex	O[2][3];
 *    double complex	R[3][2];
 *    ccdoubles_cplx_matrix_conjugate_transpose (2, 3, &R[0][0], &O[0][0]);
 */
{
  if ((result == operand) && (operand_nrows == operand_ncols)) {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=i; j<operand_ncols; ++j) {
	if (i != j) {
	  COMPLEX_SWAP_CONJ(result[j * operand_nrows + i], operand[i * operand_ncols + j]);
	} else {
	  result[j * operand_nrows + i] = conj(operand[i * operand_ncols + j]);
	}
      }
    }
  } else {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=0; j<operand_ncols; ++j) {
	result[j * operand_nrows + i] = conj(operand[i * operand_ncols + j]);
      }
    }
  }
}
void
ccdoubles_cplx_matrix_rowcol_mul (unsigned result_nrows,
				  unsigned operand_n,
				  unsigned result_ncols,
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
  for (unsigned i=0; i<result_nrows; ++i) {
    for (unsigned j=0; j<result_ncols; ++j) {
      double complex *	R = &(result[i * result_ncols + j]);
      *R = 0.0;
      for (unsigned k=0; k<operand_n; ++k) {
	*R += operand1[i * operand_n + k] * operand2[k * result_ncols + j];
      }
    }
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_exp (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_exp (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_log (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_log (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_log10 (unsigned nrows, unsigned ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_log10 (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_sqrt (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_sqrt (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_pow (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  ccdoubles_cplx_vector_pow (nrows * ncols, result, operand1, operand2);
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sin (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_sin (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_cos (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_cos (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_tan (unsigned nrows, unsigned ncols,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  ccdoubles_cplx_vector_tan (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asin (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_asin (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_acos (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_acos (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_atan (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_atan (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sinh (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_sinh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_cosh (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_cosh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_tanh (unsigned nrows, unsigned ncols,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  ccdoubles_cplx_vector_tanh (nrows * ncols, result, operand);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asinh (unsigned nrows, unsigned ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_asinh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_acosh (unsigned nrows, unsigned ncols,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  ccdoubles_cplx_vector_acosh (nrows * ncols, result, operand);
}
void
ccdoubles_cplx_matrix_atanh (unsigned nrows, unsigned ncols,
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
				     unsigned nrows, unsigned ncols,
				     double complex * operand)
{
  unsigned	i, j;
  fprintf(f, "Row-major matrix %s (dimension %u x %u) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    j = 0;
    fprintf(f, "| (%u,%u) %+lf%-+lfi ", 1+i, 1+j,
	    creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%u,%u) %+lf%-+lfi ", 1+i, 1+j,
	      creal(operand[i * ncols + j]), cimag(operand[i * ncols + j]));
    }
    fprintf(f, "|\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_cplx_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
				      double complex * operand)
{
  unsigned	i, j;
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
ccdoubles_cplx_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
				    double complex * operand)
{
  unsigned	i, j;
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
