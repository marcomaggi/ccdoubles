/*
  Part of: CCDoubles
  Contents: routines for matrices of real numbers
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is free  software: you can redistribute it and/or  modify it under the
  terms of the  GNU Lesser General Public  License as published by  the Free Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
  WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License along with
  this program.  If not, see <http://www.gnu.org/licenses/>.
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
			     ccdoubles_cplx_result_t M)
{
  ccdoubles_cplx_vector_clear(nrows * ncols, M);
}
void
ccdoubles_cplx_matrix_set (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t M,
			   double complex value)
{
  ccdoubles_cplx_vector_set(nrows * ncols, M, value);
}
void
ccdoubles_cplx_matrix_set_split (unsigned nrows, unsigned ncols,
				 ccdoubles_cplx_result_t M,
				 double value_re, double value_im)
{
  ccdoubles_cplx_vector_set_split(nrows * ncols, M, value_re, value_im);
}
void
ccdoubles_cplx_matrix_copy (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t  dst,
			    ccdoubles_cplx_operand_t src)
{
  ccdoubles_cplx_vector_copy(nrows * ncols, dst, src);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_real (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_real (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_imag (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_imag (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_magnitude (unsigned nrows, unsigned ncols,
				 ccdoubles_real_result_t R,
				 ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_magnitude (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_angle (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_angle (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_conj (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_conj (nrows * ncols, R, O);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_matrix_from_rect (unsigned nrows, unsigned ncols,
				 ccdoubles_cplx_result_t R,
				 ccdoubles_real_operand_t real,
				 ccdoubles_real_operand_t imag)
{
  ccdoubles_cplx_vector_from_rect (nrows * ncols, R, real, imag);
}
void
ccdoubles_cplx_matrix_from_polar (unsigned nrows, unsigned ncols,
				  ccdoubles_cplx_result_t R,
				  ccdoubles_real_operand_t magnitude,
				  ccdoubles_real_operand_t angle)
{
  ccdoubles_cplx_vector_from_polar (nrows * ncols, R, magnitude, angle);
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_add (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_add(nrows * ncols, R, O1, O2);
}
void
ccdoubles_cplx_matrix_sub (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_sub(nrows * ncols, R, O1, O2);
}
void
ccdoubles_cplx_matrix_mul (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_mul(nrows * ncols, R, O1, O2);
}
void
ccdoubles_cplx_matrix_div (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_div(nrows * ncols, R, O1, O2);
}
void
ccdoubles_cplx_matrix_neg (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_neg(nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Matrix operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_scalar_mul (unsigned nrows, unsigned ncols,
				  ccdoubles_cplx_result_t R,
				  double complex lambda,
				  ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_scalar_mul(nrows * ncols, R, lambda, O);
}
void
ccdoubles_cplx_matrix_scalar_mul_split (unsigned nrows, unsigned ncols,
					ccdoubles_cplx_result_t R,
					double lambda_re, double lambda_im,
					ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_scalar_mul_split(nrows * ncols, R, lambda_re, lambda_im, O);
}
void
ccdoubles_cplx_matrix_linear_combination (unsigned nrows, unsigned ncols,
					  ccdoubles_cplx_result_t R,
					  double complex alpha,
					  ccdoubles_cplx_operand_t O1,
					  double complex beta,
					  ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_linear_combination(nrows * ncols,
					   R, alpha, O1, beta, O2);
}
void
ccdoubles_cplx_matrix_linear_combination_split (unsigned nrows, unsigned ncols,
						ccdoubles_cplx_result_t R,
						double alpha_re, double alpha_im,
						ccdoubles_cplx_operand_t O1,
						double beta_re, double beta_im,
						ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_linear_combination_split(nrows * ncols, R,
						 alpha_re, alpha_im, O1,
						 beta_re, beta_im, O2);
}
void
ccdoubles_cplx_matrix_transpose (unsigned operand_nrows, unsigned operand_ncols,
				 ccdoubles_cplx_result_t R,
				 ccdoubles_cplx_operand_t O)
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
  if ((R == O) && (operand_nrows == operand_ncols)) {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=i+1; j<operand_ncols; ++j) {
	COMPLEX_SWAP(R[j * operand_nrows + i], R[i * operand_ncols + j]);
      }
    }
  } else {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=0; j<operand_ncols; ++j) {
	R[j * operand_nrows + i] = O[i * operand_ncols + j];
      }
    }
  }
}
void
ccdoubles_cplx_matrix_conjugate_transpose (unsigned operand_nrows, unsigned operand_ncols,
					   ccdoubles_cplx_result_t R,
					   ccdoubles_cplx_operand_t O)
/* To call this function we are meant to do:
 *
 *    double complex	O[2][3];
 *    double complex	R[3][2];
 *    ccdoubles_cplx_matrix_conjugate_transpose (2, 3, &R[0][0], &O[0][0]);
 */
{
  if ((R == O) && (operand_nrows == operand_ncols)) {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=i; j<operand_ncols; ++j) {
	if (i != j) {
	  COMPLEX_SWAP_CONJ(R[j * operand_nrows + i], R[i * operand_ncols + j]);
	} else {
	  R[j * operand_nrows + i] = conj(R[i * operand_ncols + j]);
	}
      }
    }
  } else {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=0; j<operand_ncols; ++j) {
	R[j * operand_nrows + i] = conj(O[i * operand_ncols + j]);
      }
    }
  }
}
void
ccdoubles_cplx_matrix_rowcol_mul (unsigned result_nrows,
				  unsigned operand_n,
				  unsigned result_ncols,
				  ccdoubles_cplx_result_t R,
				  ccdoubles_cplx_operand_t O1,
				  ccdoubles_cplx_operand_t O2)
/* To call this function we are meant to do:
 *
 *
 *    double complex	R[result_nrows][result_ncols];
 *    double complex	O1[result_nrows][operand_n];
 *    double complex	O2[operand_n][result_ncols];
 *    ccdoubles_cplx_matrix_rowcol_mul(result_nrows, O_n, R_ncols,
 *                                     &R[0][0], &O1[0][0], &O2[0][0]);
 */
{
  for (unsigned i=0; i<result_nrows; ++i) {
    for (unsigned j=0; j<result_ncols; ++j) {
      double complex *	P = &(R[i * result_ncols + j]);
      *P = 0.0;
      for (unsigned k=0; k<operand_n; ++k) {
	*P += O1[i * operand_n + k] * O2[k * result_ncols + j];
      }
    }
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_exp (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_exp (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_log (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_log (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_log10 (unsigned nrows, unsigned ncols,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_log10 (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_sqrt (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_sqrt (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_pow (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  ccdoubles_cplx_vector_pow (nrows * ncols, R, O1, O2);
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sin (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_sin (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_cos (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_cos (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_tan (unsigned nrows, unsigned ncols,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_tan (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asin (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_asin (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_acos (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_acos (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_atan (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_atan (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_sinh (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_sinh (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_cosh (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_cosh (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_tanh (unsigned nrows, unsigned ncols,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_tanh (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_asinh (unsigned nrows, unsigned ncols,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_asinh (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_acosh (unsigned nrows, unsigned ncols,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_acosh (nrows * ncols, R, O);
}
void
ccdoubles_cplx_matrix_atanh (unsigned nrows, unsigned ncols,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  ccdoubles_cplx_vector_atanh (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_matrix_print_display (FILE * f, const char * name,
				     unsigned nrows, unsigned ncols,
				     ccdoubles_cplx_operand_t O)
{
  unsigned	i;
  fprintf(f, "Row-major matrix %s (dimension %u x %u) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    unsigned	j = 0;
    fprintf(f, "| (%u,%u) %+lf%-+lfi ", 1+i, 1+j,
	    creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%u,%u) %+lf%-+lfi ", 1+i, 1+j,
	      creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    }
    fprintf(f, "|\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_cplx_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
				      ccdoubles_cplx_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "[[%+lf%-+lfi", creal(O[0]), cimag(O[0]));
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+lf%-+lfi", creal(O[j]), cimag(O[j]));
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+lf%-+lfi", creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+lf%-+lfi", creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_cplx_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
				    ccdoubles_cplx_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "{{%+lf%-+lfi", creal(O[0]), cimag(O[0]));
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+lf%-+lfi", creal(O[j]), cimag(O[j]));
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+lf%-+lfi", creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+lf%-+lfi", creal(O[i * ncols + j]), cimag(O[i * ncols + j]));
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */
