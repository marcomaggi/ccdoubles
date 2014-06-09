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

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_fmod (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  ccdoubles_real_vector_fmod (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_drem (size_t nrows, size_t ncols,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  ccdoubles_real_vector_drem (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_remainder (size_t nrows, size_t ncols,
				 double * restrict result,
				 double * restrict operand1,
				 double * restrict operand2)
{
  ccdoubles_real_vector_remainder (nrows * ncols, result, operand1, operand2);
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
 ** Comparison.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_isgreater (size_t nrows, size_t ncols,
				 int * restrict result,
				 double * restrict operand1,
				 double * restrict operand2)
{
  ccdoubles_real_vector_isgreater (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_isgreaterequal (size_t nrows, size_t ncols,
				      int * restrict result,
				      double * restrict operand1,
				      double * restrict operand2)
{
  ccdoubles_real_vector_isgreaterequal (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_isless (size_t nrows, size_t ncols,
			      int * restrict result,
			      double * restrict operand1,
			      double * restrict operand2)
{
  ccdoubles_real_vector_isless (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_islessequal (size_t nrows, size_t ncols,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  ccdoubles_real_vector_islessequal (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_islessgreater (size_t nrows, size_t ncols,
				     int * restrict result,
				     double * restrict operand1,
				     double * restrict operand2)
{
  ccdoubles_real_vector_islessgreater (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_isunordered (size_t nrows, size_t ncols,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  ccdoubles_real_vector_isunordered (nrows * ncols, result, operand1, operand2);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_min (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_min (nrows * ncols, result, operand1, operand2);
}
void
ccdoubles_real_matrix_max (size_t nrows, size_t ncols,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  ccdoubles_real_vector_max (nrows * ncols, result, operand1, operand2);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_fpclassify (size_t nrows, size_t ncols,
				  int * restrict result,
				  double * restrict operand)
{
  ccdoubles_real_vector_fpclassify (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_isfinite (size_t nrows, size_t ncols,
				int * restrict result,
				double * restrict operand)
{
  ccdoubles_real_vector_isfinite (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_isinfinite (size_t nrows, size_t ncols,
				int * restrict result,
				double * restrict operand)
{
  ccdoubles_real_vector_isinfinite (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_isnormal (size_t nrows, size_t ncols,
				int * restrict result,
				double * restrict operand)
{
  ccdoubles_real_vector_isnormal (nrows * ncols, result, operand);
}
void
ccdoubles_real_matrix_isnan (size_t nrows, size_t ncols,
			     int * restrict result,
			     double * restrict operand)
{
  ccdoubles_real_vector_isnan (nrows * ncols, result, operand);
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
void
ccdoubles_real_matrix_transpose (size_t operand_nrows, size_t operand_ncols,
				 double * restrict result,
				 double * restrict operand)
/* To call this function we are meant to do:
 *
 *    double	O[2][3];
 *    double	R[3][2];
 *    ccdoubles_real_matrix_transpose (2, 3, &R[0][0], &O[0][0]);
 */
{
  for (size_t i=0; i<operand_nrows; ++i) {
    for (size_t j=0; j<operand_ncols; ++j) {
      result[j * operand_nrows + i] = operand[i * operand_ncols + j];
      if (0) {
	printf("i=%ld, j=%ld, R=%lf, O=%lf\n", i, j,
	       result[j * operand_nrows + i],
	       operand[i * operand_ncols + j]);
      }
    }
  }
}
void
ccdoubles_real_matrix_rowcol_mul (size_t result_nrows,
				  size_t operand_n,
				  size_t result_ncols,
				  double * restrict result,
				  double * restrict operand1,
				  double * restrict operand2)
/* To call this function we are meant to do:
 *
 *
 *    double	R[result_nrows][result_ncols];
 *    double	O1[result_nrows][operand_n];
 *    double	O2[operand_n][result_ncols];
 *    ccdoubles_real_matrix_rowcol_mul(result_nrows, operand_n, result_ncols,
 *                                     &R[0][0], &O1[0][0], &O2[0][0]);
 */
{
  for (size_t i=0; i<result_nrows; ++i) {
    for (size_t j=0; j<result_ncols; ++j) {
      double *	R = &result[i * result_ncols + j];
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


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_print_display (FILE * f, const char * name,
				     size_t nrows, size_t ncols,
				     double * operand)
{
  size_t	i, j;
  fprintf(f, "Row-major matrix %s (dimension %ld x %ld) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    j = 0;
    fprintf(f, "| (%ld,%ld) %+lf ", 1+i, 1+j, operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%ld,%ld) %+lf ", 1+i, 1+j, operand[i * ncols + j]);
    }
    fprintf(f, " |\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_real_matrix_print_brackets (FILE * f, size_t nrows, size_t ncols,
				      double * operand)
{
  size_t	i, j;
  fprintf(f, "[[%+lf", operand[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+lf", operand[j]);
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+lf", operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+lf", operand[i * ncols + j]);
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_real_matrix_print_braces (FILE * f, size_t nrows, size_t ncols,
				    double * operand)
{
  size_t	i, j;
  fprintf(f, "{{%+lf", operand[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+lf", operand[j]);
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+lf", operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+lf", operand[i * ncols + j]);
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */
