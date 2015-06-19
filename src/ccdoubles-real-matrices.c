/*
  Part of: CCDoubles
  Contents: routines for matrices of real numbers
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

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
ccdoubles_real_matrix_clear (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t matrix)
{
  ccdoubles_real_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_real_matrix_set (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t matrix,
			   double value)
{
  ccdoubles_real_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_real_matrix_copy (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t dst,
			    ccdoubles_real_operand_t src)
{
  ccdoubles_real_vector_copy(nrows * ncols, dst, src);
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_add (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_add(nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_sub (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_sub(nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_mul (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_mul(nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_div (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_div(nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_neg (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_neg(nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_abs (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_abs(nrows * ncols, R, O);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_fmod (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O1,
			    ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_fmod (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_drem (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O1,
			    ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_drem (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_remainder (unsigned nrows, unsigned ncols,
				 ccdoubles_real_result_t R,
				 ccdoubles_real_operand_t O1,
				 ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_remainder (nrows * ncols, R, O1, O2);
}


/** --------------------------------------------------------------------
 ** Rounding.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_ceil (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_ceil(nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_floor (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_floor(nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_trunc (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_trunc(nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_round (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_round(nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_rint (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_rint(nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Comparison.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_isgreater (unsigned nrows, unsigned ncols,
				 ccdoubles_int_result_t R,
				 ccdoubles_real_operand_t O1,
				 ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_isgreater (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_isgreaterequal (unsigned nrows, unsigned ncols,
				      ccdoubles_int_result_t R,
				      ccdoubles_real_operand_t O1,
				      ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_isgreaterequal (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_isless (unsigned nrows, unsigned ncols,
			      ccdoubles_int_result_t R,
			      ccdoubles_real_operand_t O1,
			      ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_isless (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_islessequal (unsigned nrows, unsigned ncols,
				   ccdoubles_int_result_t R,
				   ccdoubles_real_operand_t O1,
				   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_islessequal (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_islessgreater (unsigned nrows, unsigned ncols,
				     ccdoubles_int_result_t R,
				     ccdoubles_real_operand_t O1,
				     ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_islessgreater (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_isunordered (unsigned nrows, unsigned ncols,
				   ccdoubles_int_result_t R,
				   ccdoubles_real_operand_t O1,
				   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_isunordered (nrows * ncols, R, O1, O2);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_min (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_min (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_max (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_max (nrows * ncols, R, O1, O2);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_fpclassify (unsigned nrows, unsigned ncols,
				  ccdoubles_int_result_t R,
				  ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_fpclassify (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_isfinite (unsigned nrows, unsigned ncols,
				ccdoubles_int_result_t R,
				ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_isfinite (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_isinfinite (unsigned nrows, unsigned ncols,
				ccdoubles_int_result_t R,
				ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_isinfinite (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_isnormal (unsigned nrows, unsigned ncols,
				ccdoubles_int_result_t R,
				ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_isnormal (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_isnan (unsigned nrows, unsigned ncols,
			     ccdoubles_int_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_isnan (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Matrix operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_scalar_mul (unsigned nrows, unsigned ncols,
				  ccdoubles_real_result_t R,
				  double lambda,
				  ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_scalar_mul(nrows * ncols, R, lambda, O);
}
void
ccdoubles_real_matrix_linear_combination (unsigned nrows, unsigned ncols,
					  ccdoubles_real_result_t R,
					  double alpha,
					  ccdoubles_real_operand_t O1,
					  double beta,
					  ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_linear_combination(nrows * ncols,
					   R, alpha, O1, beta, O2);
}
void
ccdoubles_real_matrix_transpose (unsigned operand_nrows, unsigned operand_ncols,
				 ccdoubles_real_result_t R,
				 ccdoubles_real_operand_t O)
/* To call this function we are meant to do:
 *
 *    #define Onrows	2
 *    #define Oncols	3
 *    #define Rnrows	Oncols
 *    #define Rncols	Onrows
 *    double	O[Onrows][Oncols];
 *    double	R[Rnrows][Rncols];
 *    ccdoubles_real_matrix_transpose (Onrows, Oncols, &R[0][0], &O[0][0]);
 */
{
  if ((R == O) && (operand_nrows == operand_ncols)) {
    for (unsigned i=0; i<operand_nrows; ++i) {
      for (unsigned j=i+1; j<operand_ncols; ++j) {
	REAL_SWAP(R[j * operand_nrows + i], R[i * operand_ncols + j]);
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
ccdoubles_real_matrix_rowcol_mul (unsigned result_nrows,
				  unsigned operand_n,
				  unsigned result_ncols,
				  ccdoubles_real_result_t R,
				  ccdoubles_real_operand_t O1,
				  ccdoubles_real_operand_t O2)
/* To call this function we are meant to do:
 *
 *
 *    double	R[result_nrows][result_ncols];
 *    double	O1[result_nrows][operand_n];
 *    double	O2[operand_n][result_ncols];
 *    ccdoubles_real_matrix_rowcol_mul(result_nrows, O_n, R_ncols,
 *                                     &R[0][0], &O1[0][0], &O2[0][0]);
 */
{
  for (unsigned i=0; i<result_nrows; ++i) {
    for (unsigned j=0; j<result_ncols; ++j) {
      double *	P = &R[i * result_ncols + j];
      *P = 0.0;
      for (unsigned k=0; k<operand_n; ++k) {
	*P += O1[i * operand_n + k] * O2[k * result_ncols + j];
      }
    }
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_linspace (unsigned nrows, unsigned ncols,
				ccdoubles_real_result_t R,
				double start,
				double row_past, double col_past)
{
  double	row_step = (row_past - start) / ((double)nrows);
  double	col_step = (col_past - start) / ((double)ncols);
  for (unsigned i=0; i<nrows; ++i) {
    double	row_first = ((double)i) * row_step + start;
    for (unsigned j=0; j<ncols; ++j) {
      R[i * ncols + j] = ((double)j) * col_step + row_first;
    }
  }
}
#if 0
void
ccdoubles_real_matrix_logspace (unsigned nrows, unsigned ncols,
				ccdoubles_real_result_t R,
				double start,
				double row_past, double col_past)
{
  double	row_step = (row_past - start) / ((double)nrows);
  double	col_step = (col_past - start) / ((double)ncols);
  for (unsigned i=0; i<nrows; ++i) {
    double	row_first = ((double)i) * row_step + start;
    for (unsigned j=0; j<ncols; ++j) {
      R[i * ncols + j] = pow(10.0, ((double)j) * col_step + row_first);
    }
  }
}
#endif


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_exp (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_exp (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_exp10 (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_exp10 (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_exp2 (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_exp2 (nrows * ncols, R, O);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_log (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_log (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_log10 (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_log10 (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_log2 (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_log2 (nrows * ncols, R, O);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_logb (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_logb (nrows * ncols, R, O);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_pow (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_pow (nrows * ncols, R, O1, O2);
}
void
ccdoubles_real_matrix_sqrt (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_sqrt (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_cbrt (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_cbrt (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_hypot (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O1,
			     ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_hypot (nrows * ncols, R, O1, O2);
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_matrix_expm1 (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_expm1 (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_log1p (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_log1p (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_sin (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_sin (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_cos (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_cos (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_tan (unsigned nrows, unsigned ncols,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_tan (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_asin (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_asin (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_acos (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_acos (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_atan (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_atan (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_atan2 (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O1,
			     ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_atan2 (nrows * ncols, R, O1, O2);
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_sinh (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_sinh (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_cosh (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_cosh (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_tanh (unsigned nrows, unsigned ncols,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_tanh (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_asinh (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_asinh (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_acosh (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_acosh (nrows * ncols, R, O);
}
void
ccdoubles_real_matrix_atanh (unsigned nrows, unsigned ncols,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  ccdoubles_real_vector_atanh (nrows * ncols, R, O);
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_row_to_row (unsigned dst_nrows, unsigned dst_ncols, ccdoubles_real_result_t  dst,
				  unsigned src_nrows, unsigned src_ncols, ccdoubles_real_operand_t src,
				  unsigned dst_row, unsigned dst_col,
				  unsigned src_row, unsigned src_col,
				  unsigned nslots)
{
  assert(dst_ncols >= dst_col + nslots);
  assert(src_ncols >= src_col + nslots);
  unsigned	dst_index = dst_row * dst_ncols + dst_col;
  unsigned	src_index = src_row * src_ncols + src_col;
  for (unsigned k=0; k<nslots; ++k, ++dst_index, ++src_index) {
    dst[dst_index] = src[src_index];
  }
}
void
ccdoubles_real_matrix_col_to_col (unsigned dst_nrows, unsigned dst_ncols, ccdoubles_real_result_t  dst,
				  unsigned src_nrows, unsigned src_ncols, ccdoubles_real_operand_t src,
				  unsigned dst_row, unsigned dst_col,
				  unsigned src_row, unsigned src_col,
				  unsigned nslots)
{
  assert(dst_nrows >= dst_row + nslots);
  assert(src_nrows >= src_row + nslots);
  unsigned	dst_index = dst_row * dst_ncols + dst_col;
  unsigned	src_index = src_row * src_ncols + src_col;
  for (unsigned k=0; k<nslots; ++k, dst_index+=dst_ncols, src_index+=src_ncols) {
    dst[dst_index] = src[src_index];
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_matrix_print_display (FILE * f, const char * name,
				     unsigned nrows, unsigned ncols,
				     ccdoubles_real_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "Row-major matrix %s (dimension %u x %u) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    j = 0;
    fprintf(f, "| (%u,%u) %+lf ", 1+i, 1+j, O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%u,%u) %+lf ", 1+i, 1+j, O[i * ncols + j]);
    }
    fprintf(f, "|\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_real_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
				      ccdoubles_real_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "[[%+lf", O[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+lf", O[j]);
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+lf", O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+lf", O[i * ncols + j]);
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_real_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
				    ccdoubles_real_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "{{%+lf", O[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+lf", O[j]);
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+lf", O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+lf", O[i * ncols + j]);
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */
