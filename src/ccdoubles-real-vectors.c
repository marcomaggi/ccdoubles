/*
  Part of: CCDoubles
  Contents: routines for vectors
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014, 2015, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it  under the  terms  of  the GNU  Lesser  General  Public License  as
  published by  the Free  Software Foundation, either  version 3  of the
  License, or (at your option) any later version.

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
ccdoubles_real_vector_clear (unsigned nslots, ccdoubles_real_result_t vector)
{
  memset(vector, 0, sizeof(double) * nslots);
}
void
ccdoubles_real_vector_set (unsigned nslots, ccdoubles_real_result_t vector, double value)
{
  for (unsigned i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_real_vector_copy (unsigned nslots,
			    ccdoubles_real_result_t dst,
			    ccdoubles_real_operand_t src)
{
  if (1) {
    memcpy(dst, src, sizeof(double) * nslots);
  } else {
    for (unsigned i=0; i<nslots; ++i) {
      dst[i] = src[i];
    }
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_add (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] + O2[i];
  }
}
void
ccdoubles_real_vector_sub (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] - O2[i];
  }
}
void
ccdoubles_real_vector_mul (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] * O2[i];
  }
}
void
ccdoubles_real_vector_div (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] / O2[i];
  }
}
void
ccdoubles_real_vector_neg (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = - O[i];
  }
}
void
ccdoubles_real_vector_abs (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = fabs(O[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fmod (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O1,
			    ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = fmod(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_drem (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O1,
			    ccdoubles_real_operand_t O2)
{
  ccdoubles_real_vector_remainder(nslots, R, O1, O2);
}
void
ccdoubles_real_vector_remainder (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O1,
			    ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = remainder(O1[i], O2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Rounding.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_ceil (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ceil(O[i]);
  }
}
void
ccdoubles_real_vector_floor (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = floor(O[i]);
  }
}
void
ccdoubles_real_vector_trunc (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = trunc(O[i]);
  }
}
void
ccdoubles_real_vector_round (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = round(O[i]);
  }
}
void
ccdoubles_real_vector_rint (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = rint(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Comparison.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_isgreater (unsigned nslots,
				 ccdoubles_int_result_t R,
				 ccdoubles_real_operand_t O1,
				 ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isgreater(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_isgreaterequal (unsigned nslots,
				      ccdoubles_int_result_t R,
				      ccdoubles_real_operand_t O1,
				      ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isgreaterequal(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_isless (unsigned nslots,
			      ccdoubles_int_result_t R,
			      ccdoubles_real_operand_t O1,
			      ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isless(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_islessequal (unsigned nslots,
				   ccdoubles_int_result_t R,
				   ccdoubles_real_operand_t O1,
				   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = islessequal(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_islessgreater (unsigned nslots,
				     ccdoubles_int_result_t R,
				     ccdoubles_real_operand_t O1,
				     ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = islessgreater(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_isunordered (unsigned nslots,
				   ccdoubles_int_result_t R,
				   ccdoubles_real_operand_t O1,
				   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isunordered(O1[i], O2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_min (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = fmin(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_max (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = fmax(O1[i], O2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fpclassify (unsigned nslots,
				  ccdoubles_int_result_t R,
				  ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = fpclassify(O[i]);
  }
}
void
ccdoubles_real_vector_isfinite (unsigned nslots,
				ccdoubles_int_result_t R,
				ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isfinite(O[i]);
  }
}
void
ccdoubles_real_vector_isinfinite (unsigned nslots,
				  ccdoubles_int_result_t R,
				  ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isfinite(O[i]) ? 0 : 1;
  }
}
void
ccdoubles_real_vector_isnormal (unsigned nslots,
				ccdoubles_int_result_t R,
				ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isnormal(O[i]);
  }
}
void
ccdoubles_real_vector_isnan (unsigned nslots,
			     ccdoubles_int_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = isnan(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double
ccdoubles_real_vector_scalar_product (unsigned nslots,
				      const ccdoubles_real_operand_t O1,
				      const ccdoubles_real_operand_t O2)
{
  double	result = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    result += O1[i] * O2[i];
  }
  return result;
}
void
ccdoubles_real_vector_scalar_mul (unsigned nslots,
				  ccdoubles_real_result_t R,
				  double lambda,
				  ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = lambda * O[i];
  }
}
void
ccdoubles_real_vector_linear_combination (unsigned nslots,
					  ccdoubles_real_result_t R,
					  double alpha,
					  ccdoubles_real_operand_t O1,
					  double beta,
					  ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = alpha * O1[i] + beta * O2[i];
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_linspace (unsigned nslots,
				ccdoubles_real_result_t R,
				double start, double past)
{
  double	step = (past - start) / ((double)nslots);
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ((double)i) * step + start;
  }
}
void
ccdoubles_real_vector_logspace (unsigned nslots,
				ccdoubles_real_result_t R,
				double start, double past)
{
  double	step = (past - start) / ((double)nslots);
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = pow(10.0, ((double)i) * step + start);
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_exp (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = exp(O[i]);
  }
}
void
ccdoubles_real_vector_exp10 (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = exp(O[i] * log(10.0));
  }
}
void
ccdoubles_real_vector_exp2 (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = exp2(O[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_log (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = log(O[i]);
  }
}
void
ccdoubles_real_vector_log10 (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = log10(O[i]);
  }
}
void
ccdoubles_real_vector_log2 (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = log2(O[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_logb (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = logb(O[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_pow (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O1,
			   ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = pow(O1[i], O2[i]);
  }
}
void
ccdoubles_real_vector_sqrt (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = sqrt(O[i]);
  }
}
void
ccdoubles_real_vector_cbrt (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cbrt(O[i]);
  }
}
void
ccdoubles_real_vector_hypot (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O1,
			     ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = hypot(O1[i], O2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_expm1 (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = expm1(O[i]);
  }
}
void
ccdoubles_real_vector_log1p (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = log1p(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sin (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = sin(O[i]);
  }
}
void
ccdoubles_real_vector_cos (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cos(O[i]);
  }
}
void
ccdoubles_real_vector_tan (unsigned nslots,
			   ccdoubles_real_result_t R,
			   ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = tan(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asin (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = asin(O[i]);
  }
}
void
ccdoubles_real_vector_acos (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = acos(O[i]);
  }
}
void
ccdoubles_real_vector_atan (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = atan(O[i]);
  }
}
void
ccdoubles_real_vector_atan2 (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O1,
			     ccdoubles_real_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = atan2(O1[i], O2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sinh (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = sinh(O[i]);
  }
}
void
ccdoubles_real_vector_cosh (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cosh(O[i]);
  }
}
void
ccdoubles_real_vector_tanh (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = tanh(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asinh (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = asinh(O[i]);
  }
}
void
ccdoubles_real_vector_acosh (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = acosh(O[i]);
  }
}
void
ccdoubles_real_vector_atanh (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_real_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = atanh(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Subvector operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_copy_forward (unsigned dst_start, unsigned src_start, unsigned nslots,
				    ccdoubles_real_result_t dst, ccdoubles_real_operand_t src)
{
  unsigned	i_end = dst_start + nslots;
  for (unsigned i=dst_start, j=src_start; i<i_end; ++i, ++j) {
    dst[i] = src[j];
  }
}
void
ccdoubles_real_vector_copy_backward (unsigned dst_start, unsigned src_start, unsigned nslots,
				     ccdoubles_real_result_t dst, ccdoubles_real_operand_t src)
{
  unsigned	dst_first = dst_start + nslots - 1;
  unsigned	src_first = src_start + nslots - 1;
  for (unsigned i=dst_first, j=src_first; i>=dst_start; --i, --j) {
    dst[i] = src[j];
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_print_display (FILE * f, const char * name,
				     unsigned nslots, ccdoubles_real_operand_t O)
{
  fprintf(f, "Vector %s (dimension %u):\n", name, nslots);
  fprintf(f, "| (1) %+lf ", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, "; (%u) %+lf ", 1+i, O[i]);
  }
  fprintf(f, "|\n");
}
void
ccdoubles_real_vector_print_brackets (FILE * f,
				      unsigned nslots, ccdoubles_real_operand_t O)
{
  fprintf(f, "[%+lf", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, " %+lf", O[i]);
  }
  fprintf(f, "]\n");
}
void
ccdoubles_real_vector_print_braces (FILE * f,
				    unsigned nslots, ccdoubles_real_operand_t O)
{
  fprintf(f, "{%+lf", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf", O[i]);
  }
  fprintf(f, "}\n");
}

/* end of file */
