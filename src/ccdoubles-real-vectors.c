/*
  Part of: CCDoubles
  Contents: routines for vectors
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
ccdoubles_real_vector_clear (size_t nslots, double * restrict vector)
{
  memset(vector, 0, sizeof(double) * nslots);
}
void
ccdoubles_real_vector_set (size_t nslots, double * restrict vector, double value)
{
  for (size_t i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_real_vector_copy (size_t nslots,
			    double * restrict dst,
			    double * restrict src)
{
  if (1) {
    memcpy(dst, src, sizeof(double) * nslots);
  } else {
    for (size_t i=0; i<nslots; ++i) {
      dst[i] = src[i];
    }
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_add (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_real_vector_sub (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_real_vector_mul (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_real_vector_div (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_real_vector_neg (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = - operand[i];
  }
}
void
ccdoubles_real_vector_abs (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = fabs(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fmod (size_t nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = fmod(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_drem (size_t nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = drem(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_remainder (size_t nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = remainder(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Rounding.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_ceil (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = ceil(operand[i]);
  }
}
void
ccdoubles_real_vector_floor (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = floor(operand[i]);
  }
}
void
ccdoubles_real_vector_trunc (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = trunc(operand[i]);
  }
}
void
ccdoubles_real_vector_round (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = round(operand[i]);
  }
}
void
ccdoubles_real_vector_rint (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = rint(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Comparison.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_isgreater (size_t nslots,
				 int * restrict result,
				 double * restrict operand1,
				 double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isgreater(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isgreaterequal (size_t nslots,
				      int * restrict result,
				      double * restrict operand1,
				      double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isgreaterequal(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isless (size_t nslots,
			      int * restrict result,
			      double * restrict operand1,
			      double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isless(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_islessequal (size_t nslots,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = islessequal(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_islessgreater (size_t nslots,
				     int * restrict result,
				     double * restrict operand1,
				     double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = islessgreater(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isunordered (size_t nslots,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isunordered(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_min (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = fmin(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_max (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = fmax(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fpclassify (size_t nslots,
				  int * restrict result,
				  double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = fpclassify(operand[i]);
  }
}
void
ccdoubles_real_vector_isfinite (size_t nslots,
				int * restrict result,
				double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isfinite(operand[i]);
  }
}
void
ccdoubles_real_vector_isinfinite (size_t nslots,
				  int * restrict result,
				  double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isfinite(operand[i]) ? 0 : 1;
  }
}
void
ccdoubles_real_vector_isnormal (size_t nslots,
				int * restrict result,
				double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isnormal(operand[i]);
  }
}
void
ccdoubles_real_vector_isnan (size_t nslots,
			     int * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = isnan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double
ccdoubles_real_vector_scalar_product (size_t nslots,
				      const double * restrict operand1,
				      const double * restrict operand2)
{
  double	result = 0.0;
  for (size_t i=0; i<nslots; ++i) {
    result += operand1[i] * operand2[i];
  }
  return result;
}
void
ccdoubles_real_vector_scalar_mul (size_t nslots,
				  double * restrict result,
				  double lambda,
				  double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_real_vector_linear_combination (size_t nslots,
					  double * restrict result,
					  double alpha,
					  double * restrict operand1,
					  double beta,
					  double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_exp (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = exp(operand[i]);
  }
}
void
ccdoubles_real_vector_exp10 (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = exp(operand[i] * log(10.0));
  }
}
void
ccdoubles_real_vector_exp2 (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = exp2(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_log (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = log(operand[i]);
  }
}
void
ccdoubles_real_vector_log10 (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = log10(operand[i]);
  }
}
void
ccdoubles_real_vector_log2 (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = log2(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_logb (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = logb(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_pow (size_t nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = pow(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_sqrt (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = sqrt(operand[i]);
  }
}
void
ccdoubles_real_vector_cbrt (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cbrt(operand[i]);
  }
}
void
ccdoubles_real_vector_hypot (size_t nslots,
			     double * restrict result,
			     double * restrict operand1,
			     double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = hypot(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_expm1 (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = expm1(operand[i]);
  }
}
void
ccdoubles_real_vector_log1p (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = log1p(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sin (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = sin(operand[i]);
  }
}
void
ccdoubles_real_vector_cos (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cos(operand[i]);
  }
}
void
ccdoubles_real_vector_tan (size_t nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = tan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asin (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = asin(operand[i]);
  }
}
void
ccdoubles_real_vector_acos (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = acos(operand[i]);
  }
}
void
ccdoubles_real_vector_atan (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = atan(operand[i]);
  }
}
void
ccdoubles_real_vector_atan2 (size_t nslots,
			     double * restrict result,
			     double * restrict operand1,
			     double * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = atan2(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sinh (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = sinh(operand[i]);
  }
}
void
ccdoubles_real_vector_cosh (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cosh(operand[i]);
  }
}
void
ccdoubles_real_vector_tanh (size_t nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = tanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asinh (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = asinh(operand[i]);
  }
}
void
ccdoubles_real_vector_acosh (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = acosh(operand[i]);
  }
}
void
ccdoubles_real_vector_atanh (size_t nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = atanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_print_display (FILE * f, const char * name,
				     size_t nslots,
				     double * operand)
{
  fprintf(f, "Vector %s (dimension %ld):\n", name, nslots);
  fprintf(f, "| (1) %+lf ", operand[0]);
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, "; (%ld) %+lf ", 1+i, operand[i]);
  }
  fprintf(f, "|\n");
}
void
ccdoubles_real_vector_print_brackets (FILE * f, size_t nslots, double * operand)
{
  fprintf(f, "[%+lf", operand[0]);
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, " %+lf", operand[i]);
  }
  fprintf(f, "]\n");
}
void
ccdoubles_real_vector_print_braces (FILE * f, size_t nslots, double * operand)
{
  fprintf(f, "{%+lf", operand[0]);
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf", operand[i]);
  }
  fprintf(f, "}\n");
}

/* end of file */
