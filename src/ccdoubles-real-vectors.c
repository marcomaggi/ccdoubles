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
ccdoubles_real_vector_clear (unsigned nslots, double * restrict vector)
{
  memset(vector, 0, sizeof(double) * nslots);
}
void
ccdoubles_real_vector_set (unsigned nslots, double * restrict vector, double value)
{
  for (unsigned i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_real_vector_copy (unsigned nslots,
			    double * restrict dst,
			    double * restrict src)
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
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_real_vector_sub (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_real_vector_mul (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_real_vector_div (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_real_vector_neg (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = - operand[i];
  }
}
void
ccdoubles_real_vector_abs (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = fabs(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fmod (unsigned nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = fmod(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_drem (unsigned nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = drem(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_remainder (unsigned nslots,
			    double * restrict result,
			    double * restrict operand1,
			    double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = remainder(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Rounding.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_ceil (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = ceil(operand[i]);
  }
}
void
ccdoubles_real_vector_floor (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = floor(operand[i]);
  }
}
void
ccdoubles_real_vector_trunc (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = trunc(operand[i]);
  }
}
void
ccdoubles_real_vector_round (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = round(operand[i]);
  }
}
void
ccdoubles_real_vector_rint (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = rint(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Comparison.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_isgreater (unsigned nslots,
				 int * restrict result,
				 double * restrict operand1,
				 double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isgreater(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isgreaterequal (unsigned nslots,
				      int * restrict result,
				      double * restrict operand1,
				      double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isgreaterequal(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isless (unsigned nslots,
			      int * restrict result,
			      double * restrict operand1,
			      double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isless(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_islessequal (unsigned nslots,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = islessequal(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_islessgreater (unsigned nslots,
				     int * restrict result,
				     double * restrict operand1,
				     double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = islessgreater(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_isunordered (unsigned nslots,
				   int * restrict result,
				   double * restrict operand1,
				   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isunordered(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_min (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = fmin(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_max (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = fmax(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_fpclassify (unsigned nslots,
				  int * restrict result,
				  double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = fpclassify(operand[i]);
  }
}
void
ccdoubles_real_vector_isfinite (unsigned nslots,
				int * restrict result,
				double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isfinite(operand[i]);
  }
}
void
ccdoubles_real_vector_isinfinite (unsigned nslots,
				  int * restrict result,
				  double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isfinite(operand[i]) ? 0 : 1;
  }
}
void
ccdoubles_real_vector_isnormal (unsigned nslots,
				int * restrict result,
				double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isnormal(operand[i]);
  }
}
void
ccdoubles_real_vector_isnan (unsigned nslots,
			     int * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = isnan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double
ccdoubles_real_vector_scalar_product (unsigned nslots,
				      const double * restrict operand1,
				      const double * restrict operand2)
{
  double	result = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    result += operand1[i] * operand2[i];
  }
  return result;
}
void
ccdoubles_real_vector_scalar_mul (unsigned nslots,
				  double * restrict result,
				  double lambda,
				  double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_real_vector_linear_combination (unsigned nslots,
					  double * restrict result,
					  double alpha,
					  double * restrict operand1,
					  double beta,
					  double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_exp (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = exp(operand[i]);
  }
}
void
ccdoubles_real_vector_exp10 (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = exp(operand[i] * log(10.0));
  }
}
void
ccdoubles_real_vector_exp2 (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = exp2(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_log (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = log(operand[i]);
  }
}
void
ccdoubles_real_vector_log10 (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = log10(operand[i]);
  }
}
void
ccdoubles_real_vector_log2 (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = log2(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_logb (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = logb(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_pow (unsigned nslots,
			   double * restrict result,
			   double * restrict operand1,
			   double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = pow(operand1[i], operand2[i]);
  }
}
void
ccdoubles_real_vector_sqrt (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = sqrt(operand[i]);
  }
}
void
ccdoubles_real_vector_cbrt (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cbrt(operand[i]);
  }
}
void
ccdoubles_real_vector_hypot (unsigned nslots,
			     double * restrict result,
			     double * restrict operand1,
			     double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = hypot(operand1[i], operand2[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_real_vector_expm1 (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = expm1(operand[i]);
  }
}
void
ccdoubles_real_vector_log1p (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = log1p(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sin (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = sin(operand[i]);
  }
}
void
ccdoubles_real_vector_cos (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cos(operand[i]);
  }
}
void
ccdoubles_real_vector_tan (unsigned nslots,
			   double * restrict result,
			   double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = tan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asin (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = asin(operand[i]);
  }
}
void
ccdoubles_real_vector_acos (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = acos(operand[i]);
  }
}
void
ccdoubles_real_vector_atan (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = atan(operand[i]);
  }
}
void
ccdoubles_real_vector_atan2 (unsigned nslots,
			     double * restrict result,
			     double * restrict operand1,
			     double * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = atan2(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sinh (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = sinh(operand[i]);
  }
}
void
ccdoubles_real_vector_cosh (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cosh(operand[i]);
  }
}
void
ccdoubles_real_vector_tanh (unsigned nslots,
			    double * restrict result,
			    double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = tanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asinh (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = asinh(operand[i]);
  }
}
void
ccdoubles_real_vector_acosh (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = acosh(operand[i]);
  }
}
void
ccdoubles_real_vector_atanh (unsigned nslots,
			     double * restrict result,
			     double * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = atanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_print_display (FILE * f, const char * name,
				     unsigned nslots,
				     double * operand)
{
  fprintf(f, "Vector %s (dimension %u):\n", name, nslots);
  fprintf(f, "| (1) %+lf ", operand[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, "; (%u) %+lf ", 1+i, operand[i]);
  }
  fprintf(f, "|\n");
}
void
ccdoubles_real_vector_print_brackets (FILE * f, unsigned nslots, double * operand)
{
  fprintf(f, "[%+lf", operand[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, " %+lf", operand[i]);
  }
  fprintf(f, "]\n");
}
void
ccdoubles_real_vector_print_braces (FILE * f, unsigned nslots, double * operand)
{
  fprintf(f, "{%+lf", operand[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf", operand[i]);
  }
  fprintf(f, "}\n");
}

/* end of file */
