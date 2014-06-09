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
#include <complex.h>
#include <string.h>


/** --------------------------------------------------------------------
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_clear (size_t nslots, double complex * restrict vector)
{
  memset(vector, 0, sizeof(double complex) * nslots);
}
void
ccdoubles_cplx_vector_set (size_t nslots, double complex * restrict vector, double complex value)
{
  for (size_t i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_cplx_vector_copy (size_t nslots,
			    double complex * restrict dst,
			    double complex * restrict src)
{
  if (1) {
    memcpy(dst, src, sizeof(double complex) * nslots);
  } else {
    for (size_t i=0; i<nslots; ++i) {
      dst[i] = src[i];
    }
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_vector_real (size_t nslots,
			    double * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = creal(operand[i]);
  }
}
void
ccdoubles_cplx_vector_imag (size_t nslots,
			    double * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cimag(operand[i]);
  }
}
void
ccdoubles_cplx_vector_magnitude (size_t nslots,
				 double * restrict result,
				 double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    if (1) {
      result[i] = cabs(operand[i]);
    } else {
      result[i] = hypot(creal(operand[i]), cimag(operand[i]));
    }
  }
}
void
ccdoubles_cplx_vector_angle (size_t nslots,
			     double * restrict result,
			     double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    if (1) {
      result[i] = carg(operand[i]);
    } else {
      result[i] = atan2(cimag(operand[i]), creal(operand[i]));
    }
  }
}
void
ccdoubles_cplx_vector_conj (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = conj(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_vector_from_rect (size_t nslots,
				 double complex * restrict result,
				 double * restrict real,
				 double * restrict imag)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = CCDOUBLES_CPLX(real[i], imag[i]);
  }
}
void
ccdoubles_cplx_vector_from_polar (size_t nslots,
				  double complex * restrict result,
				  double * restrict magnitude,
				  double * restrict angle)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = CCDOUBLES_CPLX((magnitude[i] * cos(angle[i])),
			       (magnitude[i] * sin(angle[i])));
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_add (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_cplx_vector_sub (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_cplx_vector_mul (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_cplx_vector_div (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_cplx_vector_neg (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = - operand[i];
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double complex
ccdoubles_cplx_vector_scalar_product (size_t nslots,
				      const double complex * restrict operand1,
				      const double complex * restrict operand2)
{
  double complex	result = 0.0;
  for (size_t i=0; i<nslots; ++i) {
    result += operand1[i] * operand2[i];
  }
  return result;
}
void
ccdoubles_cplx_vector_scalar_mul (size_t nslots,
				  double complex * restrict result,
				  double complex lambda,
				  double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination (size_t nslots,
					  double complex * restrict result,
					  double complex alpha,
					  double complex * restrict operand1,
					  double complex beta,
					  double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_exp (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cexp(operand[i]);
  }
}
void
ccdoubles_cplx_vector_log (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = clog(operand[i]);
  }
}
void
ccdoubles_cplx_vector_log10 (size_t nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
#ifdef HAVE_CLOG10
    result[i] = clog10(operand[i]);
#else
    result[i] = CCDOUBLES_CPLX(log10(cabs(operand[i])), carg(operand[i]));
#endif
  }
}
void
ccdoubles_cplx_vector_sqrt (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = csqrt(operand[i]);
  }
}
void
ccdoubles_cplx_vector_pow (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cpow(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sin (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = csin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cos (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = ccos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tan (size_t nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = ctan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asin (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = casin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acos (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cacos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atan (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = catan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sinh (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = csinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cosh (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = ccosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tanh (size_t nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = ctanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asinh (size_t nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = casinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acosh (size_t nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = cacosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atanh (size_t nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (size_t i=0; i<nslots; ++i) {
    result[i] = catanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_print_display (FILE * f, const char * name,
				     size_t nslots,
				     double complex * operand)
{
  fprintf(f, "Vector %s (dimension %ld):\n", name, nslots);
  fprintf(f, "| (1) %+lf%-+lfi ", creal(operand[0]), cimag(operand[0]));
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, "; (%ld) %+lf%-+lfi ", 1+i, creal(operand[i]), cimag(operand[i]));
  }
  fprintf(f, "|\n");
}
void
ccdoubles_cplx_vector_print_brackets (FILE * f, size_t nslots, double complex * operand)
{
  fprintf(f, "[%+lf%-+lfi", creal(operand[0]), cimag(operand[0]));
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, " %+lf%-+lfi", creal(operand[i]), cimag(operand[i]));
  }
  fprintf(f, "]\n");
}
void
ccdoubles_cplx_vector_print_braces (FILE * f, size_t nslots, double complex * operand)
{
  fprintf(f, "{%+lf%-+lfi", creal(operand[0]), cimag(operand[0]));
  for (size_t i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf%-+lfi", creal(operand[i]), cimag(operand[i]));
  }
  fprintf(f, "}\n");
}

/* end of file */
