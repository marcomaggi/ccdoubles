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
ccdoubles_cplx_vector_clear (unsigned nslots, double complex * restrict vector)
{
  memset(vector, 0, sizeof(double complex) * nslots);
}
void
ccdoubles_cplx_vector_set (unsigned nslots, double complex * restrict vector, double complex value)
{
  for (unsigned i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_cplx_vector_set_split (unsigned nslots, double complex * restrict vector,
				 double value_re, double value_im)
{
  for (unsigned i=0; i<nslots; ++i) {
    vector[i] = Z(value_re, value_im);
  }
}
void
ccdoubles_cplx_vector_copy (unsigned nslots,
			    double complex * restrict dst,
			    double complex * restrict src)
{
  if (1) {
    memcpy(dst, src, sizeof(double complex) * nslots);
  } else {
    for (unsigned i=0; i<nslots; ++i) {
      dst[i] = src[i];
    }
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_vector_real (unsigned nslots,
			    double * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = Re(operand[i]);
  }
}
void
ccdoubles_cplx_vector_imag (unsigned nslots,
			    double * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = Im(operand[i]);
  }
}
void
ccdoubles_cplx_vector_magnitude (unsigned nslots,
				 double * restrict result,
				 double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    if (1) {
      result[i] = cabs(operand[i]);
    } else {
      result[i] = hypot(Re(operand[i]), Im(operand[i]));
    }
  }
}
void
ccdoubles_cplx_vector_angle (unsigned nslots,
			     double * restrict result,
			     double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    if (1) {
      result[i] = carg(operand[i]);
    } else {
      result[i] = atan2(Im(operand[i]), Re(operand[i]));
    }
  }
}
void
ccdoubles_cplx_vector_conj (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = conj(operand[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_vector_from_rect (unsigned nslots,
				 double complex * restrict result,
				 double * restrict real,
				 double * restrict imag)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = Z(real[i], imag[i]);
  }
}
void
ccdoubles_cplx_vector_from_polar (unsigned nslots,
				  double complex * restrict result,
				  double * restrict magnitude,
				  double * restrict angle)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = Z((magnitude[i] * cos(angle[i])),
		  (magnitude[i] * sin(angle[i])));
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_add (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_cplx_vector_sub (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_cplx_vector_mul (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_cplx_vector_div (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_cplx_vector_neg (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = - operand[i];
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double complex
ccdoubles_cplx_vector_scalar_product (unsigned nslots,
				      const double complex * restrict operand1,
				      const double complex * restrict operand2)
{
  double complex	result = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    result += operand1[i] * operand2[i];
  }
  return result;
}
void
ccdoubles_cplx_vector_scalar_product_split (unsigned nslots,
					    double complex * result,
					    const double complex * restrict operand1,
					    const double complex * restrict operand2)
{
  double complex	R = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    R += operand1[i] * operand2[i];
  }
  *result = R;
}
void
ccdoubles_cplx_vector_scalar_mul (unsigned nslots,
				  double complex * restrict result,
				  double complex lambda,
				  double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_cplx_vector_scalar_mul_split (unsigned nslots,
					double complex * restrict result,
					double lambda_re, double lambda_im,
					double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = Z(lambda_re, lambda_im) * operand[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination (unsigned nslots,
					  double complex * restrict result,
					  double complex alpha,
					  double complex * restrict operand1,
					  double complex beta,
					  double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination_split (unsigned nslots,
						double complex * restrict result,
						double alpha_re, double alpha_im,
						double complex * restrict operand1,
						double beta_re, double beta_im,
						double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = \
      Z(alpha_re, alpha_im) * operand1[i] + \
      Z(beta_re,  beta_im)  * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_exp (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cexp(operand[i]);
  }
}
void
ccdoubles_cplx_vector_log (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = clog(operand[i]);
  }
}
void
ccdoubles_cplx_vector_log10 (unsigned nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
#ifdef HAVE_CLOG10
    result[i] = clog10(operand[i]);
#else
    result[i] = Z(log10(cabs(operand[i])), carg(operand[i]));
#endif
  }
}
void
ccdoubles_cplx_vector_sqrt (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = csqrt(operand[i]);
  }
}
void
ccdoubles_cplx_vector_pow (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand1,
			   double complex * restrict operand2)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cpow(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sin (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = csin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cos (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = ccos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tan (unsigned nslots,
			   double complex * restrict result,
			   double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = ctan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asin (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = casin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acos (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cacos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atan (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = catan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sinh (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = csinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cosh (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = ccosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tanh (unsigned nslots,
			    double complex * restrict result,
			    double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = ctanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asinh (unsigned nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = casinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acosh (unsigned nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = cacosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atanh (unsigned nslots,
			     double complex * restrict result,
			     double complex * restrict operand)
{
  for (unsigned i=0; i<nslots; ++i) {
    result[i] = catanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_print_display (FILE * f, const char * name,
				     unsigned nslots,
				     double complex * operand)
{
  fprintf(f, "Vector %s (dimension %u):\n", name, nslots);
  fprintf(f, "| (1) %+lf%-+lfi ", Re(operand[0]), Im(operand[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, "; (%u) %+lf%-+lfi ", 1+i, Re(operand[i]), Im(operand[i]));
  }
  fprintf(f, "|\n");
}
void
ccdoubles_cplx_vector_print_brackets (FILE * f, unsigned nslots, double complex * operand)
{
  fprintf(f, "[%+lf%-+lfi", Re(operand[0]), Im(operand[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, " %+lf%-+lfi", Re(operand[i]), Im(operand[i]));
  }
  fprintf(f, "]\n");
}
void
ccdoubles_cplx_vector_print_braces (FILE * f, unsigned nslots, double complex * operand)
{
  fprintf(f, "{%+lf%-+lfi", Re(operand[0]), Im(operand[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf%-+lfi", Re(operand[i]), Im(operand[i]));
  }
  fprintf(f, "}\n");
}

/* end of file */
