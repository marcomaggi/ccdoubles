/*
  Part of: CCDoubles
  Contents: routines for vectors
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014, 2015, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

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
#include <complex.h>
#include <string.h>


/** --------------------------------------------------------------------
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_clear (unsigned nslots, ccdoubles_cplx_result_t V)
{
  memset(V, 0, sizeof(double complex) * nslots);
}
void
ccdoubles_cplx_vector_set (unsigned nslots, ccdoubles_cplx_result_t V, double complex value)
{
  for (unsigned i=0; i<nslots; ++i) {
    V[i] = value;
  }
}
void
ccdoubles_cplx_vector_set_split (unsigned nslots, ccdoubles_cplx_result_t V,
				 double value_re, double value_im)
{
  for (unsigned i=0; i<nslots; ++i) {
    V[i] = Z(value_re, value_im);
  }
}
void
ccdoubles_cplx_vector_copy (unsigned nslots,
			    ccdoubles_cplx_result_t dst,
			    ccdoubles_cplx_operand_t src)
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
			    ccdoubles_real_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = Re(O[i]);
  }
}
void
ccdoubles_cplx_vector_imag (unsigned nslots,
			    ccdoubles_real_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = Im(O[i]);
  }
}
void
ccdoubles_cplx_vector_magnitude (unsigned nslots,
				 ccdoubles_real_result_t R,
				 ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    if (1) {
      R[i] = cabs(O[i]);
    } else {
      R[i] = hypot(Re(O[i]), Im(O[i]));
    }
  }
}
void
ccdoubles_cplx_vector_angle (unsigned nslots,
			     ccdoubles_real_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    if (1) {
      R[i] = carg(O[i]);
    } else {
      R[i] = atan2(Im(O[i]), Re(O[i]));
    }
  }
}
void
ccdoubles_cplx_vector_conj (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = conj(O[i]);
  }
}

/* ------------------------------------------------------------------ */

void
ccdoubles_cplx_vector_from_rect (unsigned nslots,
				 ccdoubles_cplx_result_t R,
				 ccdoubles_real_operand_t real,
				 ccdoubles_real_operand_t imag)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = Z(real[i], imag[i]);
  }
}
void
ccdoubles_cplx_vector_from_polar (unsigned nslots,
				  ccdoubles_cplx_result_t R,
				  ccdoubles_real_operand_t magnitude,
				  ccdoubles_real_operand_t angle)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = Z((magnitude[i] * cos(angle[i])),
	     (magnitude[i] * sin(angle[i])));
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_add (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] + O2[i];
  }
}
void
ccdoubles_cplx_vector_sub (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] - O2[i];
  }
}
void
ccdoubles_cplx_vector_mul (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] * O2[i];
  }
}
void
ccdoubles_cplx_vector_div (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = O1[i] / O2[i];
  }
}
void
ccdoubles_cplx_vector_neg (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = - O[i];
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double complex
ccdoubles_cplx_vector_scalar_product (unsigned nslots,
				      ccdoubles_cplx_operand_t O1,
				      ccdoubles_cplx_operand_t O2)
{
  double complex	result = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    result += O1[i] * O2[i];
  }
  return result;
}
void
ccdoubles_cplx_vector_scalar_product_split (unsigned nslots,
					    ccdoubles_cplx_result_t R,
					    ccdoubles_cplx_operand_t O1,
					    ccdoubles_cplx_operand_t O2)
{
  double complex	result = 0.0;
  for (unsigned i=0; i<nslots; ++i) {
    result += O1[i] * O2[i];
  }
  *R = result;
}
void
ccdoubles_cplx_vector_scalar_mul (unsigned nslots,
				  ccdoubles_cplx_result_t R,
				  double complex lambda,
				  ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = lambda * O[i];
  }
}
void
ccdoubles_cplx_vector_scalar_mul_split (unsigned nslots,
					ccdoubles_cplx_result_t R,
					double lambda_re, double lambda_im,
					ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = Z(lambda_re, lambda_im) * O[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination (unsigned nslots,
					  ccdoubles_cplx_result_t R,
					  double complex alpha,
					  ccdoubles_cplx_operand_t O1,
					  double complex beta,
					  ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = alpha * O1[i] + beta * O2[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination_split (unsigned nslots,
						ccdoubles_cplx_result_t R,
						double alpha_re, double alpha_im,
						ccdoubles_cplx_operand_t O1,
						double beta_re, double beta_im,
						ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = \
      Z(alpha_re, alpha_im) * O1[i] + \
      Z(beta_re,  beta_im)  * O2[i];
  }
}


/** --------------------------------------------------------------------
 ** Exponentiation and logarithms.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_exp (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cexp(O[i]);
  }
}
void
ccdoubles_cplx_vector_log (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = clog(O[i]);
  }
}
void
ccdoubles_cplx_vector_log10 (unsigned nslots,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
#ifdef HAVE_CLOG10
    R[i] = clog10(O[i]);
#else
    R[i] = Z(log10(cabs(O[i])), carg(O[i]));
#endif
  }
}
void
ccdoubles_cplx_vector_sqrt (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = csqrt(O[i]);
  }
}
void
ccdoubles_cplx_vector_pow (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O1,
			   ccdoubles_cplx_operand_t O2)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cpow(O1[i], O2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sin (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = csin(O[i]);
  }
}
void
ccdoubles_cplx_vector_cos (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ccos(O[i]);
  }
}
void
ccdoubles_cplx_vector_tan (unsigned nslots,
			   ccdoubles_cplx_result_t R,
			   ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ctan(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asin (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = casin(O[i]);
  }
}
void
ccdoubles_cplx_vector_acos (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cacos(O[i]);
  }
}
void
ccdoubles_cplx_vector_atan (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = catan(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sinh (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = csinh(O[i]);
  }
}
void
ccdoubles_cplx_vector_cosh (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ccosh(O[i]);
  }
}
void
ccdoubles_cplx_vector_tanh (unsigned nslots,
			    ccdoubles_cplx_result_t R,
			    ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = ctanh(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asinh (unsigned nslots,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = casinh(O[i]);
  }
}
void
ccdoubles_cplx_vector_acosh (unsigned nslots,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = cacosh(O[i]);
  }
}
void
ccdoubles_cplx_vector_atanh (unsigned nslots,
			     ccdoubles_cplx_result_t R,
			     ccdoubles_cplx_operand_t O)
{
  for (unsigned i=0; i<nslots; ++i) {
    R[i] = catanh(O[i]);
  }
}


/** --------------------------------------------------------------------
 ** Subvector operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_copy_forward (unsigned dst_start, unsigned src_start, unsigned nslots,
				    ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
{
  unsigned	i_end = dst_start + nslots;
  for (unsigned i=dst_start, j=src_start; i<i_end; ++i, ++j) {
    dst[i] = src[j];
  }
}
void
ccdoubles_cplx_vector_copy_backward (unsigned dst_start, unsigned src_start, unsigned nslots,
				     ccdoubles_cplx_result_t dst, ccdoubles_cplx_operand_t src)
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
ccdoubles_cplx_vector_print_display (FILE * f, const char * name,
				     unsigned nslots,
				     ccdoubles_cplx_operand_t O)
{
  fprintf(f, "Vector %s (dimension %u):\n", name, nslots);
  fprintf(f, "| (1) %+lf%-+lfi ", Re(O[0]), Im(O[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, "; (%u) %+lf%-+lfi ", 1+i, Re(O[i]), Im(O[i]));
  }
  fprintf(f, "|\n");
}
void
ccdoubles_cplx_vector_print_brackets (FILE * f, unsigned nslots, ccdoubles_cplx_operand_t O)
{
  fprintf(f, "[%+lf%-+lfi", Re(O[0]), Im(O[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, " %+lf%-+lfi", Re(O[i]), Im(O[i]));
  }
  fprintf(f, "]\n");
}
void
ccdoubles_cplx_vector_print_braces (FILE * f, unsigned nslots, ccdoubles_cplx_operand_t O)
{
  fprintf(f, "{%+lf%-+lfi", Re(O[0]), Im(O[0]));
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, ", %+lf%-+lfi", Re(O[i]), Im(O[i]));
  }
  fprintf(f, "}\n");
}

/* end of file */
