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
ccdoubles_cplx_vector_clear (size_t nitems, _Complex * __restrict__ vector)
{
  memset(vector, 0, sizeof(_Complex double) * nitems);
}
void
ccdoubles_cplx_vector_set (size_t nitems, _Complex * __restrict__ vector, _Complex value)
{
  for (size_t i=0; i<nitems; ++i) {
    vector[i] = value;
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_add (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand1,
			   _Complex * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_cplx_vector_sub (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand1,
			   _Complex * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_cplx_vector_mul (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand1,
			   _Complex * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_cplx_vector_div (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand1,
			   _Complex * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_cplx_vector_neg (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = - operand[i];
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

_Complex double
ccdoubles_cplx_vector_scalar_product (size_t nitems,
				      const _Complex * __restrict__ operand1,
				      const _Complex * __restrict__ operand2)
{
  _Complex double	result = 0.0;
  for (size_t i=0; i<nitems; ++i) {
    result += operand1[i] + operand2[i];
  }
  return result;
}
void
ccdoubles_cplx_vector_scalar_mul (size_t nitems,
				  _Complex * __restrict__ result,
				  _Complex lambda,
				  const _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_cplx_vector_linear_combination (size_t nitems,
					  _Complex * __restrict__ result,
					  _Complex alpha,
					  _Complex * __restrict__ operand1,
					  _Complex beta,
					  _Complex * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sin (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = csin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cos (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = ccos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tan (size_t nitems,
			   _Complex * __restrict__ result,
			   _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = ctan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asin (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = casin(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acos (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = cacos(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atan (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = catan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_sinh (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = csinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_cosh (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = ccosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_tanh (size_t nitems,
			    _Complex * __restrict__ result,
			    _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = ctanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_cplx_vector_asinh (size_t nitems,
			     _Complex * __restrict__ result,
			     _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = casinh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_acosh (size_t nitems,
			     _Complex * __restrict__ result,
			     _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = cacosh(operand[i]);
  }
}
void
ccdoubles_cplx_vector_atanh (size_t nitems,
			     _Complex * __restrict__ result,
			     _Complex * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = catanh(operand[i]);
  }
}

/* end of file */
