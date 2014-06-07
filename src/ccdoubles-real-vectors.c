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
ccdoubles_real_vector_clear (size_t nitems, double * __restrict__ vector)
{
  memset(vector, 0, sizeof(double) * nitems);
}
void
ccdoubles_real_vector_set (size_t nitems, double * __restrict__ vector, double value)
{
  for (size_t i=0; i<nitems; ++i) {
    vector[i] = value;
  }
}


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_add (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand1,
			   double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] + operand2[i];
  }
}
void
ccdoubles_real_vector_sub (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand1,
			   double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] - operand2[i];
  }
}
void
ccdoubles_real_vector_mul (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand1,
			   double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] * operand2[i];
  }
}
void
ccdoubles_real_vector_div (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand1,
			   double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = operand1[i] / operand2[i];
  }
}
void
ccdoubles_real_vector_neg (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = - operand[i];
  }
}


/** --------------------------------------------------------------------
 ** Vector operations.
 ** ----------------------------------------------------------------- */

double
ccdoubles_real_vector_scalar_product (size_t nitems,
				      const double * __restrict__ operand1,
				      const double * __restrict__ operand2)
{
  double	result = 0.0;
  for (size_t i=0; i<nitems; ++i) {
    result += operand1[i] + operand2[i];
  }
  return result;
}
void
ccdoubles_real_vector_scalar_mul (size_t nitems,
				  double * __restrict__ result,
				  double lambda,
				  const double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = lambda * operand[i];
  }
}
void
ccdoubles_real_vector_linear_combination (size_t nitems,
					  double * __restrict__ result,
					  double alpha,
					  double * __restrict__ operand1,
					  double beta,
					  double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = alpha * operand1[i] + beta * operand2[i];
  }
}


/** --------------------------------------------------------------------
 ** Trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sin (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = sin(operand[i]);
  }
}
void
ccdoubles_real_vector_cos (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = cos(operand[i]);
  }
}
void
ccdoubles_real_vector_tan (size_t nitems,
			   double * __restrict__ result,
			   double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = tan(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse trigonometric operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asin (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = asin(operand[i]);
  }
}
void
ccdoubles_real_vector_acos (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = acos(operand[i]);
  }
}
void
ccdoubles_real_vector_atan (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = atan(operand[i]);
  }
}
void
ccdoubles_real_vector_atan2 (size_t nitems,
			     double * __restrict__ result,
			     double * __restrict__ operand1,
			     double * __restrict__ operand2)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = atan2(operand1[i], operand2[i]);
  }
}


/** --------------------------------------------------------------------
 ** Hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_sinh (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = sinh(operand[i]);
  }
}
void
ccdoubles_real_vector_cosh (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = cosh(operand[i]);
  }
}
void
ccdoubles_real_vector_tanh (size_t nitems,
			    double * __restrict__ result,
			    double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = tanh(operand[i]);
  }
}


/** --------------------------------------------------------------------
 ** Inverse hyperbolic operations.
 ** ----------------------------------------------------------------- */

void
ccdoubles_real_vector_asinh (size_t nitems,
			     double * __restrict__ result,
			     double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = asinh(operand[i]);
  }
}
void
ccdoubles_real_vector_acosh (size_t nitems,
			     double * __restrict__ result,
			     double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = acosh(operand[i]);
  }
}
void
ccdoubles_real_vector_atanh (size_t nitems,
			     double * __restrict__ result,
			     double * __restrict__ operand)
{
  for (size_t i=0; i<nitems; ++i) {
    result[i] = atanh(operand[i]);
  }
}

/* end of file */
