/*
  Part of: CCDoubles
  Contents: tests for vector functions
  Date: Sun Jun  8, 2014

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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ccdoubles.h>


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc, const char *const argv[])
{
#define	NITEMS		1
  {
    double	V[NITEMS];
    ccdoubles_real_vector_clear(NITEMS, V);
    assert(0.0 == V[0]);
  }

  {
    double	V[NITEMS];
    ccdoubles_real_vector_set(NITEMS, V, 1.2);
    assert(1.2 == V[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NITEMS];
    double	O1[NITEMS] = { 1.2 };
    double	O2[NITEMS] = { 3.4 };
    ccdoubles_real_vector_add (NITEMS, R, O1, O2);
    assert((1.2 + 3.4) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O1[NITEMS] = { 1.2 };
    double	O2[NITEMS] = { 3.4 };
    ccdoubles_real_vector_sub (NITEMS, R, O1, O2);
    assert((1.2 - 3.4) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O1[NITEMS] = { 1.2 };
    double	O2[NITEMS] = { 3.4 };
    ccdoubles_real_vector_mul (NITEMS, R, O1, O2);
    assert((1.2 * 3.4) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O1[NITEMS] = { 1.2 };
    double	O2[NITEMS] = { 3.4 };
    ccdoubles_real_vector_div (NITEMS, R, O1, O2);
    assert((1.2 / 3.4) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_neg (NITEMS, R, O);
    assert(-1.2 == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R;
    double	O1[2] = { 1.2, 3.4 };
    double	O2[2] = { 5.6, 7.8 };
    R = ccdoubles_real_vector_scalar_product (2, O1, O2);
    assert((1.2 * 5.6 + 3.4 * 7.8) == R);
  }

  {
    double	R[NITEMS];
    double	lambda = 1.2;
    double	O[NITEMS] = { 3.4 };
    ccdoubles_real_vector_scalar_mul (NITEMS, R, lambda, O);
    assert((1.2 * 3.4) == R[0]);
  }

  {
    double	R[2];
    double	alpha = 1.2;
    double	beta  = 2.3;
    double	O1[2] = { 3.4, 4.5 };
    double	O2[2] = { 5.6, 6.7 };
    ccdoubles_real_vector_linear_combination (2, R, alpha, O1, beta, O2);
    assert((1.2 * 3.4 + 2.3 * 5.6) == R[0]);
    assert((1.2 * 4.5 + 2.3 * 6.7) == R[1]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_sin (NITEMS, R, O);
    assert(sin(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_cos (NITEMS, R, O);
    assert(cos(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_tan (NITEMS, R, O);
    assert(tan(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 0.5 };
    ccdoubles_real_vector_asin (NITEMS, R, O);
    assert(asin(0.5) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 0.5 };
    ccdoubles_real_vector_acos (NITEMS, R, O);
    assert(acos(0.5) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 0.5 };
    ccdoubles_real_vector_atan (NITEMS, R, O);
    assert(atan(0.5) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O1[NITEMS] = { 0.5 };
    double	O2[NITEMS] = { 0.6 };
    ccdoubles_real_vector_atan2 (NITEMS, R, O1, O2);
    assert(atan2(0.5, 0.6) == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_sinh (NITEMS, R, O);
    assert(sinh(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_cosh (NITEMS, R, O);
    assert(cosh(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_tanh (NITEMS, R, O);
    assert(tanh(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 0.5 };
    ccdoubles_real_vector_asinh (NITEMS, R, O);
    assert(asinh(0.5) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 1.2 };
    ccdoubles_real_vector_acosh (NITEMS, R, O);
    assert(acosh(1.2) == R[0]);
  }

  {
    double	R[NITEMS];
    double	O[NITEMS] = { 0.1 };
    ccdoubles_real_vector_atanh (NITEMS, R, O);
    assert(atanh(0.1) == R[0]);
  }

  exit(EXIT_SUCCESS);
}

/* end of file */
