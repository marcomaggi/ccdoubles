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

static void test_real_vectors (void);
static void test_cplx_vectors (void);

#define	NITEMS		1
#define EPSILON		1e-6

#define CPLX(REAL,IMAG)		((REAL) + (IMAG) * _Complex_I)


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc, const char *const argv[])
{
  test_real_vectors();
  test_cplx_vectors();
  exit(EXIT_SUCCESS);
}


/** --------------------------------------------------------------------
 ** Test real vectors.
 ** ----------------------------------------------------------------- */

void
test_real_vectors (void)
{
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
}


/** --------------------------------------------------------------------
 ** Test complex vectors.
 ** ----------------------------------------------------------------- */

void
test_cplx_vectors (void)
{
  {
    double complex	V[NITEMS];
    double complex	E = CPLX(0.0, 0.0);
    ccdoubles_cplx_vector_clear(NITEMS, V);
    assert(E == V[0]);
  }

  {
    double complex	V[NITEMS];
    double complex	E = CPLX(1.2, 3.4);
    ccdoubles_cplx_vector_set(NITEMS, V, E);
    assert(E == V[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NITEMS];
    double complex	O1[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	O2[NITEMS] = { CPLX(4.5, 6.7) };
    ccdoubles_cplx_vector_add (NITEMS, R, O1, O2);
    assert((CPLX(1.2, 3.4) + CPLX(4.5, 6.7)) == R[0]);
  }

  {
    double complex	R[NITEMS];
    double complex	O1[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	O2[NITEMS] = { CPLX(4.5, 6.7) };
    ccdoubles_cplx_vector_sub (NITEMS, R, O1, O2);
    assert((CPLX(1.2, 3.4) - CPLX(4.5, 6.7)) == R[0]);
  }

  {
    double complex	R[NITEMS];
    double complex	O1[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	O2[NITEMS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_mul(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    ccdoubles_cplx_vector_mul (NITEMS, R, O1, O2);
    assert(E == R[0]);
    assert((O1[0] * O2[0]) == R[0]);
    assert((O1[0] * O2[0]) == E);
    /* printf("multiplication E = %lf%+lfi\n", creal(E), cimag(E)); */
  }

  {
    double complex	R[NITEMS];
    double complex	O1[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	O2[NITEMS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_div(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    double complex	X = CPLX(1.2, 3.4) / CPLX(5.6, 7.8);
    ccdoubles_cplx_vector_div (NITEMS, R, O1, O2);
    assert(cabs(E - R[0]) < EPSILON);
    assert(cabs(X - R[0]) < EPSILON);
    assert(cabs(X - E)    < EPSILON);
    /* printf("division E = %lf%+lfi\n", creal(E), cimag(E)); */
    /* printf("division X = %lf%+lfi\n", creal(X), cimag(X)); */
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = ccdoubles_cplx_neg(CPLX(1.2, 3.4));
    double complex	X = -O[0];
    ccdoubles_cplx_vector_neg (NITEMS, R, O);
    assert(E == R[0]);
    assert(E == X);
    assert(X == R[0]);
    /* printf("negation R = %lf%+lfi\n", creal(R[0]), cimag(R[0])); */
    /* printf("negation X = %lf%+lfi\n", creal(X), cimag(X)); */
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R;
    double complex	O1[2] = { CPLX(1.2, 2.3), CPLX(3.4, 4.5) };
    double complex	O2[2] = { CPLX(5.6, 6.7), CPLX(7.8, 8.9) };
    double complex	E =
      ccdoubles_cplx_mul(CPLX(1.2, 2.3), CPLX(5.6, 6.7)) +
      ccdoubles_cplx_mul(CPLX(3.4, 4.5), CPLX(7.8, 8.9));
    R = ccdoubles_cplx_vector_scalar_product (2, O1, O2);
    assert(E == R);
  }

  {
    double complex	R[NITEMS];
    double complex	lambda = CPLX(1.2, 3.4);
    double complex	O[NITEMS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_mul(lambda, CPLX(5.6, 7.8));
    ccdoubles_cplx_vector_scalar_mul (NITEMS, R, lambda, O);
    assert(E == R[0]);
  }

  {
    double complex	R[2];
    double complex	alpha = CPLX(1.2, 2.3);
    double complex	beta  = CPLX(3.4, 5.6);
    double complex	O1[2] = { CPLX(1.2, 2.3), CPLX(3.4, 4.5) };
    double complex	O2[2] = { CPLX(5.6, 6.7), CPLX(7.8, 8.9) };
    double complex	E1 = \
      ccdoubles_cplx_mul(alpha, O1[0]) + ccdoubles_cplx_mul(beta, O2[0]);
    double complex	E2 = \
      ccdoubles_cplx_mul(alpha, O1[1]) + ccdoubles_cplx_mul(beta, O2[1]);
    ccdoubles_cplx_vector_linear_combination (2, R, alpha, O1, beta, O2);
    assert(E1 == R[0]);
    assert(E2 == R[1]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = csin(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_sin (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = ccos(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_cos (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = ctan(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_tan (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = casin(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_asin (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = cacos(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_acos (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = catan(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_atan (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = csinh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_sinh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = ccosh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_cosh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(1.2, 3.4) };
    double complex	E = ctanh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_tanh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = casinh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_asinh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = cacosh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_acosh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NITEMS];
    double complex	O[NITEMS] = { CPLX(0.5, 0.6) };
    double complex	E = catanh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_atanh (NITEMS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }
}

/* end of file */
