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

#define	NSLOTS		1
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
    double	V[NSLOTS];
    ccdoubles_real_vector_clear(NSLOTS, V);
    assert(0.0 == V[0]);
  }

  {
    double	V[NSLOTS];
    ccdoubles_real_vector_set(NSLOTS, V, 1.2);
    assert(1.2 == V[0]);
  }

  {
    double	R[NSLOTS];
    double	V[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_copy(NSLOTS, R, V);
    assert(1.2 == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_add (NSLOTS, R, O1, O2);
    assert((1.2 + 3.4) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_sub (NSLOTS, R, O1, O2);
    assert((1.2 - 3.4) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_mul (NSLOTS, R, O1, O2);
    assert((1.2 * 3.4) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_div (NSLOTS, R, O1, O2);
    assert((1.2 / 3.4) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_neg (NSLOTS, R, O);
    assert(-1.2 == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { -1.2 };
    ccdoubles_real_vector_abs (NSLOTS, R, O);
    assert(1.2 == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_ceil (NSLOTS, R, O);
    assert(ceil(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_floor (NSLOTS, R, O);
    assert(floor(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_trunc (NSLOTS, R, O);
    assert(trunc(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_round (NSLOTS, R, O);
    assert(round(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_rint (NSLOTS, R, O);
    assert(rint(1.2) == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_isgreater (NSLOTS, R, O1, O2);
    assert(0 == R[0]);
  }

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_isgreaterequal (NSLOTS, R, O1, O2);
    assert(0 == R[0]);
  }

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_isless (NSLOTS, R, O1, O2);
    assert(1 == R[0]);
  }

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_islessequal (NSLOTS, R, O1, O2);
    assert(1 == R[0]);
  }

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_islessgreater (NSLOTS, R, O1, O2);
    assert(1 == R[0]);
  }

  {
    int		R[NSLOTS];
    double	O1[NSLOTS] = { 1.2 };
    double	O2[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_isunordered (NSLOTS, R, O1, O2);
    assert(0 == R[0]);
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
    double	R[NSLOTS];
    double	lambda = 1.2;
    double	O[NSLOTS] = { 3.4 };
    ccdoubles_real_vector_scalar_mul (NSLOTS, R, lambda, O);
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
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_sin (NSLOTS, R, O);
    assert(sin(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_cos (NSLOTS, R, O);
    assert(cos(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_tan (NSLOTS, R, O);
    assert(tan(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 0.5 };
    ccdoubles_real_vector_asin (NSLOTS, R, O);
    assert(asin(0.5) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 0.5 };
    ccdoubles_real_vector_acos (NSLOTS, R, O);
    assert(acos(0.5) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 0.5 };
    ccdoubles_real_vector_atan (NSLOTS, R, O);
    assert(atan(0.5) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O1[NSLOTS] = { 0.5 };
    double	O2[NSLOTS] = { 0.6 };
    ccdoubles_real_vector_atan2 (NSLOTS, R, O1, O2);
    assert(atan2(0.5, 0.6) == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_sinh (NSLOTS, R, O);
    assert(sinh(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_cosh (NSLOTS, R, O);
    assert(cosh(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_tanh (NSLOTS, R, O);
    assert(tanh(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 0.5 };
    ccdoubles_real_vector_asinh (NSLOTS, R, O);
    assert(asinh(0.5) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 1.2 };
    ccdoubles_real_vector_acosh (NSLOTS, R, O);
    assert(acosh(1.2) == R[0]);
  }

  {
    double	R[NSLOTS];
    double	O[NSLOTS] = { 0.1 };
    ccdoubles_real_vector_atanh (NSLOTS, R, O);
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
    double complex	V[NSLOTS];
    double complex	E = CPLX(0.0, 0.0);
    ccdoubles_cplx_vector_clear(NSLOTS, V);
    assert(E == V[0]);
  }

  {
    double complex	V[NSLOTS];
    double complex	E = CPLX(1.2, 3.4);
    ccdoubles_cplx_vector_set(NSLOTS, V, E);
    assert(E == V[0]);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_copy (NSLOTS, R, O);
    assert(CPLX(1.2, 3.4) == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double		R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_real (NSLOTS, R, O);
    assert(1.2 == R[0]);
  }

  {
    double		R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_imag (NSLOTS, R, O);
    assert(3.4 == R[0]);
  }

  {
    double		R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_magnitude (NSLOTS, R, O);
    assert(hypot(1.2, 3.4) == R[0]);
  }

  {
    double		R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_angle (NSLOTS, R, O);
    assert(carg(CPLX(1.2, 3.4)) == R[0]);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    ccdoubles_cplx_vector_conj (NSLOTS, R, O);
    assert(CPLX(1.2, -3.4) == R[0]);
  }

  {
    double complex	R[NSLOTS];
    double		O1[NSLOTS] = { 1.2 };
    double		O2[NSLOTS] = { 3.4 };
    ccdoubles_cplx_vector_from_rect (NSLOTS, R, O1, O2);
    assert(CPLX(1.2, 3.4) == R[0]);
  }

  {
    double complex	R[NSLOTS];
    double		O1[NSLOTS] = { 1.2 };
    double		O2[NSLOTS] = { 3.4 };
    double		M = 1.2;
    double		A = 3.4;
    ccdoubles_cplx_vector_from_polar (NSLOTS, R, O1, O2);
    assert(CPLX(M*cos(A), M*sin(A)) == R[0]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NSLOTS];
    double complex	O1[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	O2[NSLOTS] = { CPLX(4.5, 6.7) };
    ccdoubles_cplx_vector_add (NSLOTS, R, O1, O2);
    assert((CPLX(1.2, 3.4) + CPLX(4.5, 6.7)) == R[0]);
  }

  {
    double complex	R[NSLOTS];
    double complex	O1[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	O2[NSLOTS] = { CPLX(4.5, 6.7) };
    ccdoubles_cplx_vector_sub (NSLOTS, R, O1, O2);
    assert((CPLX(1.2, 3.4) - CPLX(4.5, 6.7)) == R[0]);
  }

  {
    double complex	R[NSLOTS];
    double complex	O1[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	O2[NSLOTS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_mul(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    ccdoubles_cplx_vector_mul (NSLOTS, R, O1, O2);
    assert(E == R[0]);
    assert((O1[0] * O2[0]) == R[0]);
    assert((O1[0] * O2[0]) == E);
    /* printf("multiplication E = %lf%+lfi\n", creal(E), cimag(E)); */
  }

  {
    double complex	R[NSLOTS];
    double complex	O1[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	O2[NSLOTS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_div(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    double complex	X = CPLX(1.2, 3.4) / CPLX(5.6, 7.8);
    ccdoubles_cplx_vector_div (NSLOTS, R, O1, O2);
    assert(cabs(E - R[0]) < EPSILON);
    assert(cabs(X - R[0]) < EPSILON);
    assert(cabs(X - E)    < EPSILON);
    /* printf("division E = %lf%+lfi\n", creal(E), cimag(E)); */
    /* printf("division X = %lf%+lfi\n", creal(X), cimag(X)); */
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = ccdoubles_cplx_neg(CPLX(1.2, 3.4));
    double complex	X = -O[0];
    ccdoubles_cplx_vector_neg (NSLOTS, R, O);
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
    double complex	R[NSLOTS];
    double complex	lambda = CPLX(1.2, 3.4);
    double complex	O[NSLOTS] = { CPLX(5.6, 7.8) };
    double complex	E = ccdoubles_cplx_mul(lambda, CPLX(5.6, 7.8));
    ccdoubles_cplx_vector_scalar_mul (NSLOTS, R, lambda, O);
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
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = csin(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_sin (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = ccos(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_cos (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = ctan(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_tan (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = casin(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_asin (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = cacos(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_acos (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = catan(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_atan (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = csinh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_sinh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = ccosh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_cosh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(1.2, 3.4) };
    double complex	E = ctanh(CPLX(1.2, 3.4));
    ccdoubles_cplx_vector_tanh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = casinh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_asinh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = cacosh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_acosh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }

  {
    double complex	R[NSLOTS];
    double complex	O[NSLOTS] = { CPLX(0.5, 0.6) };
    double complex	E = catanh(CPLX(0.5, 0.6));
    ccdoubles_cplx_vector_atanh (NSLOTS, R, O);
    assert(cabs(E - R[0]) < EPSILON);
  }
}

/* end of file */
