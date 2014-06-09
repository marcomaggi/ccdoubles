/*
  Part of: CCDoubles
  Contents: tests for matrix functions
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

static void test_real_matrices (void);
static void test_cplx_matrices (void);

#define	NROWS		1
#define	NCOLS		1
#define EPSILON		1e-6

#define CPLX(REAL,IMAG)		CCDOUBLES_CPLX((REAL),(IMAG))


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc, const char *const argv[])
{
  test_real_matrices();
  test_cplx_matrices();
  exit(EXIT_SUCCESS);
}


/** --------------------------------------------------------------------
 ** Test real vectors.
 ** ----------------------------------------------------------------- */

void
test_real_matrices (void)
{
  {
    double	V[NROWS][NCOLS];
    ccdoubles_real_matrix_clear(NROWS, NCOLS, &V[0][0]);
    assert(0.0 == V[0][0]);
  }

  {
    double	V[NROWS][NCOLS];
    ccdoubles_real_matrix_set(NROWS, NCOLS, &V[0][0], 1.2);
    assert(1.2 == V[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	V[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_copy(NROWS, NCOLS, &R[0][0], &V[0][0]);
    assert(1.2 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_add (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((1.2 + 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_sub (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((1.2 - 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_mul (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((1.2 * 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_div (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((1.2 / 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_neg (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(-1.2 == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  -1.2 } };
    ccdoubles_real_matrix_abs (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(1.2 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_ceil (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(ceil(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_floor (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(floor(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_trunc (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(trunc(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_round (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(round(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_rint (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(rint(1.2) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isgreater (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(0 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isgreaterequal (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(0 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isless (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_islessequal (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_islessgreater (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isunordered (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(0 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_min (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(1.2 == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_max (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(3.4 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_fpclassify (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(FP_NORMAL == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isfinite (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isnormal (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isnan (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(0 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	lambda = 1.2;
    double	O[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_scalar_mul (NROWS, NCOLS, &R[0][0], lambda, &O[0][0]);
    assert((1.2 * 3.4) == R[0][0]);
  }

  {
    double	R[2][2];
    double	alpha = 1.2;
    double	beta  = 2.3;
    double	O1[2][2] = { { 3.4, 4.5 }, { 5.6, 6.7 } };
    double	O2[2][2] = { { 7.8, 8.9 }, { 9.0, 0.1 } };
    ccdoubles_real_matrix_linear_combination (2, 2, &R[0][0], alpha, &O1[0][0], beta, &O2[0][0]);
    assert((1.2 * 3.4 + 2.3 * 7.8) == R[0][0]);
    assert((1.2 * 4.5 + 2.3 * 8.9) == R[0][1]);
    assert((1.2 * 5.6 + 2.3 * 9.0) == R[1][0]);
    assert((1.2 * 6.7 + 2.3 * 0.1) == R[1][1]);
  }

  {
    double	R[3][2];
    double	O[2][3] = {
      { 1.2, 3.4, 5.6 },
      { 7.8, 8.9, 9.0 }
    };
    ccdoubles_real_matrix_transpose (2, 3, &R[0][0], &O[0][0]);
    assert(1.2 == R[0][0]); assert(7.8 == R[0][1]);
    assert(3.4 == R[1][0]); assert(8.9 == R[1][1]);
    assert(5.6 == R[2][0]); assert(9.0 == R[2][1]);
  }

  {
#undef RESULT_NROWS
#undef RESULT_NCOLS
#undef OPERAND_N
#define RESULT_NROWS		2
#define RESULT_NCOLS		3
#define OPERAND_N		4
    double	R[RESULT_NROWS][RESULT_NCOLS];
    double	O1[RESULT_NROWS][OPERAND_N] = {
      { 1.1, 1.2, 1.3, 1.4 },
      { 2.1, 2.2, 2.3, 2.4 }
    };
    double	O2[OPERAND_N][RESULT_NCOLS] = {
      { -1.1, -1.2, -1.3 },
      { -2.1, -2.2, -2.3 },
      { -3.1, -3.2, -3.3 },
      { -4.1, -4.2, -4.3 }
    };
    double	E[RESULT_NROWS][RESULT_NCOLS] = { /* expected result */
      { -13.5, -14.0, -14.5 },
      { -23.9, -24.8, -25.7 }
    };
    ccdoubles_real_matrix_rowcol_mul (RESULT_NROWS, OPERAND_N, RESULT_NCOLS,
				      &R[0][0], &O1[0][0], &O2[0][0]);
    if (0) {
      ccdoubles_real_matrix_print_display (stdout, "R", RESULT_NROWS, RESULT_NCOLS, &R[0][0]);
      ccdoubles_real_matrix_print_display (stdout, "E", RESULT_NROWS, RESULT_NCOLS, &E[0][0]);
    }
    assert(fabs(E[0][0]-R[0][0])<EPSILON); assert(fabs(E[0][1]-R[0][1])<EPSILON); assert(fabs(E[0][2]-R[0][2])<EPSILON);
    assert(fabs(E[1][0]-R[1][0])<EPSILON); assert(fabs(E[1][1]-R[1][1])<EPSILON); assert(fabs(E[1][2]-R[1][2])<EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_sin (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(sin(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_cos (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cos(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_tan (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(tan(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_asin (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(asin(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_acos (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(acos(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_atan (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(atan(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  0.5 } };
    double	O2[NROWS][NCOLS] = { {  0.6 } };
    ccdoubles_real_matrix_atan2 (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(atan2(0.5, 0.6) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_sinh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(sinh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_cosh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cosh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_tanh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(tanh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_asinh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(asinh(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_acosh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(acosh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.1 } };
    ccdoubles_real_matrix_atanh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(atanh(0.1) == R[0][0]);
  }
}


/** --------------------------------------------------------------------
 ** Test complex vectors.
 ** ----------------------------------------------------------------- */

void
test_cplx_matrices (void)
{
  {
    double complex	V[NROWS][NCOLS];
    double complex	E = CPLX(0.0, 0.0);
    ccdoubles_cplx_matrix_clear(NROWS, NCOLS, &V[0][0]);
    assert(E == V[0][0]);
  }

  {
    double complex	V[NROWS][NCOLS];
    double complex	E = CPLX(1.2, 3.4);
    ccdoubles_cplx_matrix_set(NROWS, NCOLS, &V[0][0], E);
    assert(E == V[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_copy (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(CPLX(1.2, 3.4) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_real (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(1.2 == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_imag (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(3.4 == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_magnitude (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(hypot(1.2, 3.4) == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_angle (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(carg(CPLX(1.2, 3.4)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    ccdoubles_cplx_matrix_conj (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(CPLX(1.2, -3.4) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double		O1[NROWS][NCOLS] = { { 1.2 } };
    double		O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_cplx_matrix_from_rect (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(CPLX(1.2, 3.4) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double		O1[NROWS][NCOLS] = { { 1.2 } };
    double		O2[NROWS][NCOLS] = { { 3.4 } };
    double		M = 1.2;
    double		A = 3.4;
    ccdoubles_cplx_matrix_from_polar (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(CPLX(M*cos(A), M*sin(A)) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { CPLX(4.5, 6.7) } };
    ccdoubles_cplx_matrix_add (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((CPLX(1.2, 3.4) + CPLX(4.5, 6.7)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { CPLX(4.5, 6.7) } };
    ccdoubles_cplx_matrix_sub (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert((CPLX(1.2, 3.4) - CPLX(4.5, 6.7)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { CPLX(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_mul(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    ccdoubles_cplx_matrix_mul (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(E == R[0][0]);
    assert((O1[0][0] * O2[0][0]) == R[0][0]);
    assert((O1[0][0] * O2[0][0]) == E);
    /* printf("multiplication E = %lf%+lfi\n", creal(E), cimag(E)); */
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { CPLX(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_div(CPLX(1.2, 3.4), CPLX(5.6, 7.8));
    double complex	X = CPLX(1.2, 3.4) / CPLX(5.6, 7.8);
    ccdoubles_cplx_matrix_div (NROWS, NCOLS, &R[0][0], &O1[0][0], &O2[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
    assert(cabs(X - R[0][0]) < EPSILON);
    assert(cabs(X - E)    < EPSILON);
    /* printf("division E = %lf%+lfi\n", creal(E), cimag(E)); */
    /* printf("division X = %lf%+lfi\n", creal(X), cimag(X)); */
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { CPLX(1.2, 3.4) } };
    double complex	E = ccdoubles_cplx_neg(CPLX(1.2, 3.4));
    double complex	X = -O[0][0];
    ccdoubles_cplx_matrix_neg (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(E == R[0][0]);
    assert(E == X);
    assert(X == R[0][0]);
    /* printf("negation R = %lf%+lfi\n", creal(R[0][0]), cimag(R[0][0])); */
    /* printf("negation X = %lf%+lfi\n", creal(X), cimag(X)); */
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	lambda = CPLX(1.2, 3.4);
    double complex	O[NROWS][NCOLS] = { { CPLX(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_mul(lambda, CPLX(5.6, 7.8));
    ccdoubles_cplx_matrix_scalar_mul (NROWS, NCOLS, &R[0][0], lambda, &O[0][0]);
    assert(E == R[0][0]);
  }

  {
    double complex	R[2][2];
    double complex	alpha = CPLX(1.2, 2.3);
    double complex	beta  = CPLX(3.4, 5.6);
    double complex	O1[2][2] = {
      { CPLX(1.2, 2.3), CPLX(3.4, 4.5) },
      { CPLX(5.6, 6.7), CPLX(7.8, 8.9) }
    };
    double complex	O2[2][2] = {
      { CPLX(9.0, 0.1), CPLX(1.2, 2.3) },
      { CPLX(3.4, 4.5), CPLX(5.6, 6.7) }
    };
    double complex	E00 = \
      ccdoubles_cplx_mul(alpha, O1[0][0]) + ccdoubles_cplx_mul(beta, O2[0][0]);
    double complex	E01 = \
      ccdoubles_cplx_mul(alpha, O1[0][1]) + ccdoubles_cplx_mul(beta, O2[0][1]);
    double complex	E10 =						\
      ccdoubles_cplx_mul(alpha, O1[1][0]) + ccdoubles_cplx_mul(beta, O2[1][0]);
    double complex	E11 = \
      ccdoubles_cplx_mul(alpha, O1[1][1]) + ccdoubles_cplx_mul(beta, O2[1][1]);
    ccdoubles_cplx_matrix_linear_combination (2, 2, &R[0][0], alpha, &O1[0][0], beta, &O2[0][0]);
    assert(E00 == R[0][0]);
    assert(E01 == R[0][1]);
    assert(E10 == R[1][0]);
    assert(E11 == R[1][1]);
  }

  {
    double complex	R[3][2];
    double complex	O[2][3] = {
      { CPLX(1.2, 0.0), CPLX(3.4, 0.0), CPLX(5.6, 0.0) },
      { CPLX(7.8, 0.0), CPLX(8.9, 0.0), CPLX(9.0, 0.0) }
    };
    ccdoubles_cplx_matrix_transpose (2, 3, &R[0][0], &O[0][0]);
    assert(CPLX(1.2, 0.0) == R[0][0]); assert(CPLX(7.8, 0.0) == R[0][1]);
    assert(CPLX(3.4, 0.0) == R[1][0]); assert(CPLX(8.9, 0.0) == R[1][1]);
    assert(CPLX(5.6, 0.0) == R[2][0]); assert(CPLX(9.0, 0.0) == R[2][1]);
  }

  {
    double complex	R[3][2];
    double complex	O[2][3] = {
      { CPLX(1.2, 2.3), CPLX(3.4, 4.5), CPLX(5.6, 6.7) },
      { CPLX(7.8, 8.9), CPLX(9.0, 0.1), CPLX(-1.2, -2.3) }
    };
    ccdoubles_cplx_matrix_conjugate_transpose (2, 3, &R[0][0], &O[0][0]);
    assert(CPLX(1.2, +2.3) == R[0][0]); assert(CPLX(7.8, -8.9) == R[0][1]);
    assert(CPLX(3.4, -4.5) == R[1][0]); assert(CPLX(9.0, +0.1) == R[1][1]);
    assert(CPLX(5.6, -6.7) == R[2][0]); assert(CPLX(-1.2, 2.3) == R[2][1]);
  }

  {
#undef RESULT_NROWS
#undef RESULT_NCOLS
#undef OPERAND_N
#define RESULT_NROWS		2
#define RESULT_NCOLS		3
#define OPERAND_N		4
    double complex	R[RESULT_NROWS][RESULT_NCOLS];
    double complex	O1[RESULT_NROWS][OPERAND_N] = {
      { CPLX(1.1,0.0), CPLX(1.2,0.0), CPLX(1.3,0.0), CPLX(1.4,0.0) },
      { CPLX(2.1,0.0), CPLX(2.2,0.0), CPLX(2.3,0.0), CPLX(2.4,0.0) }
    };
    double complex	O2[OPERAND_N][RESULT_NCOLS] = {
      { CPLX(-1.1,0.0),  CPLX(-1.2,0.0),  CPLX(-1.3,0.0) },
      { CPLX(-2.1,0.0),  CPLX(-2.2,0.0),  CPLX(-2.3,0.0) },
      { CPLX(-3.1,0.0),  CPLX(-3.2,0.0),  CPLX(-3.3,0.0) },
      { CPLX(-4.1,0.0),  CPLX(-4.2,0.0),  CPLX(-4.3,0.0) }
    };
    double complex	E[RESULT_NROWS][RESULT_NCOLS] = { /* expected result */
      { CPLX(-13.5,0.0), CPLX(-14.0,0.0), CPLX(-14.5,0.0) },
      { CPLX(-23.9,0.0), CPLX(-24.8,0.0), CPLX(-25.7,0.0) }
    };
    ccdoubles_cplx_matrix_rowcol_mul (RESULT_NROWS, OPERAND_N, RESULT_NCOLS,
				      &R[0][0], &O1[0][0], &O2[0][0]);
    if (0) {
      ccdoubles_cplx_matrix_print_display (stdout, "R", RESULT_NROWS, RESULT_NCOLS, &R[0][0]);
      ccdoubles_cplx_matrix_print_display (stdout, "E", RESULT_NROWS, RESULT_NCOLS, &E[0][0]);
    }
    assert(cabs(E[0][0]-R[0][0])<EPSILON); assert(cabs(E[0][1]-R[0][1])<EPSILON); assert(cabs(E[0][2]-R[0][2])<EPSILON);
    assert(cabs(E[1][0]-R[1][0])<EPSILON); assert(cabs(E[1][1]-R[1][1])<EPSILON); assert(cabs(E[1][2]-R[1][2])<EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = csin(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_sin (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = ccos(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_cos (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = ctan(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_tan (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = casin(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_asin (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = cacos(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_acos (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = catan(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_atan (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = csinh(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_sinh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = ccosh(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_cosh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(1.2, 3.4) } };
    double complex	E = ctanh(CPLX(1.2, 3.4));
    ccdoubles_cplx_matrix_tanh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = casinh(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_asinh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = cacosh(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_acosh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  CPLX(0.5, 0.6) } };
    double complex	E = catanh(CPLX(0.5, 0.6));
    ccdoubles_cplx_matrix_atanh (NROWS, NCOLS, &R[0][0], &O[0][0]);
    assert(cabs(E - R[0][0]) < EPSILON);
  }
}

/* end of file */
