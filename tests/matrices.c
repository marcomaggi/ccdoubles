/*
  Part of: CCDoubles
  Contents: tests for matrix functions
  Date: Sun Jun  8, 2014

  Abstract



  Copyright (C) 2014, 2015, 2017, 2019, 2020 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the file COPYING.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define CCDOUBLES_ENABLE_SHORT_MACROS		1
#include <ccdoubles.h>

static void test_real_matrices (void);
static void test_cplx_matrices (void);

#define	NROWS		1
#define	NCOLS		1
#define EPSILON		1e-6


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc CCLIB_UNUSED, const char *const argv[] CCLIB_UNUSED)
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
#undef NROWS
#undef NCOLS
#define	NROWS		1
#define	NCOLS		1

  /* { */
  /*   double	M[1][NROWS][NCOLS]; */
  /*   ccdoubles_real_matrix_clear(NROWS, NCOLS, M); */
  /*   assert(0.0 == M[0][0]); */
  /* } */

  {
    double	M[NROWS][NCOLS];
    ccdoubles_real_matrix_clear(NROWS, NCOLS, MREF(M));
    assert(0.0 == M[0][0]);
  }

  {
    double	M[NROWS][NCOLS];
    ccdoubles_real_matrix_set(NROWS, NCOLS, MREF(M), 1.2);
    assert(1.2 == M[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	M[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_copy(NROWS, NCOLS, MREF(R), MREF(M));
    assert(1.2 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_add (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((1.2 + 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_sub (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((1.2 - 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_mul (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((1.2 * 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_div (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((1.2 / 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_neg (NROWS, NCOLS, MREF(R), MREF(O));
    assert(-1.2 == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  -1.2 } };
    ccdoubles_real_matrix_abs (NROWS, NCOLS, MREF(R), MREF(O));
    assert(1.2 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_fmod (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(fmod(1.2, 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_drem (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(remainder(1.2, 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  1.2 } };
    double	O2[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_remainder (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(remainder(1.2, 3.4) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_ceil (NROWS, NCOLS, MREF(R), MREF(O));
    assert(ceil(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_floor (NROWS, NCOLS, MREF(R), MREF(O));
    assert(floor(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_trunc (NROWS, NCOLS, MREF(R), MREF(O));
    assert(trunc(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_round (NROWS, NCOLS, MREF(R), MREF(O));
    assert(round(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_rint (NROWS, NCOLS, MREF(R), MREF(O));
    assert(rint(1.2) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isgreater (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(0 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isgreaterequal (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(0 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isless (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_islessequal (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_islessgreater (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_isunordered (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(0 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_min (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(1.2 == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_max (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(3.4 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_fpclassify (NROWS, NCOLS, MREF(R), MREF(O));
    assert(FP_NORMAL == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isfinite (NROWS, NCOLS, MREF(R), MREF(O));
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isinfinite (NROWS, NCOLS, MREF(R), MREF(O));
    assert(0 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isnormal (NROWS, NCOLS, MREF(R), MREF(O));
    assert(1 == R[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { { 1.2 } };
    ccdoubles_real_matrix_isnan (NROWS, NCOLS, MREF(R), MREF(O));
    assert(0 == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	lambda = 1.2;
    double	O[NROWS][NCOLS] = { {  3.4 } };
    ccdoubles_real_matrix_scalar_mul (NROWS, NCOLS, MREF(R), lambda, MREF(O));
    assert((1.2 * 3.4) == R[0][0]);
  }

  {
    double	R[2][2];
    double	alpha = 1.2;
    double	beta  = 2.3;
    double	O1[2][2] = { { 3.4, 4.5 }, { 5.6, 6.7 } };
    double	O2[2][2] = { { 7.8, 8.9 }, { 9.0, 0.1 } };
    ccdoubles_real_matrix_linear_combination (2, 2, MREF(R), alpha, MREF(O1), beta, MREF(O2));
    assert((1.2 * 3.4 + 2.3 * 7.8) == R[0][0]);
    assert((1.2 * 4.5 + 2.3 * 8.9) == R[0][1]);
    assert((1.2 * 5.6 + 2.3 * 9.0) == R[1][0]);
    assert((1.2 * 6.7 + 2.3 * 0.1) == R[1][1]);
  }

  { /* transposition with different operand and result */
    double	R[3][2];
    double	O[2][3] = {
      { 1.2, 3.4, 5.6 },
      { 7.8, 8.9, 9.0 }
    };
    ccdoubles_real_matrix_transpose (2, 3, MREF(R), MREF(O));
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "R", 3, 2, MREF(R));
    assert(1.2 == R[0][0]); assert(7.8 == R[0][1]);
    assert(3.4 == R[1][0]); assert(8.9 == R[1][1]);
    assert(5.6 == R[2][0]); assert(9.0 == R[2][1]);
  }
  { /* transposition with coincident operand and result */
    double	O[3][3] = {
      { 1.1, 1.2, 1.3 },
      { 2.1, 2.2, 2.3 },
      { 3.1, 3.2, 3.3 }
    };
    ccdoubles_real_matrix_transpose (3, 3, MREF(O), MREF(O));
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "O", 3, 3, MREF(O));
    assert(1.1 == O[0][0]); assert(2.1 == O[0][1]); assert(3.1 == O[0][2]);
    assert(1.2 == O[1][0]); assert(2.2 == O[1][1]); assert(3.2 == O[1][2]);
    assert(1.3 == O[2][0]); assert(2.3 == O[2][1]); assert(3.3 == O[2][2]);
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
				      MREF(R), MREF(O1), MREF(O2));
    if (0) {
      ccdoubles_real_matrix_print_display (stdout, "R", RESULT_NROWS, RESULT_NCOLS, MREF(R));
      ccdoubles_real_matrix_print_display (stdout, "E", RESULT_NROWS, RESULT_NCOLS, MREF(E));
    }
    assert(fabs(E[0][0]-R[0][0])<EPSILON); assert(fabs(E[0][1]-R[0][1])<EPSILON); assert(fabs(E[0][2]-R[0][2])<EPSILON);
    assert(fabs(E[1][0]-R[1][0])<EPSILON); assert(fabs(E[1][1]-R[1][1])<EPSILON); assert(fabs(E[1][2]-R[1][2])<EPSILON);
  }

  {
    unsigned	nrows = 10;
    unsigned	ncols = 10;
    double	R[nrows][ncols];
    double	start    = 0.0;
    double	row_past = 10.0;
    double	col_past = 10.0;
    ccdoubles_real_matrix_linspace (nrows, ncols, MREF(R), start, row_past, col_past);
    if (1) {
      ccdoubles_real_matrix_print_brackets(stdout, nrows, ncols, MREF(R));
    }
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_exp (NROWS, NCOLS, MREF(R), MREF(O));
    assert(exp(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_exp10 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(exp(1.2 * log(10.0)) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_exp2 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(exp2(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_log (NROWS, NCOLS, MREF(R), MREF(O));
    assert(log(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_log10 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(log10(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_log2 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(log2(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_logb (NROWS, NCOLS, MREF(R), MREF(O));
    assert(logb(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_pow (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(pow(1.2, 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_sqrt (NROWS, NCOLS, MREF(R), MREF(O));
    assert(sqrt(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_cbrt (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cbrt(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { { 1.2 } };
    double	O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_real_matrix_hypot (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(hypot(1.2, 3.4) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_expm1 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(expm1(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_log1p (NROWS, NCOLS, MREF(R), MREF(O));
    assert(log1p(1.2) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	E = cexp(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_exp (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	E = clog(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_log (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	E = Z(log10(cabs(Z(1.2, 3.4))), carg(Z(1.2, 3.4)));
    ccdoubles_cplx_matrix_log10 (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	E = csqrt(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_sqrt (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { Z(5.6, 7.8) } };
    double complex	E = cpow(Z(1.2, 3.4), Z(5.6, 7.8));
    ccdoubles_cplx_matrix_pow (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_sin (NROWS, NCOLS, MREF(R), MREF(O));
    assert(sin(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_cos (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cos(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_tan (NROWS, NCOLS, MREF(R), MREF(O));
    assert(tan(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_asin (NROWS, NCOLS, MREF(R), MREF(O));
    assert(asin(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_acos (NROWS, NCOLS, MREF(R), MREF(O));
    assert(acos(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_atan (NROWS, NCOLS, MREF(R), MREF(O));
    assert(atan(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O1[NROWS][NCOLS] = { {  0.5 } };
    double	O2[NROWS][NCOLS] = { {  0.6 } };
    ccdoubles_real_matrix_atan2 (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(atan2(0.5, 0.6) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_sinh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(sinh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_cosh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cosh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_tanh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(tanh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.5 } };
    ccdoubles_real_matrix_asinh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(asinh(0.5) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  1.2 } };
    ccdoubles_real_matrix_acosh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(acosh(1.2) == R[0][0]);
  }

  {
    double	R[NROWS][NCOLS];
    double	O[NROWS][NCOLS] = { {  0.1 } };
    ccdoubles_real_matrix_atanh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(atanh(0.1) == R[0][0]);
  }

  { /* row-to-row copy, full row */
#undef NROWS
#undef NCOLS
#define	NROWS		4
#define	NCOLS		5
    double	O[NROWS][NCOLS] = {
      { 0.0, 0.1, 0.2, 0.3, 0.4 },
      { 1.0, 1.1, 1.2, 1.3, 1.4 },
      { 2.0, 2.1, 2.2, 2.3, 2.4 },
      { 3.0, 3.1, 3.2, 3.3, 3.4 }
    };
    double	R[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    double	E[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 1.0, 1.1, 1.2, 1.3, 1.4 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    ccdoubles_real_matrix_row_to_row (NROWS, NCOLS, MREF(R),
				      NROWS, NCOLS, MREF(O),
				      2, 0, /* coordinates of first item in R */
				      1, 0, /* coordinates of first item in O */
				      NCOLS);
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "R", NROWS, NCOLS, MREF(R));
    for (unsigned i=0; i<NROWS; ++i) {
      for (unsigned j=0; j<NCOLS; ++j) {
	if (0) fprintf(stderr, "i=%u, j=%u, %f, %f\n", i, j, E[i][j], R[i][j]);
	assert(E[i][j] == R[i][j]);
      }
    }
  }

  { /* row-to-row copy, sub row */
#undef NROWS
#undef NCOLS
#define	NROWS		4
#define	NCOLS		5
    double	O[NROWS][NCOLS] = {
      { 0.0, 0.1, 0.2, 0.3, 0.4 },
      { 1.0, 1.1, 1.2, 1.3, 1.4 },
      { 2.0, 2.1, 2.2, 2.3, 2.4 },
      { 3.0, 3.1, 3.2, 3.3, 3.4 }
    };
    double	R[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    double	E[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 1.1, 1.2, 1.3, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    ccdoubles_real_matrix_row_to_row (NROWS, NCOLS, MREF(R),
				      NROWS, NCOLS, MREF(O),
				      2, 1, /* coordinates of first item in R */
				      1, 1, /* coordinates of first item in O */
				      3);
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "R", NROWS, NCOLS, MREF(R));
    for (unsigned i=0; i<NROWS; ++i) {
      for (unsigned j=0; j<NCOLS; ++j) {
	if (0) fprintf(stderr, "i=%u, j=%u, %f, %f\n", i, j, E[i][j], R[i][j]);
	assert(E[i][j] == R[i][j]);
      }
    }
  }

  { /* col-to-col copy, full column */
#undef NROWS
#undef NCOLS
#define	NROWS		4
#define	NCOLS		5
    double	O[NROWS][NCOLS] = {
      { 0.0, 0.1, 0.2, 0.3, 0.4 },
      { 1.0, 1.1, 1.2, 1.3, 1.4 },
      { 2.0, 2.1, 2.2, 2.3, 2.4 },
      { 3.0, 3.1, 3.2, 3.3, 3.4 }
    };
    double	R[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    static const double	E[NROWS][NCOLS] = {
      { 0.0, 0.2, 0.0, 0.0, 0.0 },
      { 0.0, 1.2, 0.0, 0.0, 0.0 },
      { 0.0, 2.2, 0.0, 0.0, 0.0 },
      { 0.0, 3.2, 0.0, 0.0, 0.0 }
    };
    ccdoubles_real_matrix_col_to_col (NROWS, NCOLS, MREF(R),
				      NROWS, NCOLS, MREF(O),
				      0, 1, /* coordinates of first item in R */
				      0, 2, /* coordinates of first item in O */
				      NROWS);
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "R", NROWS, NCOLS, MREF(R));
    for (unsigned i=0; i<NROWS; ++i) {
      for (unsigned j=0; j<NCOLS; ++j) {
	if (0) fprintf(stderr, "i=%u, j=%u, E=%f, R=%f\n", i, j, E[i][j], R[i][j]);
	assert(E[i][j] == R[i][j]);
      }
    }
  }

  { /* col-to-col copy, sub column */
#undef NROWS
#undef NCOLS
#define	NROWS		4
#define	NCOLS		5
    double	O[NROWS][NCOLS] = {
      { 0.0, 0.1, 0.2, 0.3, 0.4 },
      { 1.0, 1.1, 1.2, 1.3, 1.4 },
      { 2.0, 2.1, 2.2, 2.3, 2.4 },
      { 3.0, 3.1, 3.2, 3.3, 3.4 }
    };
    double	R[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    static const double	E[NROWS][NCOLS] = {
      { 0.0, 0.0, 0.0, 0.0, 0.0 },
      { 0.0, 2.2, 0.0, 0.0, 0.0 },
      { 0.0, 3.2, 0.0, 0.0, 0.0 },
      { 0.0, 0.0, 0.0, 0.0, 0.0 }
    };
    ccdoubles_real_matrix_col_to_col (NROWS, NCOLS, MREF(R),
				      NROWS, NCOLS, MREF(O),
				      1, 1, /* coordinates of first item in R */
				      2, 2, /* coordinates of first item in O */
				      2);
    if (0)
      ccdoubles_real_matrix_print_display (stderr, "R", NROWS, NCOLS, MREF(R));
    for (unsigned i=0; i<NROWS; ++i) {
      for (unsigned j=0; j<NCOLS; ++j) {
	if (0) fprintf(stderr, "i=%u, j=%u, E=%f, R=%f\n", i, j, E[i][j], R[i][j]);
	assert(E[i][j] == R[i][j]);
      }
    }
  }

}


/** --------------------------------------------------------------------
 ** Test complex vectors.
 ** ----------------------------------------------------------------- */

void
test_cplx_matrices (void)
{
#undef NROWS
#undef NCOLS
#define	NROWS		1
#define	NCOLS		1

  {
    double complex	M[NROWS][NCOLS];
    double complex	E = Z(0.0, 0.0);
    ccdoubles_cplx_matrix_clear(NROWS, NCOLS, MREF(M));
    assert(E == M[0][0]);
  }

  {
    double complex	M[NROWS][NCOLS];
    double complex	E = Z(1.2, 3.4);
    ccdoubles_cplx_matrix_set(NROWS, NCOLS, MREF(M), E);
    assert(E == M[0][0]);
  }

  {
    double complex	M[NROWS][NCOLS];
    double complex	E = Z(1.2, 3.4);
    ccdoubles_cplx_matrix_set_split(NROWS, NCOLS, MREF(M), Re(E), Im(E));
    assert(E == M[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_copy (NROWS, NCOLS, MREF(R), MREF(O));
    assert(Z(1.2, 3.4) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_real (NROWS, NCOLS, MREF(R), MREF(O));
    assert(1.2 == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_imag (NROWS, NCOLS, MREF(R), MREF(O));
    assert(3.4 == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_magnitude (NROWS, NCOLS, MREF(R), MREF(O));
    assert(hypot(1.2, 3.4) == R[0][0]);
  }

  {
    double		R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_angle (NROWS, NCOLS, MREF(R), MREF(O));
    assert(carg(Z(1.2, 3.4)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    ccdoubles_cplx_matrix_conj (NROWS, NCOLS, MREF(R), MREF(O));
    assert(Z(1.2, -3.4) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double		O1[NROWS][NCOLS] = { { 1.2 } };
    double		O2[NROWS][NCOLS] = { { 3.4 } };
    ccdoubles_cplx_matrix_from_rect (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(Z(1.2, 3.4) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double		O1[NROWS][NCOLS] = { { 1.2 } };
    double		O2[NROWS][NCOLS] = { { 3.4 } };
    double		M = 1.2;
    double		A = 3.4;
    ccdoubles_cplx_matrix_from_polar (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(Z(M*cos(A), M*sin(A)) == R[0][0]);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { Z(4.5, 6.7) } };
    ccdoubles_cplx_matrix_add (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((Z(1.2, 3.4) + Z(4.5, 6.7)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { Z(4.5, 6.7) } };
    ccdoubles_cplx_matrix_sub (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert((Z(1.2, 3.4) - Z(4.5, 6.7)) == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { Z(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_mul(Z(1.2, 3.4), Z(5.6, 7.8));
    ccdoubles_cplx_matrix_mul (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(E == R[0][0]);
    assert((O1[0][0] * O2[0][0]) == R[0][0]);
    assert((O1[0][0] * O2[0][0]) == E);
    /* printf("multiplication E = %lf%+lfi\n", Re(E), Im(E)); */
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O1[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	O2[NROWS][NCOLS] = { { Z(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_div(Z(1.2, 3.4), Z(5.6, 7.8));
    double complex	X = Z(1.2, 3.4) / Z(5.6, 7.8);
    ccdoubles_cplx_matrix_div (NROWS, NCOLS, MREF(R), MREF(O1), MREF(O2));
    assert(cabs(E - R[0][0]) < EPSILON);
    assert(cabs(X - R[0][0]) < EPSILON);
    assert(cabs(X - E)    < EPSILON);
    /* printf("division E = %lf%+lfi\n", Re(E), Im(E)); */
    /* printf("division X = %lf%+lfi\n", Re(X), Im(X)); */
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { { Z(1.2, 3.4) } };
    double complex	E = ccdoubles_cplx_neg(Z(1.2, 3.4));
    double complex	X = -O[0][0];
    ccdoubles_cplx_matrix_neg (NROWS, NCOLS, MREF(R), MREF(O));
    assert(E == R[0][0]);
    assert(E == X);
    assert(X == R[0][0]);
    /* printf("negation R = %lf%+lfi\n", Re(R[0][0]), Im(R[0][0])); */
    /* printf("negation X = %lf%+lfi\n", Re(X), Im(X)); */
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	lambda = Z(1.2, 3.4);
    double complex	O[NROWS][NCOLS] = { { Z(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_mul(lambda, Z(5.6, 7.8));
    ccdoubles_cplx_matrix_scalar_mul (NROWS, NCOLS, MREF(R), lambda, MREF(O));
    assert(E == R[0][0]);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	lambda = Z(1.2, 3.4);
    double complex	O[NROWS][NCOLS] = { { Z(5.6, 7.8) } };
    double complex	E = ccdoubles_cplx_mul(lambda, Z(5.6, 7.8));
    ccdoubles_cplx_matrix_scalar_mul_split (NROWS, NCOLS, MREF(R), Re(lambda), Im(lambda), MREF(O));
    assert(E == R[0][0]);
  }

  {
    double complex	R[2][2];
    double complex	alpha = Z(1.2, 2.3);
    double complex	beta  = Z(3.4, 5.6);
    double complex	O1[2][2] = {
      { Z(1.2, 2.3), Z(3.4, 4.5) },
      { Z(5.6, 6.7), Z(7.8, 8.9) }
    };
    double complex	O2[2][2] = {
      { Z(9.0, 0.1), Z(1.2, 2.3) },
      { Z(3.4, 4.5), Z(5.6, 6.7) }
    };
    double complex	E00 = \
      ccdoubles_cplx_mul(alpha, O1[0][0]) + ccdoubles_cplx_mul(beta, O2[0][0]);
    double complex	E01 = \
      ccdoubles_cplx_mul(alpha, O1[0][1]) + ccdoubles_cplx_mul(beta, O2[0][1]);
    double complex	E10 =						\
      ccdoubles_cplx_mul(alpha, O1[1][0]) + ccdoubles_cplx_mul(beta, O2[1][0]);
    double complex	E11 = \
      ccdoubles_cplx_mul(alpha, O1[1][1]) + ccdoubles_cplx_mul(beta, O2[1][1]);
    ccdoubles_cplx_matrix_linear_combination (2, 2, MREF(R), alpha, MREF(O1), beta, MREF(O2));
    assert(E00 == R[0][0]);
    assert(E01 == R[0][1]);
    assert(E10 == R[1][0]);
    assert(E11 == R[1][1]);
  }

  {
    double complex	R[2][2];
    double complex	alpha = Z(1.2, 2.3);
    double complex	beta  = Z(3.4, 5.6);
    double complex	O1[2][2] = {
      { Z(1.2, 2.3), Z(3.4, 4.5) },
      { Z(5.6, 6.7), Z(7.8, 8.9) }
    };
    double complex	O2[2][2] = {
      { Z(9.0, 0.1), Z(1.2, 2.3) },
      { Z(3.4, 4.5), Z(5.6, 6.7) }
    };
    double complex	E00 = \
      ccdoubles_cplx_mul(alpha, O1[0][0]) + ccdoubles_cplx_mul(beta, O2[0][0]);
    double complex	E01 = \
      ccdoubles_cplx_mul(alpha, O1[0][1]) + ccdoubles_cplx_mul(beta, O2[0][1]);
    double complex	E10 =						\
      ccdoubles_cplx_mul(alpha, O1[1][0]) + ccdoubles_cplx_mul(beta, O2[1][0]);
    double complex	E11 = \
      ccdoubles_cplx_mul(alpha, O1[1][1]) + ccdoubles_cplx_mul(beta, O2[1][1]);
    ccdoubles_cplx_matrix_linear_combination_split (2, 2, MREF(R),
						    Re(alpha), Im(alpha), MREF(O1),
						    Re(beta),  Im(beta),  MREF(O2));
    assert(E00 == R[0][0]);
    assert(E01 == R[0][1]);
    assert(E10 == R[1][0]);
    assert(E11 == R[1][1]);
  }

  { /* transposition with distinct operand and result */
    double complex	R[3][2];
    double complex	O[2][3] = {
      { Z(1.2, 0.0), Z(3.4, 0.0), Z(5.6, 0.0) },
      { Z(7.8, 0.0), Z(8.9, 0.0), Z(9.0, 0.0) }
    };
    ccdoubles_cplx_matrix_transpose (2, 3, MREF(R), MREF(O));
    assert(Z(1.2, 0.0) == R[0][0]); assert(Z(7.8, 0.0) == R[0][1]);
    assert(Z(3.4, 0.0) == R[1][0]); assert(Z(8.9, 0.0) == R[1][1]);
    assert(Z(5.6, 0.0) == R[2][0]); assert(Z(9.0, 0.0) == R[2][1]);
  }
  { /* transposition with coincident operand and result */
    double complex	O[3][3] = {
      { Z(1.1,0.1), Z(1.2,0.1), Z(1.3,0.1) },
      { Z(2.1,0.1), Z(2.2,0.1), Z(2.3,0.1) },
      { Z(3.1,0.1), Z(3.2,0.1), Z(3.3,0.1) }
    };
    ccdoubles_cplx_matrix_transpose (3, 3, MREF(O), MREF(O));
    if (0)
      ccdoubles_cplx_matrix_print_display (stderr, "O", 3, 3, MREF(O));
    assert(Z(1.1,0.1) == O[0][0]); assert(Z(2.1,0.1) == O[0][1]); assert(Z(3.1,0.1) == O[0][2]);
    assert(Z(1.2,0.1) == O[1][0]); assert(Z(2.2,0.1) == O[1][1]); assert(Z(3.2,0.1) == O[1][2]);
    assert(Z(1.3,0.1) == O[2][0]); assert(Z(2.3,0.1) == O[2][1]); assert(Z(3.3,0.1) == O[2][2]);
  }

  { /* conjugate-transpose with distinct operand and result */
    double complex	R[3][2];
    double complex	O[2][3] = {
      { Z(1.2, 2.3), Z(3.4, 4.5), Z(5.6, 6.7) },
      { Z(7.8, 8.9), Z(9.0, 0.1), Z(-1.2, -2.3) }
    };
    ccdoubles_cplx_matrix_conjugate_transpose (2, 3, MREF(R), MREF(O));
    assert(Z(1.2, -2.3) == R[0][0]); assert(Z(7.8, -8.9) == R[0][1]);
    assert(Z(3.4, -4.5) == R[1][0]); assert(Z(9.0, -0.1) == R[1][1]);
    assert(Z(5.6, -6.7) == R[2][0]); assert(Z(-1.2, 2.3) == R[2][1]);
  }
  { /* conjugate-transpose with coincident operand and result */
    double complex	O[3][3] = {
      { Z(1.1, 11.11), Z(1.2, 11.22), Z(1.3, 11.33) },
      { Z(2.1, 22.11), Z(2.2, 22.22), Z(2.3, 22.33) },
      { Z(3.1, 33.11), Z(3.2, 33.22), Z(3.3, 33.33) }
    };
    ccdoubles_cplx_matrix_conjugate_transpose (3, 3, MREF(O), MREF(O));
    if (0)
      ccdoubles_cplx_matrix_print_display (stderr, "O", 3, 3, MREF(O));
    assert(Z(1.1, -11.11) == O[0][0]); assert(Z(2.1, -22.11) == O[0][1]); assert(Z(3.1, -33.11) == O[0][2]);
    assert(Z(1.2, -11.22) == O[1][0]); assert(Z(2.2, -22.22) == O[1][1]); assert(Z(3.2, -33.22) == O[1][2]);
    assert(Z(1.3, -11.33) == O[2][0]); assert(Z(2.3, -22.33) == O[2][1]); assert(Z(3.3, -33.33) == O[2][2]);
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
      { Z(1.1,0.0), Z(1.2,0.0), Z(1.3,0.0), Z(1.4,0.0) },
      { Z(2.1,0.0), Z(2.2,0.0), Z(2.3,0.0), Z(2.4,0.0) }
    };
    double complex	O2[OPERAND_N][RESULT_NCOLS] = {
      { Z(-1.1,0.0),  Z(-1.2,0.0),  Z(-1.3,0.0) },
      { Z(-2.1,0.0),  Z(-2.2,0.0),  Z(-2.3,0.0) },
      { Z(-3.1,0.0),  Z(-3.2,0.0),  Z(-3.3,0.0) },
      { Z(-4.1,0.0),  Z(-4.2,0.0),  Z(-4.3,0.0) }
    };
    double complex	E[RESULT_NROWS][RESULT_NCOLS] = { /* expected result */
      { Z(-13.5,0.0), Z(-14.0,0.0), Z(-14.5,0.0) },
      { Z(-23.9,0.0), Z(-24.8,0.0), Z(-25.7,0.0) }
    };
    ccdoubles_cplx_matrix_rowcol_mul (RESULT_NROWS, OPERAND_N, RESULT_NCOLS,
				      MREF(R), MREF(O1), MREF(O2));
    if (0) {
      ccdoubles_cplx_matrix_print_display (stdout, "R", RESULT_NROWS, RESULT_NCOLS, MREF(R));
      ccdoubles_cplx_matrix_print_display (stdout, "E", RESULT_NROWS, RESULT_NCOLS, MREF(E));
    }
    assert(cabs(E[0][0]-R[0][0])<EPSILON); assert(cabs(E[0][1]-R[0][1])<EPSILON); assert(cabs(E[0][2]-R[0][2])<EPSILON);
    assert(cabs(E[1][0]-R[1][0])<EPSILON); assert(cabs(E[1][1]-R[1][1])<EPSILON); assert(cabs(E[1][2]-R[1][2])<EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = csin(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_sin (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = ccos(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_cos (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = ctan(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_tan (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = casin(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_asin (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = cacos(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_acos (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = catan(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_atan (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

/* ------------------------------------------------------------------ */

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = csinh(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_sinh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = ccosh(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_cosh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(1.2, 3.4) } };
    double complex	E = ctanh(Z(1.2, 3.4));
    ccdoubles_cplx_matrix_tanh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = casinh(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_asinh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = cacosh(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_acosh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }

  {
    double complex	R[NROWS][NCOLS];
    double complex	O[NROWS][NCOLS] = { {  Z(0.5, 0.6) } };
    double complex	E = catanh(Z(0.5, 0.6));
    ccdoubles_cplx_matrix_atanh (NROWS, NCOLS, MREF(R), MREF(O));
    assert(cabs(E - R[0][0]) < EPSILON);
  }
}

/* end of file */
