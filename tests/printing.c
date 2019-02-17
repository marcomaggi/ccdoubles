/*
  Part of: CCDoubles
  Contents: tests for printing routines
  Date: Mon Jun  9, 2014

  Abstract



  Copyright (C) 2014, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the file COPYING.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <ccdoubles.h>

#define CPLX(REAL,IMAG)		CCDOUBLES_CPLX((REAL),(IMAG))


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc CCDOUBLES_UNUSED, const char *const argv[] CCDOUBLES_UNUSED)
{
  printf("\n***** Display tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    double	O[NSLOTS]= { 1.1, 1.2, 1.3 };
    printf("\n--- real vector:\n\n");
    ccdoubles_real_vector_print_display(stdout, "O", NSLOTS, O);
  }

  {
#undef NSLOTS
#define NSLOTS		3
    double complex	O[NSLOTS] = {
      CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3)
    };
    printf("\n--- complex vector:\n\n");
    ccdoubles_cplx_vector_print_display(stdout, "O", NSLOTS, O);
  }

  {
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double	O[NROWS][NCOLS] = {
      { 1.1, 1.2, 1.3 },
      { 2.1, 2.2, 2.3 }
    };
    printf("\n--- real matrix:\n\n");
    ccdoubles_real_matrix_print_display(stdout, "O", NROWS, NCOLS, &O[0][0]);
  }

  { /* complex matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double complex	O[NROWS][NCOLS] = {
      { CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3) },
      { CPLX(2.1,-2.1), CPLX(2.2,-2.2), CPLX(2.3,-2.3) }
    };
    printf("\n--- complex matrix:\n\n");
    ccdoubles_cplx_matrix_print_display(stdout, "O", NROWS, NCOLS, &O[0][0]);
  }

/* ------------------------------------------------------------------ */

  printf("\n***** Brackets tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    double	O[NSLOTS]= { 1.1, 1.2, 1.3 };
    printf("\n--- real vector:\n\n");
    ccdoubles_real_vector_print_brackets(stdout, NSLOTS, O);
  }

  {
#undef NSLOTS
#define NSLOTS		3
    double complex	O[NSLOTS] = {
      CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3)
    };
    printf("\n--- complex vector:\n\n");
    ccdoubles_cplx_vector_print_brackets(stdout, NSLOTS, O);
  }

  { /* real matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double	O[NROWS][NCOLS] = {
      { 1.1, 1.2, 1.3 },
      { 2.1, 2.2, 2.3 }
    };
    printf("\n--- real matrix:\n\n");
    ccdoubles_real_matrix_print_brackets(stdout, NROWS, NCOLS, &O[0][0]);
  }

  { /* complex matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double complex	O[NROWS][NCOLS] = {
      { CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3) },
      { CPLX(2.1,-2.1), CPLX(2.2,-2.2), CPLX(2.3,-2.3) }
    };
    printf("\n--- complex matrix:\n\n");
    ccdoubles_cplx_matrix_print_brackets(stdout, NROWS, NCOLS, &O[0][0]);
  }

/* ------------------------------------------------------------------ */

  printf("\n***** Braces tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    double	O[NSLOTS]= { 1.1, 1.2, 1.3 };
    printf("\n--- real vector:\n\n");
    ccdoubles_real_vector_print_braces(stdout, NSLOTS, O);
  }

  {
#undef NSLOTS
#define NSLOTS		3
    double complex	O[NSLOTS] = {
      CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3)
    };
    printf("\n--- complex vector:\n\n");
    ccdoubles_cplx_vector_print_braces(stdout, NSLOTS, O);
  }

  { /* real matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double	O[NROWS][NCOLS] = {
      { 1.1, 1.2, 1.3 },
      { 2.1, 2.2, 2.3 }
    };
    printf("\n--- real matrix:\n\n");
    ccdoubles_real_matrix_print_braces(stdout, NROWS, NCOLS, &O[0][0]);
  }

  { /* complex matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double complex	O[NROWS][NCOLS] = {
      { CPLX(1.1,-1.1), CPLX(1.2,-1.2), CPLX(1.3,-1.3) },
      { CPLX(2.1,-2.1), CPLX(2.2,-2.2), CPLX(2.3,-2.3) }
    };
    printf("\n--- complex matrix:\n\n");
    ccdoubles_cplx_matrix_print_braces(stdout, NROWS, NCOLS, &O[0][0]);
  }

  exit(EXIT_SUCCESS);
}

/* end of file */
