/*
  Part of: CCDoubles
  Contents: tests for integers functions
  Date: Tue Jun 10, 2014

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

#define CCDOUBLES_ENABLE_SHORT_MACROS		1
#include <ccdoubles.h>

static void test_int_vectors (void);
static void test_int_matrices (void);
static void test_printing (void);

#define	NSLOTS		1
#define	NROWS		1
#define	NCOLS		1


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc, const char *const argv[])
{
  test_int_vectors();
  test_int_matrices();
  test_printing();
  exit(EXIT_SUCCESS);
}


/** --------------------------------------------------------------------
 ** Test integer vectors.
 ** ----------------------------------------------------------------- */

void
test_int_vectors (void)
{
  {
    int		V[NSLOTS];
    ccdoubles_int_vector_clear(NSLOTS, V);
    assert(0.0 == V[0]);
  }

  {
    int		V[NSLOTS];
    ccdoubles_int_vector_set(NSLOTS, V, 1);
    assert(1 == V[0]);
  }

  {
    int		R[NSLOTS];
    int		V[NSLOTS] = { 1 };
    ccdoubles_int_vector_copy(NSLOTS, R, V);
    assert(1 == R[0]);
  }
}


/** --------------------------------------------------------------------
 ** Test integer matrices.
 ** ----------------------------------------------------------------- */

void
test_int_matrices (void)
{
  {
    int		M[NROWS][NCOLS];
    ccdoubles_int_matrix_clear(NROWS, NCOLS, MREF(M));
    assert(0 == M[0][0]);
  }

  {
    int		M[NROWS][NCOLS];
    ccdoubles_int_matrix_set(NROWS, NCOLS, MREF(M), 1);
    assert(1 == M[0][0]);
  }

  {
    int		R[NROWS][NCOLS];
    int		M[NROWS][NCOLS] = { { 1 } };
    ccdoubles_int_matrix_copy(NROWS, NCOLS, MREF(R), MREF(M));
    assert(1 == R[0][0]);
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
test_printing (void)
{
  printf("\n***** Display tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    int		O[NSLOTS]= { 11, 22, 33 };
    printf("\n--- int vector:\n\n");
    ccdoubles_int_vector_print_display(stdout, "O", NSLOTS, O);
  }

  {
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    int		O[NROWS][NCOLS] = {
      { 11, 22, 33 },
      { 44, 55, 66 }
    };
    printf("\n--- int matrix:\n\n");
    ccdoubles_int_matrix_print_display(stdout, "O", NROWS, NCOLS, &O[0][0]);
  }

/* ------------------------------------------------------------------ */

  printf("\n***** Brackets tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    int		O[NSLOTS]= { 11, 22, 33 };
    printf("\n--- int vector:\n\n");
    ccdoubles_int_vector_print_brackets(stdout, NSLOTS, O);
  }

  {
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    int		O[NROWS][NCOLS] = {
      { 11, 22, 33 },
      { 44, 55, 66 }
    };
    printf("\n--- int matrix:\n\n");
    ccdoubles_int_matrix_print_brackets(stdout, NROWS, NCOLS, &O[0][0]);
  }

/* ------------------------------------------------------------------ */

  printf("\n***** Braces tests:\n");

  {
#undef NSLOTS
#define NSLOTS		3
    int		O[NSLOTS]= { 11, 22, 33 };
    printf("\n--- int vector:\n\n");
    ccdoubles_int_vector_print_braces(stdout, NSLOTS, O);
  }

  {
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    int		O[NROWS][NCOLS] = {
      { 11, 22, 33 },
      { 44, 55, 66 }
    };
    printf("\n--- int matrix:\n\n");
    ccdoubles_int_matrix_print_braces(stdout, NROWS, NCOLS, &O[0][0]);
  }

}

/* end of file */
