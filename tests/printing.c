/*
  Part of: CCDoubles
  Contents: tests for printing routines
  Date: Mon Jun  9, 2014

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

#include <ccdoubles.h>

#define CPLX(REAL,IMAG)		CCDOUBLES_CPLX((REAL),(IMAG))


/** --------------------------------------------------------------------
 ** Main.
 ** ----------------------------------------------------------------- */

int
main (int argc, const char *const argv[])
{
  { /* real matrix */
#undef NROWS
#undef NCOLS
#define NROWS		2
#define NCOLS		3
    double	O[NROWS][NCOLS] = {
      { 1.1, 1.2, 1.3 },
      { 2.1, 2.2, 2.3 }
    };
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
    ccdoubles_cplx_matrix_print_display(stdout, "O", NROWS, NCOLS, &O[0][0]);
  }

  exit(EXIT_SUCCESS);
}

/* end of file */
