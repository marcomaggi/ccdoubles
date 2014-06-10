/*
  Part of: CCDoubles
  Contents: functions for complex scalar numbers
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

#include "ccdoubles-internals.h"


/** --------------------------------------------------------------------
 ** Arithmetic operations.
 ** ----------------------------------------------------------------- */

double complex
ccdoubles_cplx_mul (double complex O1, double complex O2)
{
  return CPLX((Re(O1) * Re(O2) - Im(O1) * Im(O2)),
	      (Re(O1) * Im(O2) + Im(O1) * Re(O2)));
}
double complex
ccdoubles_cplx_div (double complex O1, double complex O2)
{
#undef A
#undef B
#undef C
#undef D
#define A	Re(O1)
#define B	Im(O1)
#define C	Re(O2)
#define D	Im(O2)
  const double	denom = C*C+D*D;
  return CPLX((A*C+B*D)/denom, (B*C-A*D)/denom);
}
double complex
ccdoubles_cplx_neg (double complex O)
{
  return CPLX(-Re(O), -Im(O));
}

/* end of file */
