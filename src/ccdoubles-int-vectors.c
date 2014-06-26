/*
  Part of: CCDoubles
  Contents: routines for vectors of int
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

#include "ccdoubles-internals.h"
#include <string.h>


/** --------------------------------------------------------------------
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_vector_clear (unsigned nslots, ccdoubles_int_result_t vector)
{
  memset(vector, 0, sizeof(int) * nslots);
}
void
ccdoubles_int_vector_set (unsigned nslots, ccdoubles_int_result_t vector, int value)
{
  for (unsigned i=0; i<nslots; ++i) {
    vector[i] = value;
  }
}
void
ccdoubles_int_vector_copy (unsigned nslots, ccdoubles_int_result_t dst, ccdoubles_int_operand_t src)
{
  if (1) {
    memcpy(dst, src, sizeof(int) * nslots);
  } else {
    for (unsigned i=0; i<nslots; ++i) {
      dst[i] = src[i];
    }
  }
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_vector_print_display (FILE * f, const char * name,
				    unsigned nslots,
				    ccdoubles_int_operand_t O)
{
  fprintf(f, "Vector %s (dimension %u):\n", name, nslots);
  fprintf(f, "| (1) %+d ", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, "; (%u) %+d ", 1+i, O[i]);
  }
  fprintf(f, "|\n");
}
void
ccdoubles_int_vector_print_brackets (FILE * f, unsigned nslots, ccdoubles_int_operand_t O)
{
  fprintf(f, "[%+d", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, " %+d", O[i]);
  }
  fprintf(f, "]\n");
}
void
ccdoubles_int_vector_print_braces (FILE * f, unsigned nslots, ccdoubles_int_operand_t O)
{
  fprintf(f, "{%+d", O[0]);
  for (unsigned i=1; i<nslots; ++i) {
    fprintf(f, ", %+d", O[i]);
  }
  fprintf(f, "}\n");
}

/* end of file */
