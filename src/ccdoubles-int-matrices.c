/*
  Part of: CCDoubles
  Contents: routines for matrices of int numbers
  Date: Tue Jun 10, 2014

  Abstract



  Copyright (C) 2014, 2017, 2019 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it  under the  terms  of  the GNU  Lesser  General  Public License  as
  published by  the Free  Software Foundation, either  version 3  of the
  License, or (at your option) any later version.

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
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_matrix_clear (unsigned nrows, unsigned ncols,
			    ccdoubles_int_result_t matrix)
{
  ccdoubles_int_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_int_matrix_set (unsigned nrows, unsigned ncols,
			  ccdoubles_int_result_t matrix,
			  int value)
{
  ccdoubles_int_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_int_matrix_copy (unsigned nrows, unsigned ncols,
			   ccdoubles_int_result_t dst,
			   ccdoubles_int_operand_t src)
{
  ccdoubles_int_vector_copy(nrows * ncols, dst, src);
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_matrix_print_display (FILE * f, const char * name,
				    unsigned nrows, unsigned ncols,
				    ccdoubles_int_operand_t O)
{
  unsigned	i;
  fprintf(f, "Row-major matrix %s (dimension %u x %u) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    unsigned j = 0;
    fprintf(f, "| (%u,%u) %+d ", 1+i, 1+j, O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%u,%u) %+d ", 1+i, 1+j, O[i * ncols + j]);
    }
    fprintf(f, "|\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_int_matrix_print_brackets (FILE * f, unsigned nrows, unsigned ncols,
				     ccdoubles_int_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "[[%+d", O[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+d", O[j]);
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+d", O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+d", O[i * ncols + j]);
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_int_matrix_print_braces (FILE * f, unsigned nrows, unsigned ncols,
				   ccdoubles_int_operand_t O)
{
  unsigned	i, j;
  fprintf(f, "{{%+d", O[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+d", O[j]);
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+d", O[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+d", O[i * ncols + j]);
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */
