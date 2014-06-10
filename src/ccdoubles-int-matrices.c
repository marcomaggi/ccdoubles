/*
  Part of: CCDoubles
  Contents: routines for matrices of int numbers
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


/** --------------------------------------------------------------------
 ** Basic routines.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_matrix_clear (size_t nrows, size_t ncols,
			    int * restrict matrix)
{
  ccdoubles_int_vector_clear(nrows * ncols, matrix);
}
void
ccdoubles_int_matrix_set (size_t nrows, size_t ncols,
			  int * restrict matrix,
			  int value)
{
  ccdoubles_int_vector_set(nrows * ncols, matrix, value);
}
void
ccdoubles_int_matrix_copy (size_t nrows, size_t ncols,
			   int * restrict dst,
			   int * restrict src)
{
  ccdoubles_int_vector_copy(nrows * ncols, dst, src);
}


/** --------------------------------------------------------------------
 ** Printing.
 ** ----------------------------------------------------------------- */

void
ccdoubles_int_matrix_print_display (FILE * f, const char * name,
				    size_t nrows, size_t ncols,
				    int * operand)
{
  size_t	i, j;
  fprintf(f, "Row-major matrix %s (dimension %ld x %ld) (displayed in row-major order):\n",
	  name, nrows, ncols);
  for (i=0; i<nrows; ++i) {
    j = 0;
    fprintf(f, "| (%ld,%ld) %+d ", 1+i, 1+j, operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, "; (%ld,%ld) %+d ", 1+i, 1+j, operand[i * ncols + j]);
    }
    fprintf(f, "|\n");
  }
  fprintf(f, "\n");
}
void
ccdoubles_int_matrix_print_brackets (FILE * f, size_t nrows, size_t ncols,
				     int * operand)
{
  size_t	i, j;
  fprintf(f, "[[%+d", operand[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, " %+d", operand[j]);
  }
  fprintf(f, "]");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, "\n [%+d", operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, " %+d", operand[i * ncols + j]);
    }
    fprintf(f, "]");
  }
  fprintf(f, "]\n");
}
void
ccdoubles_int_matrix_print_braces (FILE * f, size_t nrows, size_t ncols,
				   int * operand)
{
  size_t	i, j;
  fprintf(f, "{{%+d", operand[0]);
  for (j=1; j<ncols; ++j) {
    fprintf(f, ", %+d", operand[j]);
  }
  fprintf(f, "}");
  for (i=1; i<nrows; ++i) {
    j = 0;
    fprintf(f, ",\n {%+d", operand[i * ncols + j]);
    for (++j; j<ncols; ++j) {
      fprintf(f, ", %+d", operand[i * ncols + j]);
    }
    fprintf(f, "}");
  }
  fprintf(f, "}\n");
}

/* end of file */