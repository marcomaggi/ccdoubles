/*
  Part of: CCDoubles
  Contents: version functions
  Date: Sat Jun  7, 2014

  Abstract



  Copyright (C) 2014, 2017, 2019, 2020 Marco Maggi <mrc.mgg@gmail.com>

  This program is free  software: you can redistribute it and/or  modify it under the
  terms of the  GNU Lesser General Public  License as published by  the Free Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
  WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License along with
  this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "ccdoubles-internals.h"


char const *
ccdoubles_version_string (void)
{
  return ccdoubles_VERSION_INTERFACE_STRING;
}
int
ccdoubles_version_interface_current (void)
{
  return ccdoubles_VERSION_INTERFACE_CURRENT;
}
int
ccdoubles_version_interface_revision (void)
{
  return ccdoubles_VERSION_INTERFACE_REVISION;
}
int
ccdoubles_version_interface_age (void)
{
  return ccdoubles_VERSION_INTERFACE_AGE;
}

/* end of file */
