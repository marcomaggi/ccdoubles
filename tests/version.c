/*
  Part of: CCDdoubles
  Contents: test for version functions
  Date: Sat Jun  7, 2014

  Abstract

	Test file for version functions.

  Copyright (C) 2014, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <ccdoubles.h>

int
main (int argc CCDOUBLES_UNUSED, const char *const argv[] CCDOUBLES_UNUSED)
{
  printf("version number string: %s\n", ccdoubles_version_string());
  printf("libtool version number: %d:%d:%d\n",
	 ccdoubles_version_interface_current(),
	 ccdoubles_version_interface_revision(),
	 ccdoubles_version_interface_age());
  exit(EXIT_SUCCESS);
}

/* end of file */
