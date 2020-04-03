# @configure_input@
#

AC_PREREQ([2.69])
MMUX_PKG_VERSIONS([0],[3],[0],[devel.1])
AC_INIT([CCDoubles],[MMUX_PACKAGE_VERSION],
  [marco.maggi-ipsu@poste.it],
  [ccdoubles],[http://github.com/marcomaggi/ccdoubles/])
MMUX_INIT
MMUX_LIBTOOL_LIBRARY_VERSIONS([ccdoubles],1,0,0)
AC_REVISION([0.2])
AC_COPYRIGHT([Copyright (c) 2014, 2015, 2017, 2019, 2020 Marco Maggi <marco.maggi-ipsu@poste.it>

This program is free software: you  can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free  Software Foundation, either version  3 of the License,  or (at
your option) any later version.

This program  is distributed  in the  hope that it  will be  useful, but
WITHOUT   ANY   WARRANTY;  without   even   the   implied  warranty   of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
])
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

LT_PREREQ([2.4])
LT_INIT


#### basic system inspection

MMUX_CHECK_TARGET_OS
AX_IS_RELEASE([git-directory])

AC_LANG([C])
MMUX_LANG_C11

AM_PROG_AS
AM_PROG_CC_C_O
AC_HEADER_STDC
AC_CHECK_HEADERS([assert.h stdint.h strings.h errno.h stddef.h])
AC_CHECK_SIZEOF(void *)
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

CCLIB_CONFIG_SETUP

AC_CACHE_SAVE


#### checks for library functions

AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([exp10 clog10])

AC_CACHE_SAVE


#### external libraries

PKG_PREREQ([0.29])
PKG_INSTALLDIR

AC_CHECK_LIB([m],[cabs])


#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
   [meta/scripts/ccdoubles.pc])
MMUX_OUTPUT
AC_OUTPUT

### end of file
