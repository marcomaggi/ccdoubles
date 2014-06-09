## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL configure.sh
dist_doc_DATA		= README COPYING
AM_CFLAGS		= -Wall

## --------------------------------------------------------------------

EXTRA_DIST		+= meta/scripts/ccdoubles.pc.in
pkgconfigdir		= $(libdir)/pkgconfig
nodist_pkgconfig_DATA	= meta/scripts/ccdoubles.pc

## --------------------------------------------------------------------

AM_MAKEINFOFLAGS	= --no-split

info_TEXINFOS		= doc/ccdoubles.texi
doc_ccdoubles_TEXINFOS	= \
	doc/macros.texi		\
	doc/gpl-3.0.texi	\
	doc/fdl-1.3.texi

#page
#### libraries

ccdoubles_CURRENT	= @ccdoubles_VERSION_INTERFACE_CURRENT@
ccdoubles_REVISION	= @ccdoubles_VERSION_INTERFACE_REVISION@
ccdoubles_AGE		= @ccdoubles_VERSION_INTERFACE_AGE@

include_HEADERS		= src/ccdoubles.h

lib_LTLIBRARIES		= libccdoubles.la
libccdoubles_la_LDFLAGS= -version-info $(ccdoubles_CURRENT):$(ccdoubles_REVISION):$(ccdoubles_AGE)
libccdoubles_la_SOURCES= \
	src/ccdoubles.h			\
	src/ccdoubles-internals.h	\
	src/ccdoubles-version.c		\
	src/ccdoubles-cplx-scalars.c	\
	src/ccdoubles-real-vectors.c	\
	src/ccdoubles-cplx-vectors.c	\
	src/ccdoubles-real-matrices.c	\
	src/ccdoubles-cplx-matrices.c

#page
#### tests

check_PROGRAMS	= \
	tests/version		\
	tests/vectors		\
	tests/matrices		\
	tests/printing

TESTS				= $(check_PROGRAMS)

ccdoubles_tests_cppflags	= -I$(srcdir)/src
ccdoubles_tests_ldadd		= libccdoubles.la

tests_version_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_version_LDADD		= $(ccdoubles_tests_ldadd)

tests_vectors_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_vectors_LDADD		= $(ccdoubles_tests_ldadd)

tests_matrices_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_matrices_LDADD		= $(ccdoubles_tests_ldadd)

tests_printing_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_printing_LDADD		= $(ccdoubles_tests_ldadd)

installcheck-local: $(TESTS)
	@for f in $(TESTS); do $(builddir)/$$f; done

### end of file
