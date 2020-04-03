## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL meta/slackware/slack-desc.in
dist_doc_DATA		= README COPYING
AM_CPPFLAGS		= -I$(srcdir)/src
AM_CFLAGS		= $(MMUX_CFLAGS)
CLEANFILES		=

## --------------------------------------------------------------------

nodist_pkgconfig_DATA	= meta/scripts/ccdoubles.pc

## --------------------------------------------------------------------

EXTRA_DIST		+= \
	scheme/Makefile				\
	scheme/process.sps			\
	scheme/table.scm			\
	scheme/ccdoubles.sls			\
	scheme/ccdoubles/compat.vicare.sls	\
	scheme/test-vicare.sps


#### documentation

EXTRA_DIST		+= doc/cclibraries.style.css
CLEANFILES		+= doc/cclibraries.css

AM_MAKEINFOFLAGS	= --no-split
AM_MAKEINFOHTMLFLAGS	= --split=node -c WORDS_IN_PAGE=0 --css-ref=cclibraries.css \
	-c PRE_BODY_CLOSE="<p>This document describes version <tt>$(PACKAGE_VERSION)</tt> of <em>$(PACKAGE_NAME)</em>.</p>"

info_TEXINFOS		= doc/ccdoubles.texi
doc_ccdoubles_TEXINFOS	= \
	doc/macros.texi			\
	doc/overview-linking.texi	\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/ccdoubles.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/ccdoubles.html/
	@: > doc/ccdoubles.html/$(am__dirstamp)

doc/ccdoubles.html/cclibraries.css: doc/cclibraries.style.css doc/ccdoubles.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/cclibraries.style.css" "$(builddir)/doc/ccdoubles.html/cclibraries.css"

## --------------------------------------------------------------------

html-local: doc/ccdoubles.html/cclibraries.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/ccdoubles.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/ccdoubles.html/cclibraries.css "$(DESTDIR)$(htmldir)/ccdoubles.html/"


#### libraries

ccdoubles_CURRENT		= @ccdoubles_VERSION_INTERFACE_CURRENT@
ccdoubles_REVISION		= @ccdoubles_VERSION_INTERFACE_REVISION@
ccdoubles_AGE			= @ccdoubles_VERSION_INTERFACE_AGE@
ccdoubles_LINKER_VERSION	= -version-info $(ccdoubles_CURRENT):$(ccdoubles_REVISION):$(ccdoubles_AGE)

include_HEADERS		= src/ccdoubles.h

lib_LTLIBRARIES		= libccdoubles.la
libccdoubles_la_LDFLAGS	= $(ccdoubles_LINKER_VERSION) $(AM_LDFLAGS)
libccdoubles_la_SOURCES	= \
	src/ccdoubles.h			\
	src/ccdoubles-internals.h	\
	src/ccdoubles-version.c		\
	src/ccdoubles-cplx-scalars.c	\
	src/ccdoubles-real-vectors.c	\
	src/ccdoubles-cplx-vectors.c	\
	src/ccdoubles-real-matrices.c	\
	src/ccdoubles-cplx-matrices.c	\
	src/ccdoubles-int-vectors.c	\
	src/ccdoubles-int-matrices.c


#### tests

check_PROGRAMS	= \
	tests/version		\
	tests/vectors		\
	tests/matrices		\
	tests/integers		\
	tests/printing

TESTS				= $(check_PROGRAMS)

ccdoubles_tests_cppflags	= $(AM_CPPFLAGS) -I$(srcdir)/src
ccdoubles_tests_cflags		= $(AM_CFLAGS)
ccdoubles_tests_ldadd		= $(AM_LDFLAGS) libccdoubles.la

## --------------------------------------------------------------------

tests_version_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_version_CFLAGS		= $(ccdoubles_tests_cflags)
tests_version_LDADD		= $(ccdoubles_tests_ldadd)

tests_vectors_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_vectors_CFLAGS		= $(ccdoubles_tests_cflags)
tests_vectors_LDADD		= $(ccdoubles_tests_ldadd)

tests_matrices_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_matrices_CFLAGS		= $(ccdoubles_tests_cflags)
tests_matrices_LDADD		= $(ccdoubles_tests_ldadd)

tests_integers_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_integers_CFLAGS		= $(ccdoubles_tests_cflags)
tests_integers_LDADD		= $(ccdoubles_tests_ldadd)

tests_printing_CPPFLAGS		= $(ccdoubles_tests_cppflags)
tests_printing_CFLAGS		= $(ccdoubles_tests_cflags)
tests_printing_LDADD		= $(ccdoubles_tests_ldadd)

## --------------------------------------------------------------------

installcheck-local: $(TESTS)
	@for f in $(TESTS); do $(builddir)/$$f; done


#### common rules

include $(top_srcdir)/meta/automake/mmux-makefile-rules

### end of file
