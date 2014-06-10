## Makefile --

.PHONY: vall vtest pall rall sall

vall:
	vicare process.sps >ccdoubles.sls

pall:
	petite --program process.sps >ccdoubles.sls

rall:
	plt-r6rs process.sps >ccdoubles.sls

sall:
	sash process.sps >ccdoubles.sls

## --------------------------------------------------------------------

vtest:
	vicare -L $(PWD) $(VFLAGS) test-vicare.sps

### end of file
