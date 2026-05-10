# The is the Makefile to (re-)build the dnaplot program and
# create the crossreference listing xdnaplot.

# Implemented by Martin Richards (c) 22 Jan 2020

# Change history

#30/10/2020
# First version

x:	xdnaplot xdnappm

xdnaplot:	dnaplot.b
	cintsys -c c b dnaplot xref >rawxref
	cintsys -c sortxref rawxref to xdnaplot
	rm rawxref

xdnappm:	dnappm.b
	cintsys -c c b dnappm xref >rawxref
	cintsys -c sortxref rawxref to xdnappm
	rm rawxref

