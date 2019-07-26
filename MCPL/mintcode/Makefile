# Makefile to build the MCPL Mintcode System
# (c) Martin Richards October 2006

# Last modified: 12 October 2006

# Select the appropriate definition of CC, LIBS, SYSM and ENDER
# by uncommenting out the appropriate lines below.

SYSM = sysm
SYSC = sysc
OBJ = obj
MIN = min
SYSMIN = min/sysmin
MHDRS = g

LIBS =

#CC = gcc -O9 -DforMAC

#CC = gcc -O9 -DforMIPS
#ASM = sysasm/MIPS

CC = gcc -O9 -DforLINUX
ASM = sysasm/LINUX

#CC = gcc -O9 -DforSUN4
#ASM = sysasm/SUN4

#CC = gcc -O9 -DforSPARC
#ASM = sysasm/SPARC

#CC = gcc -O9 -DforALPHA
#ASM = sysasm/ALPHA
#LIBS = -lbsd

#CC = gcc -O9 -DforMSDOS
#ASM = sysasm/MSDOS

#CC = gcc -O9 -DforCYGWIN32
#ASM = sysasm/CYGWIN32

#CC = gcc -O9 -DforOS2
#ASM = sysasm/OS2

MINTASM = $(OBJ)/mnrastlib.o
#MINTASM = mintasm.o mnrastlib.o

MRASTER = $(OBJ)/mrasterp.o $(OBJ)/mrastlib.o
HDRS = $(MHDRS)/mcpl.h $(MHDRS)/io.h $(MHDRS)/mcli.h


run:	bin/mintsys $(SYSMIN)/MBOOT $(SYSMIN)/MLIB $(SYSMIN)/MCLI \
	$(MIN)/echo $(MIN)/logout
	@echo
	bin/mintsys

ras:	bin/mrastsys $(SYSMIN)/MBOOT $(SYSMIN)/MLIB $(SYSMIN)/MCLI \
	$(MIN)/echo $(MIN)/logout
	@echo
	bin/mrastsys

sane:
	stty sane

$(OBJ)/mintsys.o:	$(SYSC)/mintsys.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/mintsys.o -c $(SYSC)/mintsys.c

$(OBJ)/minterp.o:	$(SYSC)//minterp.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/minterp.o -c $(SYSC)/minterp.c

$(OBJ)/mrasterp.o:	$(SYSC)/mrasterp.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/nrasterp.o -c $(SYSC)/mrasterp.c

$(OBJ)/mnrastlib.o:	$(SYSC)/mnrastlib.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/mnrastlib.o -c $(SYSC)/mnrastlib.c

$(OBJ)/mrastlib.o:	$(SYSC)/mrastlib.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/mrastlib.o -c $(SYSC)/mrastlib.c

$(OBJ)/mkblib.o:	$(SYSC)/mkblib.c $(SYSC)/minterp.h
	$(CC) -o $(OBJ)/mkblib.o -c $(SYSC)/mkblib.c

bin/mintsys:	$(OBJ)/mintsys.o $(OBJ)/minterp.o $(OBJ)/mkblib.o $(MINTASM)
	$(CC) -o bin/mintsys $(OBJ)/mintsys.o $(OBJ)/minterp.o \
	$(OBJ)/mkblib.o $(MINTASM) $(LIBS)

bin/mrastsys:	$(OBJ)/mintsys.o $(OBJ)/minterp.o $(OBJ)/mkblib.o $(MRASTER)
	$(CC) -o bin/mrastsys $(OBJ)/mintsys.o $(OBJ)/minterp.o \
	$(OBJ)/mkblib.o $(RASTER) $(LIBS)

$(SYSMIN)/MBOOT:	$(SYSM)/MBOOT.m Makefile $(HDRS)
	cintsys -c c ms MBOOT

$(SYSMIN)/MLIB:	$(SYSM)/MLIB.m Makefile $(HDRS)
	cintsys -c c ms MLIB

$(SYSMIN)/MCLI:	$(SYSM)/MCLI.m Makefile $(HDRS) 
	cintsys -c c ms MCLI

$(MIN)/logout:	com/logout.m Makefile $(HDRS)
	cintsys -c c mc logout

$(MIN)/echo:	com/echo.m Makefile $(HDRS)
	cintsys -c c mc echo

clean:
	rm -f *~ doc/*~ com/*~ $(SYSM)/*~
	rm -f *.min OCODE MCODE
	rm -f bin/minterp bin/mrasterp minterp.exe mrasterp.exe *.o junk*
	rm -f $(MIN)/echo $(MIN)/logout
	rm -f $(SYSMIN)/MBOOT $(SYSMIN)/MLIB $(SYSMIN)/MCLI
	rm -f $(OBJ)/*
