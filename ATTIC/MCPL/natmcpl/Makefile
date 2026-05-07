# This makefile should run in the directory natmcpl
# It will build the MCPL to native code system, currently
# available for Linux and DEC Alpha machines.

# Last modified: by MR 16 October 2006

# sys  is the location of m/c independent C code and compiled .s files

AS = as

# Select the appropriate definition of CC, LIBS and SYSM
# SYSM gives the location of machine specific code (eg syslib.s)

#CC = gcc -O9 -DforMAC
#CVMIAL = cvmialmac

#CC = gcc -O9 -DforMIPS
#SYSM = sysasm/MIPS
#CVMIAL = cvmialmips

CC = gcc -O9 -DforLINUX
SYSM = sysasm/LINUX
CVMIAL = cvmial386

#CC = gcc -O9 -DforSUN4
#SYSM = sysasm/SUN4
#CVMIAL = cvmialsun4

#CC = gcc -O9 -DforSPARC
#SYSM = sysasm/SPARC
#CVMIAL = cvmialsparc

#CC = gcc -O9 -DforALPHA -lbsd
#SYSM = sysasm/ALPHA
#CVMIAL = cvmialalpha


#CC = gcc -O9 -DforMSDOS
#SYSM = sysasm/MSDOS
#CVMIAL = cvmialmsdos

#CC = gcc -O9 -DforOS2
#SYSM = sysasm/OS2
#CVMIAL = cvmialos2

CINTCODE = ../../BCPL/cintcode

run:	prog
	stty sane
	echo "" | prog

obj/prog.o:	sysasm/prog.s
	$(AS) -o obj/prog.o sysasm/prog.s

obj/mlib.o:	sysasm/mlib.s
	$(AS) -o obj/mlib.o sysasm/mlib.s

obj/initprog.o:	sysc/initprog.c sysc/mcplsys.h
	$(CC) -o obj/initprog.o -c sysc/initprog.c

obj/clib.o:	sysc/clib.c sysc/mcplsys.h
	$(CC) -o obj/clib.o -c sysc/clib.c

obj/kblib.o:	sysc/kblib.c sysc/mcplsys.h
	$(CC) -o obj/kblib.o -c sysc/kblib.c

obj/syslib.o:	$(SYSM)/syslib.s
	$(AS) -o obj/syslib.o $(SYSM)/syslib.s

prog:	obj/kblib.o obj/mlib.o obj/prog.o obj/initprog.o obj/syslib.o obj/clib.o
	$(CC) -o prog obj/kblib.o obj/mlib.o obj/prog.o obj/initprog.o \
	obj/syslib.o obj/clib.o

sysasm/mlib.s:	sysm/mlib.m
	cintsys -c c nms mlib

sysasm/prog.s:	prog.m Makefile
	cintsys -c c nmp prog
	echo prog.m compiled to sysasm/prog.s

clean:
	rm -f core
	rm -f *~ sysasm/*~ doc/*~
	rm -f sysasm/mlib.s
	rm -f sysasm/prog.s






