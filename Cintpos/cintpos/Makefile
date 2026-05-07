# This is the Makefile to build the BCPL Cintpos System

# Implemented by Martin Richards (c) 20 March 2021

# Change history

# 31/03/2021

# Cintpos and Cintsys have many files in common with the master
# copies in directory BCPL/. These are copied automatically to
# the Cintpos directory when needed by {\tt make}. These shared
# files can be copied by the command {\tt make filesfromBCPL}.

# 25/08/2019
# Unlike the Makefile for cintsys and cintsys64, this makefile
# does not need the directories enderlit and enderbig to hold
# the compiled code of boot, blib etc, since these can be
# recompiled if necessary using cintsys or cintsys64. There is
# no need to invoke bcpl with the oender option and so bcb
# bcb64, bcl, bcl64, bsb, bsb64 bsl and bsl64 are not needed.
# The four versions of syslib are now held in sysb as files
# syslib32lit, syslib64lit, syslib32big and syslib64big. The
# appropriate version is copied into cin/syscin or cin64/sysin
# when necessary. Which version to copy is determined by the
# setting of ENDER and TARGET64.

# 24/08/2019
# Made changes to allow 32 and 64 bit cinpos to be be compiled
# and run on 32 and 64 bit machines under Linux.

# 29/12/18
# Made this Makefile more compatible with the one for cintsys
# Made Makefile automatically copy mkint-h.c from BCPL.

# 14/04/2014
# Added the EXT user extension library.
# See g/ext.h, ext.b, sysc/extfn.c

# 23/01/2014
# A minor modification of this Makefile called MakefileGL can
# create a BCPL system that interfaces with the OpenGL graphics
# library. On some systems it uses SDL and the full OpenGL library
# but on other systems it uses OpenGL ES and EGL.
# The BCPL graphics library g/GL.b with header g/GL.h hides the
# differences between the two versions of OpenGL, so the same BCPL
# code should work without change with either version of OpenGL.

# The Makefile for the Raspberry Pi is called MakefileRaspiGL. It uses
# the OpenGL ES and EGL libraries.

# Added NOFLOAT for systems (eg GP2X) that do not support floating
# point. This has since been removed.

# Added sys(Sys_sound, fno, a1, a2, a3) to provide alsa sound
# facilities when running under Linux.
# ----- Under development ------------

# Moved the executables cintpos and rastpos into $POSROOT/bin
# from $POSROOT. This required a change in the setting of
# the environment variable PATH.

# Renamed most file names to lowercase letters to simplify running on
# Windows machines.

# Before running this makefile check that the environment variables
# POSLROOT, POSHDRS, POSPATH, POSSCRIPTS and PATH are correctly defined
# as described in docs/README

# Just modified to allow cross compilation for the GP2X handheld Linux machine.

# Location of Assembly language source code (architecture dependent)
# for example: SYSM = sysasm/LINUX

# My public HTML directory when not mountable on this machine.
# Remember to call ssh-add before calling make sshpub
SSHPUB = ely.cl.cam.ac.uk:public_html

# The BCPL cintcode directory
BCPL = $(HOME)/distribution/BCPL/cintcode

# The following for armgcc under cygwin
#GP2XCC = /cygdrive/c/devkitGP2X/bin/arm-linux-gcc -O3 -static -DforGP2X

# The following for arm-linux-gcc for GP2X under Linux
GP2XCC = $(HOME)/devkitPro/devkitGP2X/bin/arm-linux-gcc -O3 -static -DforGP2X

# The following for arm gnueabi-gcc under Linux
#GP2XCC = arm-none-linux-gnueabi-gcc -O3 -static -DforLinux

# Location of the SD card under Cygwin
#SD = /cygdrive/g

# Location of the SD card under Linux on Solestreet
SD = /media/535A-B0C2

# There are typically three versions of the interpreter

# Two are derived from sysc/cinterp.c. A fast version is generated
# by #defining FASTyes to produce obj/fasterp.o. Without FASTyes defined
# a version with all debugging aids is generated in obj/cinterp.o
# The fast version is implemented in assembly language for some
# architectures eg sysasm/linux/cintasm.s gives obj/cintasm.o

# On a 1GHz Mobile Pentium III the times for running the benchmark
# bench100 in bcplprogs/tests/bench100.b are:

# cinterp                   18.76 secs
# fasterp                   14.93 secs
# cintasm                   12.37 secs

# So hand written assembly language is hardly worth the effort,
# so is no longer maintained.

# Now define/redefine the variables for particular architectures.

#####################################################################
# add FASTERP = obj/cintasm.o for architectures having a hand written
# assembly language fast interpreter. This will override the
# default setting of: FASTERP = obj/fasterp.o
#####################################################################

FASTERP = obj/fasterp.o
FASTERP64 = obj64/fasterp.o

# Assume a little ender machine unless otherwise specified
ENDER = litender

#CC = gcc -O3 -DforMAC
#ENDER = bigender

#CC = gcc -O3 -DforMIPS

#CC = gcc -O3 -DforSGI
#ENDER = bigender

# For ARM Linux without any graphics libraries eg Raspberry Pi
#CC = gcc -O2 -DforARM -DEXTavail

# For Raspberry Pi Linux with the SDL library
#CC = gcc -O3 -DforRaspiSDL -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs`

# For Raspberry Pi Linux with EGL and OpenGL ES
#CC = gcc -O3 -DforRaspiGL -DEXTavail
#CFLAGS+=-DSTANDALONE -D__STDC_CONSTANT_MACROS
#CFLAGS+=-D__STDC_LIMIT_MACROS -DTARGET_POSIX -D_LINUX -fPIC
#CFLAGS+=-DPIC -D_REENTRANT -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64
#CFLAGS+=-U_FORTIFY_SOURCE -Wall -g -DHAVE_LIBOPENMAX=2
#CFLAGS+=-DOMX -DOMX_SKIP64BIT -ftree-vectorize -pipe -DUSE_EXTERNAL_OMX
#CFLAGS+=-DHAVE_LIBBCM_HOST -DUSE_EXTERNAL_LIBBCM_HOST -DUSE_VCHIQ_ARM -Wno-psabi
#CFLAGS+=-I/opt/vc/include/ -I/opt/vc/include/interface/vcos/pthreads
#CFLAGS+=-I/opt/vc/include/interface/vmcs_host/linux -I./ -I../libs/ilclient -I../libs/vgfont
#LDFLAGS+=-L/opt/vc/lib/ -lGLESv2 -lEGL -lopenmaxil -lbcm_host
#LDFLAGS+=-lvcos -lvchiq_arm -lpthread -lrt -L../libs/ilclient -L../libs/vgfont

# For Pentium Linux without any graphics libraries or joystick
SYSM = sysasm/linux
CC = gcc -O3 -DforLinux -DSOUND -DEXTavail -DCINTPOSyes
#CC = gcc -O3 -DforLinux64

# For Pentium Linux without any graphics libraries but with a joystick
#CC = gcc -O3 -DforLinux -DSOUND -DCALLC -DEXTavail -DJSAvail

# For Linux with the SDL features
#CC = gcc -O3 -DforLinuxSDL -DSOUND -DCALLC -DEXTavail -DJSAvail
#CC = gcc -O3 -DforLinuxSDL -DSOUND -DCALLC -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs`

# For Linux with GL features called from SDL
#CC = gcc -O3 -DforLinuxGL -DSOUND -DCALLC -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs` -lGL

# For 32 bit Pentium Linux compiling a 64-bit Cintcode interpreter.
#CC = gcc -O3 -DforLinux64

# The following works under Cygwin on Pentiums and AMD 64 Athlon machines.
# make gp2x will cross compile the system for the GP2x handheld machine,
# and make gp2xinstall will install the BCPL Cintcode system on the SD memory
# card of the GP2X assuming it is on drive G:.
#CC = gcc -O3 -DforCYGWIN32

# Version for the Power PC running Mac OS 10.3.9 -- added 26/7/05
#CC = gcc -O3 -DforMacOSPPC
#ENDER = bigender

# Version for the Macmini running Intel Mac OS-X 32 bit version -- added 12/01/11
#CC = gcc -O3 -m32 -DforMacOSX
#ENDER = litender

# The following is under development for AMD 64 Athlon machines
#CC = gcc -O3 -DforLinuxAMD64

# Version for the PowerMac G4 running Debian Linux was
# kindly provided by Chris Emerson
#CC = gcc -O3 -DforLinuxPPC
#ENDER = bigender

#CC = gcc -O3 -DforSUN4
#ENDER = bigender

#CC = gcc -O3 -DforSPARC
#ENDER = bigender

#CC = gcc -O3 -DforALPHA

#CC = gcc -O3 -DforMSDOS

#CC = gcc -O3 -DforOS2


run:	bcplready
	./bin/cintpos

run64:	bcplready64
	bin/cintpos64

sys:	bin/cintpos $(ENDER)
	@echo "bin/cintpos is up to date"

sys64:	bin/cintpos64 $(ENDER)64
	@echo "bin/cintsys64 is up to date"

bcplready:	bin/cintpos $(ENDER) allcompiled
	date >bcplready

bcplready64:	bin/cintpos64  $(ENDER)64 allcompiled64
	date >bcplready64

allcompiled:	bin/cintpos
	cintsys -c c compall
	date >allcompiled

allcompiled64:	bin/cintpos64
	cintsys64 -c c compall64
	date >allcompiled64

filesfromBCPL:	\
	g/libhdr.h \
	g/bcplfecg.h \
	com/bcpl.b \
	com/bcplfe.b \
	com/bcplcgcin.b \
	sysc/soundfn.c

g/libhdr.h:	$(BCPL)/cintcode/g/libhdr.h
	cp $(BCPL)/cintcode/g/libhdr.h g/libhdr.h

com/bcpl.b:	$(BCPL)/com/bcpl.b
	cp $(BCPL)/com/bcpl.b com/bcpl.b

com/bcplfe.b:	$(BCPL)/com/bcplfe.b
	cp $(BCPL)/com/bcplfe.b com/bcplfe.b

com/bcplcgcin.b:	$(BCPL)/com/bcplcgcin.b
	cp $(BCPL)/com/bcplcgcin.b com/bcplcgcin.b

g/bcplfecg.h:	$(BCPL)/g/bcplfecg.h
	cp $(BCPL)/g/bcplfecg.h g/bcplfecg.h

sysc/soundfn.c:	$(BCPL)/cintcode/sysc/soundfn.c
	cp $(BCPL)/cintcode/sysc/soundfn.c sysc/soundfn.c

all:	$(ENDER) \
	bin/mconn \
	bin/cintpos cin/syscin/boot cin/syscin/blib cin/syscin/klib \
	cin/syscin/idle cin/syscin/cli cin/syscin/cli_init cin/syscin/debug \
	cin/syscin/cohand cin/syscin/fh0 cin/syscin/mbxhand cin/syscin/tcphand\
	cin/syscin/dlib \
	cin/bcpl cin/c cin/echo
	@echo
#	cintsys -c c compall
#	./bin/cintpos

all64:	$(ENDER)64 \
	bin/mconn \
	bin/cintpos64 cin64/syscin/boot cin64/syscin/blib cin64/syscin/klib \
	cin64/syscin/idle cin64/syscin/cli cin64/syscin/cli_init \
	cin64/syscin/debug cin64/syscin/cohand cin64/syscin/fh0 \
	cin64/syscin/mbxhand cin64/syscin/tcphand cin64/syscin/dlib \
	cin64/bcpl cin64/c cin64/echo
	@echo
	cintsys64 -c c compall64
	./bin/cintpos64

sane:	bcplready
	stty sane
	./bin/cintpos

xref:	rawxref
	cintsys -c sortxref rawxref to xrefdata
	rm rawxref

rawxref:
	cintsys -c c compall xref >rawxref

bigender:
	@echo "Ensure cintsys exist"
	cp sysb/syslib32big cin/syscin/syslib
	rm -f litender
	date >bigender

bigender64:
	@echo "Ensure cintsys64 exist"
	cp sysb/syslib64big cin64/syscin/syslib
	rm -f litender
	date >bigender64

litender:
	@echo "Ensure cintsys exist"
	cp sysb/syslib32lit cin/syscin/syslib
	rm -f bigender
	date >litender

litender64:
	@echo "Ensure cintsys64 exist"
	cp sysb/syslib64lit cin64/syscin/syslib
	rm -f bigender64
	date >litender64

pos:	$(ENDER) bin/cintpos
	echo "bin/cintsys is up to date"

pos64:	$(ENDER)64 bin/cintpos64
	@echo "bin/cintsys64 is up to date"

pubthreadtest:
	cintsys -c xencode sysc/threadtest.c to sysc/threadtest.cx
	scp sysc/threadtest.c sysc/threadtest.cx trace.txt $(SSHPUB)/threadtest

threadtest:	bin/threadtest
	threadtest

bin/threadtest:	obj/threadtest.o
	$(CC) -o bin/threadtest obj/threadtest.o -lpthread

obj/threadtest.o:	sysc/threadtest.c Makefile
	$(CC) -o obj/threadtest.o -c sysc/threadtest.c

sysc/INT.h:	sysc/mkint-h.c
	$(CC) $(CFLAGS) -o mkint-h sysc/mkint-h.c $(LDFLAGS)
	./mkint-h >sysc/INT.h
	rm -f mkint-h

sysc/mkint-h.c:	$(BCPL)/sysc/mkint-h.c
	cp $(BCPL)/sysc/mkint-h.c sysc/mkint-h.c

bin/mconn:	obj/mconn.o obj/kblib.o
	$(CC) -o bin/mconn obj/mconn.o obj/kblib.o

obj/mconn.o:	sysc/mconn.c sysc/INT.h
	$(CC) -o obj/mconn.o -c sysc/mconn.c

sysc/mconn.c:	$(BCPL)/sysc/mconn.c
	cp $(BCPL)/sysc/mconn.c sysc/mconn.c



# Rules for the creation of cintpos

bin/cintpos:	\
	obj/cintpos.o obj/cinterp.o \
	$(FASTERP) obj/nullrastlib.o \
	obj/kblib.o \
	obj/joyfn.o obj/sdlfn.o obj/glfn.o obj/extfn.o \
	obj/sdldrawlib.o \
	obj/devices.o
	echo "linking bin/cintpos\n"
	$(CC) $(CFLAGS) -o bin/cintpos \
	obj/cintpos.o obj/cinterp.o \
	$(FASTERP) obj/nullrastlib.o \
	obj/kblib.o \
	obj/joyfn.o obj/sdlfn.o obj/glfn.o obj/extfn.o \
	obj/sdldrawlib.o \
	obj/devices.o \
	-lpthread -lm $(LDFLAGS)

bin/cintpos64:	\
	obj64/cintpos.o obj64/cinterp.o \
	$(FASTERP64) obj64/nullrastlib.o \
	obj64/kblib.o \
	obj64/joyfn.o obj64/sdlfn.o obj64/glfn.o obj64/extfn.o \
	obj64/sdldrawlib.o \
	obj64/devices.o
	$(CC) $(CFLAGS) -o bin/cintpos64 \
	obj64/cintpos.o obj64/cinterp.o \
	$(FASTERP64) obj64/nullrastlib.o \
	obj64/kblib.o \
	obj64/joyfn.o obj64/sdlfn.o obj64/glfn.o obj64/extfn.o \
	obj64/sdldrawlib.o \
	obj64/devices.o \
	-lpthread -lm $(LDFLAGS)

obj/cintpos.o:	sysc/cintpos.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) $(CFLAGS) -DCINTPOSyes -o obj/cintpos.o -c sysc/cintpos.c

# There are three versions of the Cintcode interpreter all compiled
# from the same source code cinterp.c. These versions are selected
# by defining CINTERPyes, FASTERPyes or RASTERPyes.

obj/cinterp.o:	sysc/cinterp.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj/cinterp.o -c sysc/cinterp.c

obj/fasterp.o:	sysc/cinterp.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -DFASTyes -o obj/fasterp.o -c sysc/cinterp.c

obj/devices.o:	sysc/devices.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj/devices.o -c sysc/devices.c

obj/cintasm.o:	$(SYSM)/cintasm.s sysc/INT.h Makefile
	$(CC) -o obj/cintasm.o -c $(SYSM)/cintasm.s

obj/kblib.o:	sysc/kblib.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) $(CFLAGS) -o obj/kblib.o -c sysc/kblib.c

obj/cfuncs.o:	sysc/cfuncs.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj/cfuncs.o -c sysc/cfuncs.c

obj/joyfn.o:	sysc/joyfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj/joyfn.o -c sysc/joyfn.c

obj/sdlfn.o:	sysc/sdlfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj/sdlfn.o -c sysc/sdlfn.c

obj/glfn.o:	sysc/glfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj/glfn.o -c sysc/glfn.c

obj/extfn.o:	sysc/extfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj/extfn.o -c sysc/extfn.c

obj/sdldrawlib.o:	sysc/sdldrawlib.c sysc/cintpos.h sysc/INT.h
#	$(CC) $(CFLAGS) -E sysc/sdldrawlib.c >sysc/sdldrawlib.txt
	$(CC) $(CFLAGS) -o obj/sdldrawlib.o -c sysc/sdldrawlib.c

obj/nullrastlib.o:	sysc/nullrastlib.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj/nullrastlib.o -c sysc/nullrastlib.c


obj64/cinterp.o:	sysc/cinterp.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj64/cinterp.o -c sysc/cinterp.c -DTARGET64

obj64/cintpos.o:	sysc/cintpos.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj64/cintpos.o -c sysc/cintpos.c -DTARGET64

obj64/fasterp.o:	sysc/cinterp.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -DFASTyes -o obj64/fasterp.o -c sysc/cinterp.c -DTARGET64

obj64/devices.o:	sysc/devices.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj64/devices.o -c sysc/devices.c -DTARGET64

obj64/cintasm.o:	$(SYSM64)/cintasm.s sysc/INT.h Makefile
	$(CC) -o obj64/cintasm.o -c $(SYSM64)/cintasm.s -DTARGET64

obj64/kblib.o:	sysc/kblib.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) $(CFLAGS) -o obj64/kblib.o -c sysc/kblib.c -DTARGET64

obj64/cfuncs.o:	sysc/cfuncs.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj64/cfuncs.o -c sysc/cfuncs.c -DTARGET64

obj64/joyfn.o:	sysc/joyfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj64/joyfn.o -c sysc/joyfn.c -DTARGET64

obj64/sdlfn.o:	sysc/sdlfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj64/sdlfn.o -c sysc/sdlfn.c -DTARGET64

obj64/glfn.o:	sysc/glfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj64/glfn.o -c sysc/glfn.c -DTARGET64

obj64/extfn.o:	sysc/extfn.c sysc/cintpos.h sysc/INT.h
	$(CC) $(CFLAGS) -o obj64/extfn.o -c sysc/extfn.c -DTARGET64

obj64/sdldrawlib.o:	sysc/sdldrawlib.c sysc/cintpos.h sysc/INT.h
#	$(CC) $(CFLAGS) -E sysc/sdldrawlib.c >sysc/sdldrawlib.txt
	$(CC) $(CFLAGS) -o obj64/sdldrawlib.o -c sysc/sdldrawlib.c -DTARGET64

obj64/nullrastlib.o:	sysc/nullrastlib.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj64/nullrastlib.o -c sysc/nullrastlib.c -DTARGET64


gp2x:	cintpos.gpu

gp2xinstall:	cintpos.gpu
	rm -rf $(SD)/cintpos
	cp -rp ../../Cintpos $(SD)/cintpos
	rm -f $(SD)/Bin/cintpos.gpu
	cp -r cintpos.gpu $(SD)/Bin/cintpos
	rm -f $(SD)/Bin/cintpos.gpe
	cp -r ../../GP2X/gp2x/cintpos.gpe $(SD)/Bin/cintpos.gpe

cintpos.gpu:	\
	obj/gp2xcintpos.o obj/gp2xcinterp.o obj/gp2xkblib.o \
	obj/gp2xfasterp.o obj/gp2xnullrastlib.o obj/gp2xdevices.o
	$(GP2XCC) -o cintpos.gpu obj/gp2xcintpos.o \
	obj/gp2xcinterp.o obj/gp2xkblib.o \
	obj/gp2xfasterp.o obj/gp2xnullrastlib.o obj/gp2xdevices.o -lpthread

obj/gp2xcintpos.o:	sysc/cintpos.c sysc/cintpos.h Makefile
	$(GP2XCC) -o obj/gp2xcintpos.o -c sysc/cintpos.c

obj/gp2xcinterp.o:	sysc/cinterp.c sysc/cintpos.h Makefile
	$(GP2XCC) -o obj/gp2xcinterp.o -c sysc/cinterp.c

obj/gp2xfasterp.o:	sysc/cinterp.c sysc/cintpos.h Makefile
	$(GP2XCC) -DFASTyes -o obj/gp2xfasterp.o -c sysc/cinterp.c

obj/gp2xdevices.o:	sysc/devices.c sysc/cintpos.h Makefile
	$(GP2XCC) -o obj/gp2xdevices.o -c sysc/devices.c

obj/gp2xcintasm.o:	$(SYSM)/cintasm.s Makefile
	$(GP2XCC) -o obj/gp2xcintasm.o -c $(SYSM)/cintasm.s

obj/gp2xkblib.o:	sysc/kblib.c sysc/cintpos.h Makefile
	$(GP2XCC) -o obj/gp2xkblib.o -c sysc/kblib.c

obj/gp2xnullrastlib.o:	sysc/nullrastlib.c sysc/cintpos.h Makefile
	$(GP2XCC) -o obj/gp2xnullrastlib.o -c sysc/nullrastlib.c


# Rules to compile the 32 and 64 bit BCPL source

cin/syscin/boot:	sysb/boot.b Makefile g/libhdr.h
	cintsys -c "c bs boot"

cin64/syscin/boot:	sysb/boot.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 boot"

cin/syscin/blib:	sysb/blib.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs blib"

cin64/syscin/blib:	sysb/blib.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 blib"

cin/syscin/dlib:	sysb/dlib.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs dlib"

cin64/syscin/dlib:	sysb/dlib.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 dlib"

cin/syscin/klib:	sysb/klib.b Makefile g/libhdr.h
	cintsys -c "c bs klib"

cin64/syscin/klib:	sysb/klib.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 klib"

cin/syscin/idle:	sysb/idle.b Makefile g/libhdr.h
	cintsys -c "c bs idle"

cin64/syscin/idle:	sysb/idle.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 idle"

cin/syscin/cli:	sysb/cli.b Makefile g/libhdr.h
	cintsys -c "c bs cli"

cin64/syscin/cli:	sysb/cli.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 cli"

cin/syscin/cli_init:	sysb/cli_init.b Makefile g/libhdr.h
	cintsys -c "c bs cli_init"

cin64/syscin/cli_init:	sysb/cli_init.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 cli_init"

cin/syscin/debug:	sysb/debug.b Makefile g/libhdr.h
	cintsys -c "c bs debug"

cin64/syscin/debug:	sysb/debug.b Makefile g/libhdr.h
	cintsys64 -c "c bs64 debug"

cin/syscin/cohand:	sysb/cohand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs cohand"

cin64/syscin/cohand:	sysb/cohand.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 cohand"

cin/syscin/fh0:	sysb/fh0.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs fh0"

cin64/syscin/fh0:	sysb/fh0.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 fh0"

cin/syscin/mbxhand:	sysb/mbxhand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs mbxhand"

cin64/syscin/mbxhand:	sysb/mbxhand.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 mbxhand"

cin/syscin/tcphand:	sysb/tcphand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs tcphand"
 
cin64/syscin/tcphand:	sysb/tcphand.b Makefile g/libhdr.h g/manhdr.h
	cintsys64 -c "c bs64 tcphand"

cin/bcpl:	com/bcpl.b com/bcplfe.b com/bcplcgcin.b g/bcplfecg.h \
	Makefile g/libhdr.h
	cintsys -c "c bc bcpl"

cin64/bcpl:	com/bcpl.b com/bcplfe.b com/bcplcgcin.b g/bcplfecg.h \
	Makefile g/libhdr.h
	cintsys64 -c "c bc64 bcpl"

cin/abort:	com/abort.b Makefile g/libhdr.h
	cintsys -c "c bc abort"

cin64/abort:	com/abort.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 abort"

cin/bounce:	com/bounce.b Makefile g/libhdr.h
	cintsys -c "c bc bounce"

cin64/bounce:	com/bounce.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 bounce"

cin/break:	com/break.b Makefile g/libhdr.h
	cintsys -c "c bc break"

cin64/break:	com/break.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 break"

cin/c:	com/c.b Makefile g/libhdr.h
	cintsys -c "c bc c"

cin64/c:	com/c.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 c"

cin/changepri:	com/changepri.b Makefile g/libhdr.h
	cintsys -c "c bc changepri"

cin64/changepri:	com/changepri.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 changepri"

cin/echo:	com/echo.b Makefile g/libhdr.h
	cintsys -c "c bc echo"

cin64/echo:	com/echo.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 echo"

cin/endcli:	com/endcli.b Makefile g/libhdr.h
	cintsys -c "c bc endcli"

cin64/endcli:	com/endcli.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 endcli"

cin/enlarge:	com/enlarge.b Makefile g/libhdr.h
	cintsys -c "c bc enlarge"

cin64/enlarge:	com/enlarge.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 enlarge"

cin/hold:	com/hold.b Makefile g/libhdr.h
	cintsys -c "c bc hold"

cin64/hold:	com/hold.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 hold"

cin/input:	com/input.b Makefile g/libhdr.h
	cintsys -c "c bc input"

cin64/input:	com/input.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 input"

cin/logout:	com/logout.b Makefile g/libhdr.h
	cintsys -c "c bc logout"

cin64/logout:	com/logout.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 logout"

cin/map:	com/map.b Makefile g/libhdr.h
	cintsys -c "c bc map"

cin64/map:	com/map.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 map"

cin/newcli:	com/newcli.b Makefile g/libhdr.h
	cintsys -c "c bc newcli"

cin64/newcli:	com/newcli.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 newcli"

cin/prompt:	com/prompt.b Makefile g/libhdr.h
	cintsys -c "c bc prompt"

cin64/prompt:	com/prompt.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 prompt"

cin/unhold:	com/unhold.b Makefile g/libhdr.h
	cintsys -c "c bc unhold"

cin64/unhold:	com/unhold.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 unhold"

cin/repeat:	com/repeat.b Makefile g/libhdr.h
	cintsys -c "c bc repeat"

cin64/repeat:	com/repeat.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 repeat"

cin/run:	com/run.b Makefile g/libhdr.h
	cintsys -c "c bc run"

cin64/run:	com/run.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 run"

cin/send:	com/send.b Makefile g/libhdr.h
	cintsys -c "c bc send"

cin64/send:	com/send.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 send"

cin/stack:	com/stack.b Makefile g/libhdr.h
	cintsys -c "c bc stack"

cin64/stack:	com/stack.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 stack"

cin/status:	com/status.b Makefile g/libhdr.h
	cintsys -c "c bc status"

cin64/status:	com/status.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 status"

cin/test1:	com/test1.b Makefile g/libhdr.h
	cintsys -c "c bc test1"

cin64/test1:	com/test1.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 test1"

cin/type:	com/type.b Makefile g/libhdr.h
	cintsys -c "c bc type"

cin64/type:	com/type.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 type"

cin/wait:	com/wait.b Makefile g/libhdr.h
	cintsys -c "c bc wait"

cin64/wait:	com/wait.b Makefile g/libhdr.h
	cintsys64 -c "c bc64 wait"

env:
	@echo
	@echo "To set the Cintpos environment variables"
	@echo "read, edit and run the script ./setenv, which is:"
	@echo 
	cat ./setenv
	@echo
# Alternatively put something like the following in your .profile file.
#	export POSROOT=S(HOME)/distribution/Cintpos/cintpos
#	export POSPATH=$CINTPOS/cin
#	export POSHDRS=$CINTPOS/g
#	export PATH=$PATH:$CINTPOS:.

clean:
	rm -f sysc/mkint-h.c sysc/mconn.c
	rm -f OCODE COMFILE core rawxref
	rm -f *~ sysc/*~ sysb/*~ sysasm/*/*~ com/*~ doc/*~
	rm -f litender bigender sysc/INT.h
	rm -rf obj  cin/syscin cin
	rm -f cintpos
	rm -f bin/cintpos
	rm -f bin/cintpos.exe bin/rastpos.exe
	rm -f raster raster.ps stats analysis profile
	mkdir cin cin/syscin
	mkdir obj
	rm -f allcompiled bcplready

clean64:
	rm -f OCODE COMFILE core rawxref
	rm -f *~ sysc/*~ sysb/*~ sysasm/*/*~ com/*~ doc/*~
	rm -f sysc/INT.h
	rm -f litender64 bigender64
	rm -rf obj64 cin64/syscin cin64
	rm -f cintsys64
	rm -f bin/cintsys64
	rm -f bin/cintsys64.exe
	mkdir cin64 cin64/syscin
	mkdir obj64
	rm -f allcompiled64 bcplready64

vclean:	clean clean64




