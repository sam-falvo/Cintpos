# The is the Makefile to (re-)build the BCPL Cintpos System

# Implemented by Martin Richards (c) 29 December 2019

# Change history

# 29/10/18
# Made this Makefile more compatible with the one for cintsys

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

# (Renamed most file names to lowercase letters to simplify running on
# Windows machines.)
# Modified the Makefile to allow compilation of the system for the
# GP2X handheld machine when running under Cygwin.

# Before running this makefile check that the environment variables
# POSLROOT, POSHDRS, POSPATH, POSSCRIPTS and PATH are correctly defined
# as described in docs/README

# Just modified to allow cross compilation for the GP2X handheld Linux machine.

# Location of Assembly language source code (architecture dependent)
# for example: SYSM = sysasm/LINUX

# My public HTML directory when not mountable on this machine.
# Remember to call ssh-add before calling make sshpub
SSHPUB = rhee.cl.cam.ac.uk:public_html

# The following for armgcc under cygwin
#GP2XCC = /cygdrive/c/devkitGP2X/bin/arm-linux-gcc -O4 -static -DforGP2X

# The following for arm-linux-gcc for GP2X under Linux
GP2XCC = /home/mr10/devkitPro/devkitGP2X/bin/arm-linux-gcc -O4 -static -DforGP2X

# The following for arm gnueabi-gcc under Linux
#GP2XCC = arm-none-linux-gnueabi-gcc -O4 -static -DforLinux

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

# Assume a little ender machine unless otherwise specified
ENDER = litender

#CC = gcc -O4 -DforMAC
#ENDER = bigender

#CC = gcc -O4 -DforMIPS

#CC = gcc -O4 -DforSGI
#ENDER = bigender

# For ARM Linux without any graphics libraries eg Raspberry Pi
#CC = gcc -O2 -DforARM -DEXTavail

# For Raspberry Pi Linux with the SDL library
#CC = gcc -O4 -DforRaspiSDL -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs`

# For Raspberry Pi Linux with EGL and OpenGL ES
#CC = gcc -O4 -DforRaspiGL -DEXTavail
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
CC = gcc -O4 -DforLinux -DSOUND -DEXTavail

# For Pentium Linux without any graphics libraries but with a joystick
#CC = gcc -O4 -DforLinux -DSOUND -DCALLC -DEXTavail -DJSAvail

# For Linux with the SDL features
#CC = gcc -O4 -DforLinuxSDL -DSOUND -DCALLC -DEXTavail -DJSAvail
#CC = gcc -O4 -DforLinuxSDL -DSOUND -DCALLC -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs`

# For Linux with GL features called from SDL
#CC = gcc -O4 -DforLinuxGL -DSOUND -DCALLC -DEXTavail
#CFLAGS+=`sdl-config --cflags`
#LDFLAGS+=`sdl-config --libs` -lGL

# For 32 bit Pentium Linux compiling a 64-bit Cintcode interpreter.
#CC = gcc -O4 -DforLinux64

# The following works under Cygwin on Pentiums and AMD 64 Athlon machines.
# make gp2x will cross compile the system for the GP2x handheld machine,
# and make gp2xinstall will install the BCPL Cintcode system on the SD memory
# card of the GP2X assuming it is on drive G:.
#CC = gcc -O4 -DforCYGWIN32

# Version for the Power PC running Mac OS 10.3.9 -- added 26/7/05
#CC = gcc -O4 -DforMacOSPPC
#ENDER = bigender

# Version for the Macmini running Intel Mac OS-X 32 bit version -- added 12/01/11
#CC = gcc -O4 -m32 -DforMacOSX
#ENDER = litender

# The following is under development for AMD 64 Athlon machines
#CC = gcc -O4 -DforLinuxAMD64

# Version for the PowerMac G4 running Debian Linux was
# kindly provided by Chris Emerson
#CC = gcc -O4 -DforLinuxPPC
#ENDER = bigender

#CC = gcc -O4 -DforSUN4
#ENDER = bigender

#CC = gcc -O4 -DforSPARC
#ENDER = bigender

#CC = gcc -O4 -DforALPHA

#CC = gcc -O4 -DforMSDOS

#CC = gcc -O4 -DforOS2


all:	$(ENDER) \
	bin/mconn \
	bin/cintpos cin/syscin/boot cin/syscin/blib cin/syscin/klib \
	cin/syscin/idle cin/syscin/cli cin/syscin/cli_init cin/syscin/debug \
	cin/syscin/cohand cin/syscin/fh0 cin/syscin/mbxhand cin/syscin/tcphand\
	cin/syscin/dlib \
	cin/abort cin/bounce cin/break \
	cin/c cin/changepri \
	cin/endcli cin/echo cin/enlarge \
	cin/hold cin/input cin/newcli \
	cin/logout cin/map cin/newcli cin/prompt \
	cin/unhold cin/repeat cin/run cin/send \
	cin/stack cin/status cin/test1 \
	cin/type cin/wait
	@echo
	./bin/cintpos

run:	all sane
	./bin/cintpos

sys:	bin/cintpos $(ENDER)
	echo "bin/cintpos is up to date"

run64:	bcplready64
	bin/cintpos64

sys64:	bin/cintpos64 $(ENDER)64
	echo "bin/cintsys64 is up to date"

bcplready:	bin/cintpos $(ENDER) allcompiled
	echo touch >bcplready

xref:	rawxref
	cintsys -c sortxref rawxref to xrefdata
	rm rawxref

rawxref:
	cintsys -c c compall xref >rawxref

bigender:
	cp enderbig/syslib cin/syscin
	cp enderbig/boot cin/syscin
	cp enderbig/blib cin/syscin
	cp enderbig/klib cin/syscin
	cp enderbig/dlib cin/syscin
	cp enderbig/idle cin/syscin
	cp enderbig/cli cin/syscin
	cp enderbig/cli_init cin/syscin
	cp enderbig/debug cin/syscin
	cp enderbig/cohand cin/syscin
	cp enderbig/fh0 cin/syscin
	cp enderbig/mbxhand cin/syscin
	cp enderbig/tcphand cin/syscin
	cp enderbig/tbcpl cin
	cp enderbig/bcpl cin
	cp enderbig/c cin
	cp enderbig/echo cin
	cp enderbig/logout cin
	cp enderbig/cmpltest cin
	cp enderbig/map cin
	cp enderbig/abort cin
	cp enderbig/bcb .
	cp enderbig/bcl .
	cp enderbig/bsb .
	cp enderbig/bsl .
	echo touch >bigender

litender:
	cp enderlit/syslib cin/syscin
	cp enderlit/boot cin/syscin
	cp enderlit/blib cin/syscin
	cp enderlit/klib cin/syscin
	cp enderlit/dlib cin/syscin
	cp enderlit/idle cin/syscin
	cp enderlit/cli cin/syscin
	cp enderlit/cli_init cin/syscin
	cp enderlit/debug cin/syscin
	cp enderlit/cohand cin/syscin
	cp enderlit/fh0 cin/syscin
	cp enderlit/mbxhand cin/syscin
	cp enderlit/tcphand cin/syscin
	cp enderlit/tbcpl cin
	cp enderlit/bcpl cin
	cp enderlit/c cin
	cp enderlit/echo cin
	cp enderlit/logout cin
	cp enderlit/cmpltest cin
	cp enderlit/map cin
	cp enderlit/abort cin
	cp enderlit/bcb .
	cp enderlit/bcl .
	cp enderlit/bsb .
	cp enderlit/bsl .
	echo touch >litender

sane:
	stty sane

pubthreadtest:
	cintsys -c xencode sysc/threadtest.c to sysc/threadtest.cx
	scp sysc/threadtest.c sysc/threadtest.cx trace.txt $(SSHPUB)/threadtest

threadtest:	bin/threadtest
	threadtest

bin/threadtest:	obj/threadtest.o
	$(CC) -o bin/threadtest obj/threadtest.o -lpthread

obj/threadtest.o:	sysc/threadtest.c Makefile
	$(CC) -o obj/threadtest.o -c sysc/threadtest.c

bin/cintpos:	\
	obj/cintpos.o obj/cinterp.o \
	$(FASTERP) obj/nullrastlib.o \
	obj/kblib.o \
	obj/joyfn.o obj/sdlfn.o obj/glfn.o obj/extfn.o \
	obj/sdldrawlib.o \
	obj/devices.o
	$(CC) $(CFLAGS) -o bin/cintpos \
	obj/cintpos.o obj/cinterp.o \
	$(FASTERP) obj/nullrastlib.o \
	obj/kblib.o \
	obj/joyfn.o obj/sdlfn.o obj/glfn.o obj/extfn.o \
	obj/sdldrawlib.o \
	obj/devices.o \
	-lpthread -lm $(LDFLAGS)


bin/mconn:	obj/mconn.o obj/kblib.o
	$(CC) -o bin/mconn obj/mconn.o obj/kblib.o

obj/mconn.o:	sysc/mconn.c sysc/INT.h
	$(CC) -o obj/mconn.o -c sysc/mconn.c


sysc/INT.h:	sysc/mkint-h.c
	gcc -o mkint-h sysc/mkint-h.c
	mkint-h >sysc/INT.h
	rm -f mkint-h

obj/cintpos.o:	sysc/cintpos.c sysc/cintpos.h sysc/INT.h Makefile
	$(CC) -o obj/cintpos.o -c sysc/cintpos.c

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



# Rules to compile the GP2X version

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


# Rules to compile the BCPL source

cin/syscin/boot:	sysb/boot.b Makefile g/libhdr.h
	cintsys -c "c bs boot"

cin/syscin/blib:	sysb/blib.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs blib"

cin/syscin/dlib:	sysb/dlib.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs dlib"

cin/syscin/klib:	sysb/klib.b Makefile g/libhdr.h
	cintsys -c "c bs klib"

cin/syscin/idle:	sysb/idle.b Makefile g/libhdr.h
	cintsys -c "c bs idle"

cin/syscin/cli:	sysb/cli.b Makefile g/libhdr.h
	cintsys -c "c bs cli"

cin/syscin/cli_init:	sysb/cli_init.b Makefile g/libhdr.h
	cintsys -c "c bs cli_init"

cin/syscin/debug:	sysb/debug.b Makefile g/libhdr.h
	cintsys -c "c bs debug"

cin/syscin/cohand:	sysb/cohand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs cohand"

cin/syscin/fh0:	sysb/fh0.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs fh0"

cin/syscin/mbxhand:	sysb/mbxhand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs mbxhand"

cin/syscin/tcphand:	sysb/tcphand.b Makefile g/libhdr.h g/manhdr.h
	cintsys -c "c bs tcphand"
 
cin/abort:	com/abort.b Makefile g/libhdr.h
	cintsys -c "c bc abort"

cin/bounce:	com/bounce.b Makefile g/libhdr.h
	cintsys -c "c bc bounce"

cin/break:	com/break.b Makefile g/libhdr.h
	cintsys -c "c bc break"

cin/c:	com/c.b Makefile g/libhdr.h
	cintsys -c "c bc c"

cin/changepri:	com/changepri.b Makefile g/libhdr.h
	cintsys -c "c bc changepri"

cin/echo:	com/echo.b Makefile g/libhdr.h
	cintsys -c "c bc echo"

cin/endcli:	com/endcli.b Makefile g/libhdr.h
	cintsys -c "c bc endcli"

cin/enlarge:	com/enlarge.b Makefile g/libhdr.h
	cintsys -c "c bc enlarge"

cin/hold:	com/hold.b Makefile g/libhdr.h
	cintsys -c "c bc hold"

cin/input:	com/input.b Makefile g/libhdr.h
	cintsys -c "c bc input"

cin/logout:	com/logout.b Makefile g/libhdr.h
	cintsys -c "c bc logout"

cin/map:	com/map.b Makefile g/libhdr.h
	cintsys -c "c bc map"

cin/newcli:	com/newcli.b Makefile g/libhdr.h
	cintsys -c "c bc newcli"

cin/prompt:	com/prompt.b Makefile g/libhdr.h
	cintsys -c "c bc prompt"

cin/unhold:	com/unhold.b Makefile g/libhdr.h
	cintsys -c "c bc unhold"

cin/repeat:	com/repeat.b Makefile g/libhdr.h
	cintsys -c "c bc repeat"

cin/run:	com/run.b Makefile g/libhdr.h
	cintsys -c "c bc run"

cin/send:	com/send.b Makefile g/libhdr.h
	cintsys -c "c bc send"

cin/stack:	com/stack.b Makefile g/libhdr.h
	cintsys -c "c bc stack"

cin/status:	com/status.b Makefile g/libhdr.h
	cintsys -c "c bc status"

cin/test1:	com/test1.b Makefile g/libhdr.h
	cintsys -c "c bc test1"

cin/type:	com/type.b Makefile g/libhdr.h
	cintsys -c "c bc type"

cin/wait:	com/wait.b Makefile g/libhdr.h
	cintsys -c "c bc wait"

env:
	@echo
	@echo "To set the Cintpos environment variables"
	@echo "read, edit and run the script ./setenv, which is:"
	@echo 
	cat ./setenv
	@echo
# Alternatively put something like the following in your .profile file.
#	export POSROOT=/homes/mr10/distribution/Cintpos/cintpos
#	export POSPATH=$CINTPOS/cin
#	export POSHDRS=$CINTPOS/g
#	export PATH=$PATH:$CINTPOS:.

clean:
	rm -f OCODE COMFILE litender bigender sysc/INT.h
	rm -f bin/cintpos bin/cintpos.exe
	rm -f ../*~ *~ */*~
	rm -rf obj cin/syscin cin
	mkdir cin cin/syscin
	mkdir obj

vclean:	clean




