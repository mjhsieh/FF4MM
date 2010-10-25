# Copyright (c) 2010, Mengjuei Hsieh,
# All rights reserved. - please read information in "LICENCSE.txt"
# Written by Mengjuei Hsieh, University of California Irvine
FPP=cpp -traditional -P

FC=ifort -FR
FOPTFLAGS=-ip -O3 -axSTP
FLIB=-L/opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_intel_lp64.a \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_sequential.a \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_core.a \
     -lpthread -lsvml
FC=gfortran -ffree-form
FOPTFLAGS=-O3 -mtune=generic -msse -mfpmath=sse
FLIB=


OBJ=simplemm.o unittest.o framework.o utilities.o vector.o energy.o

simplemm: $(OBJ)
	$(FC) $(FOPTFLAGS) -o simplemm $(OBJ) \
	$(FLIB)

unittest: simplemm
	ln -fs simplemm unittest

install: simplemm unittest
	mv simplemm unittest ../bin/

uninstall:
	rm -f ../bin/unittest ../bin/simplemm

clean:
	rm -f unittest simplemm $(OBJ)
	rm -f *.mod _*.f

.f.o: $<
	$(FPP) $< > _$<
	$(FC) -c $(FOPTFLAGS) -o $@ _$<

framework.o: \
	definitions.fpp

tienchun.mod: \
	framework.o

vector.o: \
	definitions.fpp

vec3d.mod: \
	vector.o

utilities.o: \
	definitions.fpp \
	tienchun.mod

unittest.o: \
	definitions.fpp \
	tienchun.mod \
	utilities.o \
	vec3d.mod \
	energy.o

constants.o: \
	definitions.fpp

energy.o: \
	definitions.fpp
