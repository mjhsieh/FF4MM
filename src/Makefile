# Copyright (c) 2010, Mengjuei Hsieh,
# All rights reserved. - please read information in "LICENCSE.txt"
# Written by Mengjuei Hsieh, University of California Irvine

FC=ifort -FR
FPP=cpp -traditional -P
FORTFLAGS=-ip -O3 -axSTP
FLIB=-L/opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_intel_lp64.a \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_sequential.a \
     /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_core.a \
     -lpthread -lsvml

uOBJ=framework.o utilities.o vector.o energy.o

default:
	@echo "no default rule"

unittest: $(uOBJ) unittest.o
	$(FC) $(FOPTFLAGS) -o unittest unittest.o $(uOBJ) \
	$(FLIB)

install: unittest
	mv unittest ../bin/

uninstall:
	rm -f ../bin/unittest

clean:
	rm -f unittest unittest.o $(uOBJ)
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
#        timer.o

constants.o: \
	definitions.fpp

energy.o: \
	definitions.fpp
