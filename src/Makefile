FC=ifort -FR
FPP=cpp -traditional -P
FORTFLAGS=-ip -O3 -xHost
FLIB=
#FLIB=-L/opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_intel_lp64.a /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_sequential.a /opt/intel/Compiler/11.1/084/Frameworks/mkl/lib/em64t/libmkl_core.a -lpthread -lsvml

#default:
#	echo "no default rule"

unittest: f90vector.o unittest.o
	$(FC) $(FOPTFLAGS) -o unittest f90vector.o unittest.o \
	$(FLIB)

.f.o: $<
	$(FPP) $< > _$<
	$(FC) -c $(FOPTFLAGS) -o $@ _$<

clean:
	rm -f tienchun.mod unittest f90vector.o unittest.o
	rm -f _*.f

f90vector.o: \
	definitions.fpp

unittest.o: \
	definitions.fpp \
	tienchun.mod

tienchun.mod: \
	f90vector.o
