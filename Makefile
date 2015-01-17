# Makefile for IRAS

FC = gfortran
FFLAGS=-O2 -ffixed-line-length-132
FLDFLAGS=

SOURCES=modules.for Allocations.for Gwlnkq.for Loss.for Performance.for Read_sim_data.for \
Simlnk.for Finterp.for Hydsim.for  rd_flow_day.for Release.for \
Simsys.for Flwsim.for InitSys.for Output.for Readfiles.for Selseq.for

OBJECTS=$(SOURCES:.for=.o)
EXECUTABLE=iras
.SUFFIXES: .o .for

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(FC) $(FLDFLAGS) $(OBJECTS) -o $@

.for.o :
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf *.o *.mod iras