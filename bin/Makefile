####### General Compiler Flags ######################################

#RELEASEFLAGS=-g -O3 -static
#FCFLAGS=$(RELEASEFLAGS)
FCFLAGS=-g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -Wall
#The below flags are needed for static linking with openmp
#FCFLAGS=-g -O3 -static -Wl,--whole-archive -lpthread -Wl,--no-whole-archive -flto
#FCFLAGS=-g -O3

CPPFLAGS=$(FCFLAGS)

#####################################################################

####### GNU Compiler Flags ##########################################

FC=gfortran
CPP=g++
#OMP_FLAG=-fopenmp
XFOIL_REAL8_FLAG=-fdefault-real-8

#####################################################################

####### Intel Compiler Flags ########################################

#FC=ifort
#OMP_FLAG=-openmp
#XFOIL_REAL8_FLAG=-r8

#####################################################################

####### Preprocessor variables ######################################

ifeq ($(FC), ifort)
  PREPROC=-Dintel_compilers
else
  PREPROC=-Uintel_compilers
endif

####### Definitions, rules, targets #################################

#OBJ=math_deps.o vardef.o xfoil_inc.o xfoil_math.o my_equivalence.o xfoil_geom.o blpar_inc.o xbl_inc.o xfoil_bl.o xfoil_aero.o xfoil_driver.o parametrization.o airfoil_operations.o airfoil_evaluation.o optimization_util.o particle_swarm.o genetic_algorithm.o simplex_search.o input_output.o input_sanity.o memory_util.o optimization_driver.o xoptfoil_interface.o dummy_main.o
OBJ=math_deps.o xfoil_inc.o xfoil_math.o my_equivalence.o xfoil_geom.o blpar_inc.o xbl_inc.o xfoil_bl.o xfoil_aero.o xfoil_driver.o optimization_util.o vardef.o parametrization.o airfoil_operations.o input_output.o memory_util.o xoptfoil_interface.o cli_main.o
EXE=xoptfoil
XOBJ=math_deps.o vardef.o xfoil_inc.o xfoil_math.o my_equivalence.o xfoil_geom.o blpar_inc.o xbl_inc.o xfoil_bl.o xfoil_aero.o xfoil_driver.o parametrization.o airfoil_operations.o airfoil_evaluation.o input_output.o xfoil_only.o
XEXE=xfoil_only
DIR=../src
INCLUDE=$(DIR)/include

# all: $(EXE) $(XEXE)
all: $(EXE)

remake: clean all

$(EXE): $(OBJ)
	$(CPP) $(CPPFLAGS) $(OMP_FLAG) -o $(EXE) $(OBJ) -lgfortran

$(XEXE): $(XOBJ)
	$(FC) $(FCFLAGS) $(OMP_FLAG) -o $(XEXE) $(XOBJ)

clean: 
	rm -f *.mod *.o

math_deps.o: $(DIR)/fortran/math_deps.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/math_deps.f90

vardef.o: $(DIR)/fortran/vardef.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/vardef.f90

xfoil_inc.o: $(DIR)/fortran/xfoil_inc.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_inc.f90
xfoil_math.o: $(DIR)/fortran/xfoil_deps/xfoil_math.f
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(XFOIL_REAL8_FLAG) $(DIR)/fortran/xfoil_deps/xfoil_math.f

my_equivalence.o: $(DIR)/fortran/xfoil_deps/my_equivalence.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_deps/my_equivalence.f90

xfoil_geom.o: $(DIR)/fortran/xfoil_deps/xfoil_geom.f
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(XFOIL_REAL8_FLAG) $(DIR)/fortran/xfoil_deps/xfoil_geom.f

blpar_inc.o: $(DIR)/fortran/xfoil_deps/blpar_inc.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_deps/blpar_inc.f90

xbl_inc.o: $(DIR)/fortran/xfoil_deps/xbl_inc.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_deps/xbl_inc.f90

xfoil_bl.o: $(DIR)/fortran/xfoil_deps/xfoil_bl.f
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(XFOIL_REAL8_FLAG) $(DIR)/fortran/xfoil_deps/xfoil_bl.f

xfoil_aero.o: $(DIR)/fortran/xfoil_deps/xfoil_aero.f
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(XFOIL_REAL8_FLAG) $(DIR)/fortran/xfoil_deps/xfoil_aero.f

xfoil_driver.o: $(DIR)/fortran/xfoil_driver.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_driver.f90

parametrization.o: $(DIR)/fortran/parametrization.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/parametrization.f90

airfoil_operations.o: $(DIR)/fortran/airfoil_operations.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/airfoil_operations.f90

airfoil_evaluation.o: $(DIR)/fortran/airfoil_evaluation.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/airfoil_evaluation.f90

optimization_util.o: $(DIR)/fortran/optimization_util.F90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(PREPROC) $(DIR)/fortran/optimization_util.F90

particle_swarm.o: $(DIR)/fortran/particle_swarm.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/particle_swarm.f90

genetic_algorithm.o: $(DIR)/fortran/genetic_algorithm.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/genetic_algorithm.f90

simplex_search.o: $(DIR)/fortran/simplex_search.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/simplex_search.f90

input_output.o: $(DIR)/fortran/input_output.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/input_output.f90

input_sanity.o: $(DIR)/fortran/input_sanity.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/input_sanity.f90

memory_util.o: $(DIR)/fortran/memory_util.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/memory_util.f90

optimization_driver.o: $(DIR)/fortran/optimization_driver.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/optimization_driver.f90

main.o: $(DIR)/fortran/main.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/main.f90

xoptfoil_interface.o: $(DIR)/fortran/xoptfoil_interface.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xoptfoil_interface.f90

cli_main.o: $(DIR)/cpp/cli_main.cpp
	$(CPP) -c $(CPPFLAGS) $(OMP_FLAG) -I$(INCLUDE) $(DIR)/cpp/cli_main.cpp

xfoil_only.o: $(DIR)/fortran/xfoil_only.f90
	$(FC) -c $(FCFLAGS) $(OMP_FLAG) $(DIR)/fortran/xfoil_only.f90