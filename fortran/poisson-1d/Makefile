# Fortran compiler
FC = gfortran

# Compiler flags (O0, O1, O2, O3, Ofast) are optimization levels, -Wall enables all compiler warnings
FFLAGS = -O0 -Wall 

# Source code directory (used here to easily switch between the template and the examples)
SRC_DIR = ./example

# Target executable name
TARGET = poisson

# Build the target executable (my_module.o is a dependency, so the compiler will first compile 
#                              my_module.f90 into my_module.o if it hasn't been done already)
$(TARGET): my_module_m.o
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC_DIR)/$(TARGET).f90 my_module_m.o

# Compile the module (the -c flag tells the compiler to compile without linking)
my_module_m.o: $(SRC_DIR)/my_module_m.f90
	$(FC) $(FFLAGS) -c $(SRC_DIR)/my_module_m.f90

# Clean up the directory by removing object files and the executable
clean:
	rm -f *.o $(TARGET)