# Define the Fortran compiler to use.
FC = gfortran

# Define the compiler flags to use.
FFLAGS = -Wall

# Define the target executable name.
TARGET = loops_and_conditionals

# Define the command to build the executable:
$(TARGET): 
	$(FC) $(FFLAGS) -o loops_and_conditionals loops_and_conditionals.f90

# Define the command to clean up the directory by removing the compiled executable.
clean:
	rm -f $(TARGET)
