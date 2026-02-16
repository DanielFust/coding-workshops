#--------------------------------------------------------------------------------------------------
# This is a Makefile. It provides easy commands to build and run the Fortran "Hello, World!" 
# program. Variables may be defined similar to how they are in a shell script, and are commonly
# used to specify compilers and compiler flags that may differ from system to system or may be 
# required by multiple translation units.
#--------------------------------------------------------------------------------------------------

# Define the Fortran compiler to use.
FC = gfortran

# Define the compiler flags to use. The -Wall flag enables all compiler warnings, which is a good
# practice to help catch potential issues in the code.
FFLAGS = -Wall

# Define the target executable name.
TARGET = hello_world

# Define the command to build the executable:
# The '-o' flag specifies the name of the output executable, and 'hello_world.f90' 
# is the source file to compile.
$(TARGET): 
	$(FC) $(FFLAGS) -o hello_world hello_world.f90

# Define the command to clean up the directory by removing the compiled executable.
clean:
	rm -f $(TARGET)
