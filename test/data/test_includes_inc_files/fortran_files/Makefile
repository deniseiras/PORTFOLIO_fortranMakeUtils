# Compiler options
FC = gfortran
FFLAGS = -Wall -Wextra -I'./includes' -ffree-form -ffree-line-length-none

# Source files
SRCS := $(wildcard *.F)

# Object files
OBJS := $(patsubst %.F, %.o, $(SRCS))

# Default target
all: objs

# Program target
program: $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^

# objects target
objs: $(OBJS)

# Object file compilation
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Clean target
clean:
	rm -f program $(OBJS)
