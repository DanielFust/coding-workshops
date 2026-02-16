# Fortran Style Guide

## Fortran Language Resources
- Fortran-Lang basics: https://fortran-lang.org/learn/
- Fortran90 best practices: https://www.fortran90.org/src/best-practices.html

## Naming Conventions

- Fortran constructs (e.g. `do`, `if`, `module`, `function`, `subroutine`, ...) should be lowercase.
- Attributes should be written in lowercase (e.g. `pointer`, `intent`, `allocatable`, ...)
- Follow capitalization conventions for mathematical variables and procedures (`Gamma`, `theta`, ...)
- Other variables and procedures should be lowercase (e.g. `spline`, `time`, `meshgrid`, ...). Longer names should be snake case (e.g. `spline_interp`, `start_time`, ...)
- Constants (Fortran parameters) should be written in all capitals (e.g. `real, parameter :: PI=3.14159`)
- Module names should end with a `_m` suffix (e.g. `my_module_m`)
- Derived data type names (type name not instance name) should end with a `_t` suffix and be snake case (e.g. `my_type_t`)

## Indentation & Spacing

- Indent after all Fortran constructs (e.g. `subroutine`, `function`, `program`, `do`, `if`, `block`, `interface`, `module`, `type`, etc.)
- Indentation should be consistent within a file. Either two or four spaces is preferred.
- Line width should not exceed a specified number of columns (usually 80 or 100) within a file unless it notably improves clarity.

## Code structure

- All procedures (subroutine and function) and derived types should be enclosed in modules
- In most instances, programs, modules, and procedures should have a title block.


## Templates and Examples
### Subroutines

```fortran
    !--------------------------------------------------------------------------
    ! subroutine my_subroutine(...)
    !
    ! Description of subroutine
    !
    ! Dummy Arguments:
    !   input1.......description of input 1
    !   input2.......description of input 2
    !   output1......description of output 1
    !   output2......description of output 2
    !--------------------------------------------------------------------------
    subroutine my_subroutine(input1,input2,output1, output2)
      implicit none
      !------------ Dummy Arguments -------------
      integer, intent(in)    :: input1
      integer, intent(in)    :: input2
      integer, intent(in)    :: input3
      integer, intent(out)   :: output1
      integer, intent(inout) :: output2
      !--------------- Constants -----------------
      !------------- Local Variables -------------
      !-------------------------------------------

      !-- subroutine body --
    end subroutine my_subroutine
```




