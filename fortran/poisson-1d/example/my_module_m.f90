!--------------------------------------------------------------------------------------------------
! module my_module_m
!
! Modules are use to encapsulate procedures (subroutines and functions) and data. It is good 
! practice to define procedures in modules to avoid naming conflicts and prevent a program from
! becoming too bloated.
!--------------------------------------------------------------------------------------------------
module my_module_m
  implicit none

  !-- The initial 'block' of a module is used to declare module variables, parameters, interfaces,
  !   derived types (similar to structs or classes in other languages) and declare any 'public' or 
  !   'private' attributes for the module.


  !-- It is common to define a parameter for the working precision (WP) in a module so that it can 
  !   be easily changed in one place if needed.
  integer,  parameter :: WP = KIND(1.0d0) 
  real(WP), parameter :: pi = 3.1415926535897932384626433832795_WP !<-- example of a real parameter


  !-- The 'public' attribute allows the procedures and variables to be accessed from outside the 
  !   module. By default, all procedures and variables in a module are 'public' unless they are 
  !   declared as 'private'.
  public :: tridiagonal_solver
  public :: write_results
  public :: factorial

  
  !-- After the 'contains' statement, we define the procedure bodies
  contains

    !----------------------------------------------------------------------------------------------
    ! subroutine tridiagonal_solver(...)
    !
    ! This subroutine implements the Thomas algorithm, which is a direct method for solving 
    ! tridiagonal matrix equations using forward elimination and back substitution. The main 
    ! diagonals are stored as vectors of length 'n' to prevent storing unnecessary zero entries. 
    !
    ! Linear system: A*u = f
    !
    !      | b1 c1 0  0  ... 0 |
    !      | a2 b2 c2 0  ... 0 |
    !  A = | 0  a3 b3 c3 ... 0 |
    !      | :  .  .  .  ... . |
    !      | 0  ... an bn cn-1 |
    !      | 0  ... 0  an bn   |
    !
    ! Dummy Arguments:
    !   a(:) - sub-diagonal entries (a(1) is not used)
    !   b(:) - diagonal entries
    !   c(:) - super-diagonal entries (c(n) is not used)
    !   f(:) - right-hand side vector
    !   u(:) - solution vector (output)
    !----------------------------------------------------------------------------------------------
    subroutine tridiagonal_solver(a, b, c, f, u)
      !------------ dummy arguments -------------
      real(WP), intent(inout) :: a(:), b(:), c(:), f(:)
      real(WP), intent(out)   :: u(:)
      !------------ Local variables -------------
      integer :: n, i
      !------------------------------------------

      !-- Note: it is good practice to assign an 'intent' to each argument of a subroutine.
      !         This can assist in debugging and clearly communicates to other uses (including
      !         yourself in the future) which arguments may be modified by the subroutine and which
      !         will remain constant. The 'intent' can be 'in', 'out' or 'inout' depending on 
      !         whether the argument is only read.

      !-- unpack the size of the arrays (assuming they are all the same size)
      n = size(a)

      !-- Forward elimination
      do i = 2,n
        a(i) = a(i)/b(i-1)
        b(i) = b(i) - a(i)*c(i-1)
        f(i) = f(i) - a(i)*f(i-1)
      enddo

      !-- Back substitution
      u(n) = f(n)/b(n)
      do i = n-1,1,-1
        u(i) = (f(i) - c(i)*u(i+1))/b(i)
      enddo
    end subroutine tridiagonal_solver


    !----------------------------------------------------------------------------------------------
    ! subroutine write_results(filename, x, u)
    !
    ! This is an example subroutine that writes the results of a computation to a text file for 
    ! later analysis or visualization. 
    !
    ! Dummy Arguments:
    !   filename - name of the output file
    !   x(:)     - array of x values (input)
    !   u(:)     - array of u values (input)
      !----------------------------------------------------------------------------------------------
    subroutine write_results(filename, x, u)
      !-------------- dummy arguments -------------
      character(len=*), intent(in) :: filename
      real(WP),         intent(in) :: x(:), u(:)
      !-------------- local variables -------------
      integer :: n, i
      !--------------------------------------------
      
      n = size(x)
      !-- first the file must be opened --
      open(unit=10, file=filename, status='replace')
      !-- then the data can be written to the file using 'write' statements --
      do i = 1,n
        write(10,*) x(i), u(i)
      enddo

      !-- finally the file should be closed to ensure all data is written and resources are freed --
      close(10)
    end subroutine write_results



    !----------------------------------------------------------------------------------------------
    ! function factorial(n)
    !
    ! This is an example function that computes the factorial of a non-negative integer 'n' using 
    ! recursion. Functions are used to compute and return a single value and generally should
    ! not have side effects (i.e. they should not modify their arguments).
    !
    ! Dummy Arguments:
    !   n - non-negative integer input
    ! Return value:
    !   factorial of n
    !----------------------------------------------------------------------------------------------
    function factorial(n) result(fact)
      !--------- dummy arguments and return value -------------
      integer, intent(in) :: n
      integer             :: fact
      !---------------- local variables -----------------------
      integer :: i
      !--------------------------------------------------------

      if (n < 0) then
        print *, 'Error: n must be a non-negative integer.'
        fact = -1 !<-- return -1 to indicate an error
      else if (n == 0) then
        fact = 1
        return !<-- return immediately since 0! is 1
      end if

      fact = 1
      do i = 1,n
        fact = fact * i
      enddo
    end function factorial
end module my_module_m