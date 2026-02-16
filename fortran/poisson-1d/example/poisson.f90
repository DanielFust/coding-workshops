!--------------------------------------------------------------------------------------------------
! program poisson
!
! This program numerically solves the 1D Poisson equation using central difference discretization
! and Gaussian elimination.
!
! Boundary value problem: 
!    u'' = f(x)
!    u(0) = a, u(1) = b
!
! Fortran remains popular in scientific computing due in no small part to its built-in support
! for arrays and array operations. Arrays may be declared with any number of dimensions and may 
! either be a fixed size (specified at compile time) or dynamically allocated 
! (specified at runtime).
!-------------------------------------------------------------------------------------------------- 
program poisson
  use my_module_m, only: tridiagonal_solver, write_results, pi, WP !<-- import the solver and the value of pi from the module
  implicit none
  !---------- grid size ---------
  integer, parameter :: n = 100
  !-- These arrays are declared with a fixed size of 'n' --
  real(WP) :: x(n), u(n) 
  !-- arrays may also be declared as allocatable and allocated at runtime --
  real(WP), allocatable :: a(:), b(:), c(:), f(:)
  real(WP) :: dx
  integer  :: i
  !-------------------------------
  
  !-- allocate the arrays for the tridiagonal system --
  allocate(a(n), b(n), c(n), f(n)) 

  !-- Initialize the arrays --
  ! Note: some compilers may not initialize arrays or variables with a fixed value so it is
  !       usually good practice to assign a value to all elements of an array before using it.
  a(1:n) = 0.0d0
  b(1:n) = 0.0d0
  c(1:n) = 0.0d0
  f(1:n) = 0.0d0

  !-- set up the grid --
  dx = 1.0d0/(real(n,8)-1.0_WP) !<-- grid spacing
  do i = 1,n
    x(i) = (real(i,WP)-1.0d0)/(real(n,WP)-1.0_WP) !<-- grid points from 0 to 1
  enddo

  !-- set up the right-hand side --
  do i = 1,n
    f(i) = sin(2.0_WP*pi*x(i)) !<-- example source term
  enddo

  !-- set up the tridiagonal system --
  do i=1,n
    a(i) = -1.0_WP/dx**2
    b(i) =  2.0_WP/dx**2
    c(i) = -1.0_WP/dx**2
  enddo

  !-- apply Dirichlet boundary conditions --
  b(1) = 1.0_WP
  c(1) = 0.0_WP
  f(1) = 0.0_WP
  b(n) = 1.0_WP
  a(n) = 0.0_WP
  f(n) = 0.0_WP

  !-- call a subroutine to solve the tridiagonal system --
  call tridiagonal_solver(a, b, c, f, u)

  !-- write the results to a text file --
  call write_results('poisson_results.dat', x, u)
 
  !-- clean up the dynamic arrays --
  deallocate(a, b, c, f)
end program poisson