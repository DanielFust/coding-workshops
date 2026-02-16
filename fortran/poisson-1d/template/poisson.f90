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
  !-- import the module the contains the subprograms and variables we need --
  implicit none !<-- always remember to use 'implicit none'
  !---------- declare the grid size as a fixed constant ---------
  
  !-- declare the arrays needed to store the grid, solution, RHS, and tridiagonal matrix coefficients --

  !-- declare any other variables needed --
  
  !-------------------------------
  

  !-- initialize (and allocate if necessary) the arrays --


  !-- set up a uniform grid from 0 to 1 and determine the grid spacing or 'dx' value --


  !-- set up the right-hand side forcing function to the Poisson problem --


  !-- Discretize the Laplace operator using central finite difference to obtain tridiagonal matrix coefficients  --
  

  !-- apply Dirichlet boundary conditions to the matrix coefficients and --
  

  !-- call a subroutine to solve the tridiagonal system --

  !-- write the results to a text file --
  call write_results('poisson_results.dat', x, u)
 
  !-- clean up the dynamic arrays if necessary --
  
end program poisson