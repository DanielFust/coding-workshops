!--------------------------------------------------------------------------------------------------
! program loops_and_conditionals
!
! This program demonstrates the use of loops and conditionals in Fortran as well as basics such as
! variable declaration and program structure.
!--------------------------------------------------------------------------------------------------
program loops_and_conditionals
  implicit none !<-- disable implicit typing

  !-- variables and their types must be declared before they are used --
  integer :: i, iter
  real(8) :: x, res !<-- 'real(8)' is a double precision floating point type

  !-- The 'parameter' attribute is used to declare a compile-time constant --
  integer, parameter :: n = 10
  integer, parameter :: iter_max = 100
  real(8), parameter :: tol = 1.0D-06 !<-- 'd' indicates double precision for literals
  
  !-- 'logical' is the boolean type in Fortran --
  logical :: is_positive
  logical, parameter :: two_plus_two_is_four = .true. !<-- logical constants are '.true.' and '.false.'
  logical, parameter :: two_plus_two_is_five = .false.

  !==================== 'do' Loops ========================
  !
  ! 'do' loops are used for iteration in Fortran
  ! and are similar to 'for' loops in other languages. 
  ! The syntax is:
  ! 
  ! do iterator = start, end, step
  !   ! loop body
  ! enddo
  !
  ! The 'iterator' will take on values from 'start' to 'end' 
  ! (including 'start' and 'end') in increments of 'step'. 
  ! If 'step' is omitted, it defaults to 1.
  !--------------------------------------------------------

  !-- A simple loop that iterates from 1 to n --
  print *, 'Simple loop from 1 to', n, ':'
  do i = 1,n
    print *, 'Iteration:', i
  enddo

  !-- A strided loop that iterates from 1 to n with a step of 2 --
  print *, 'Strided loop (step of 2):'
  do i = 1, n, 2
    print *, 'Iteration:', i
  enddo

  !-- A loop with a negative step that iterates from n down to 1 --
  print *, 'Loop with negative step (from n down to 1):'
  do i = n,1,-1
    print *, 'Iteration:', i
  enddo


  !==================== Conditionals ========================
  !
  ! Fortran uses 'if' statements for conditional logic. These
  ! function similarly to 'if' statements in other languages.
  ! The syntax is:
  !
  ! if (condition) then
  !   ! code to execute if condition is true
  ! else if (another_condition) then
  !   ! code to execute if another_condition is true
  ! else
  !   ! code to execute if all conditions are false
  ! end if
  !
  ! Notes:
  !   - There may be any number of 'else if' branches, but
  !     at most one 'else' branch.
  !   - The keyword 'then' is required after the condition 
  !     in an 'if' statement if it is terminated by an 
  !     'endif'.
  !   - The 'else' branch is optional. An 'if' statement
  !     can be used without 'then' or 'endif' if
  !     and only if it is a single statement.
  !--------------------------------------------------------

  !-- Check if x is positive, negative, or zero --
  x = -3.5d0
  if (x > 0.0d0) then
    is_positive = .true.
    print *, 'x is positive.'
  else if (x < 0.0d0) then
    is_positive = .false.
    print *, 'x is negative.'
  else
    is_positive = .false.
    print *, 'x is zero.'
  end if

  !============== Logical and relational Operators ==============
  !
  ! For comparison of 'logical' types and statements:
  !    - .and.    (logical AND)
  !    - .or.     (logical OR)
  !    - .not.    (logical NOT, no symbolic equivalent
  !                   in standard Fortran)
  !    - .eqv.    (logical equivalence)
  !    - .neqv.   (logical non-equivalence)
  !
  ! Some extensions/compilers allow the use of symbolic logical
  ! operators (&&, ||), but these are not part of the Fortran 
  ! standard and may not be portable.
  !
  ! For comparison of numeric types:
  !    - .eq.     == (logical equality)
  !    - .ne.     /= (logical inequality)
  !    - .gt.     >  (greater than)
  !    - .lt.     <  (less than)
  !    - .ge.     >= (greater than or equal to)
  !    - .le.     <= (less than or equal to)
  !
  ! From Fortran 90 onward relational operators can use either
  ! symbolic or word-based forms. Legacy Fortran 77 only 
  ! supports the word-based forms.
  !============================================================




  !=================== 'do while' Loops =======================
  !
  ! 'do while' loops are repeatedly execute the loop body until
  ! the specified condition evaluates to false. The condition
  ! is checked before each iteration.
  !
  ! The syntax is:
  ! do while (condition)
  !   ! loop body
  ! end do
  !
  ! Note: 'do while' loops are not available in Fortran 77 
  ! and earlier.
  !------------------------------------------------------------

  !-- Use a 'do while' loop to solve x^2 = 0 with fixed point iteration --
  x    = 0.0d0              ! initial guess
  res  = sin(x) + 0.5d0 - x ! initial residual
  iter = 0 !<-- iteration counter
  print *, 'Solving sin(x) + 0.5 = x using fixed point iteration:'
  do while (abs(res) > tol)
    iter = iter + 1

    !-- check for maximum iterations to prevent infinite loops --
    if (iter > iter_max) then
      print *, 'Maximum iterations reached without convergence.'
      exit !<-- exit the loop if maximum iterations is exceeded
    end if

    !-- compute next guess and residual --
    x   = sin(x) + 0.5d0
    res = sin(x) + 0.5d0 - x

    !-- report progress --
    print *, 'Iteration:', iter, 'Current residual:', res, 'Current guess:', x
  end do
  print *, 'Converged after', iter, 'iterations. Final guess:', x, 'Final residual:', res
  

end program loops_and_conditionals