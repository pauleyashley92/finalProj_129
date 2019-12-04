module mod1
 contains
#define PRINTINFO 1 


subroutine gaussian_elimination(A,b)
  real, allocatable, dimension(:,:), intent(INOUT) :: A
  real, allocatable, dimension(:), intent(INOUT) :: b
  integer :: i,j

  ! gaussian elimination
  do j = 1, 2           ! j is column
     do i = j+1, 3       ! i is row
  !do j = 1, 3           ! j is column
  !   do i = 1, 3       ! i is row
        factor = A(i,j)/A(1,1)
        A(i,:) = A(i,:) - factor*A(j,:)
        b(i) = b(i) - factor*b(j)
     end do
  end do
end subroutine gaussian_elimination


subroutine backsubstitution(A,b)
  real, allocatable, dimension(:,:), intent(INOUT) :: A
  real, allocatable, dimension(:), intent(INOUT) :: b
  integer :: i,j

  ! doing back substitution
  do j = 3, 2, -1            ! j is column
     do i = j-1, 1, -1        ! i is column
        factor = A(i,j)/A(j,j)
        A(i,:) = A(i,:) - factor*A(j,:)
        b(i) = b(i) - factor*b(j)
     end do
  end do

  ! overwrite the solution vector to b
  do i = 1, 3
     b(i) = b(i)/A(i,i)
  end do
end subroutine backsubstitution
end module mod1

program gaussian
  use mod1
  real, allocatable, dimension(:,:) :: A
  real, allocatable, dimension(:) :: b
  integer :: i, j

  ! initialize matrix A and vector b
  !changed the values in A to be correct
  A = reshape((/2., 4., 7., 3., 7., 10., -1., 1., -4., 1., 3., 4./),(/3,3/))
  b = (/1., 3., 4./)

#ifdef PRINTINFO
  ! print augmented matrix
  do i = 1, 3           ! i is row
     print *, A(i,:), "|", b(i)
  end do
#endif


#ifdef PRINTINFO  
  print *, ""    ! print a blank line
  print *, "Gaussian elimination........"
#endif
  call gaussian_elimination(A,b)

#ifdef PRINTINFO  
  ! print echelon form
  print *, "***********************"
  !do i = 1, 10
  do i = 1, 3
     print *,A(i,:), "|", b(i)
  end do

  print *,""    ! print a blank line
  print *,"back subs......"
#endif
  call backsubstitution(A,b)



#ifdef PRINTINFO  
  ! print the results
  print *,"***********************"
  !do i = 1, 10
  do i = 1, 3
     print *, A(i,:), "|", b(i)
  end do

  print *, ""
  print *,"The solution vector is;"
  !do i = 1, 10
  do i = 1, 3
     print *,b(i)
  end do
#endif

end program gaussian






