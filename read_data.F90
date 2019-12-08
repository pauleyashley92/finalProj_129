! Final Project for AM129 , Fall 2019
! Written by Ashley Pauley and Kevin Lo

module set_up

implicit none
contains 

  !----------------------------------------!
  ! Set create NXN matrix
  ! N is the input
  ! NxN array is the return value
  !----------------------------------------!
  subroutine initMatrix (N,A)
    integer, intent (in) :: N
    real, dimension (:,:), allocatable, intent (out) :: A
    integer :: i

    allocate ( A(N,N) )

  return
  end subroutine initMatrix

  !----------------------------------------!
  ! Set create vector of length N
  ! N is the input
  ! N sized vector is the return value
  !----------------------------------------!
  subroutine initVector ( N, V )
    integer, intent (in) :: N
    real, dimension (:), allocatable, intent (out) :: V

    allocate ( V(N) )

  return
  end subroutine initVector

  !----------------------------------------!
  ! Print Matrix
  ! M is the matrix
  ! N is the rank
  !----------------------------------------!
  subroutine printMatrix(M,N)

    integer, intent (in) :: N
    real, dimension (:,:), allocatable, intent (in) :: M
    integer :: i
    do i=1, N
      print *,  M(i,:) 
    end do

  end subroutine printMatrix

  !----------------------------------------!
  ! Print Vector
  ! V is the vector
  ! N is the size
  !----------------------------------------!
  subroutine printVector(V,N)

    integer, intent (in) :: N
    real, dimension (:), allocatable, intent (in) :: V
    integer :: i
    do i=1, N
      print *,  V(i) 
    end do

  end subroutine printVector

  !----------------------------------------!
  ! Print Augmented Matrix
  ! A is the matrix
  ! b is vedtor to augment with
  ! N is the rank
  !----------------------------------------!
  subroutine printAugmented(A,b,N)

    real, dimension (:,:), allocatable, intent (in) :: A
    real, dimension (:), allocatable, intent (in) :: B
    integer, intent (in) :: N
    integer :: i
    do i = 1, N           ! i is row
      print *, A(i,:), "|", b(i)
    end do

  end subroutine printAugmented

  !----------------------------------------!
  ! Gaussian Elimination
  ! A is the matrix
  ! b is vector to augment with
  ! N is the rank
  !----------------------------------------!
  subroutine gaussian_elimination(A,b,N)
    real, allocatable, dimension(:,:), intent(INOUT) :: A
    real, allocatable, dimension(:), intent(INOUT) :: b
    integer, intent (in) :: N
    integer :: i,j
    real :: factor

    ! gaussian elimination
    do j = 1, N-1           ! j is column
       do i = j+1, N       ! i is row
          factor = A(i,j)/A(1,1)
          A(i,:) = A(i,:) - factor*A(j,:)
          b(i) = b(i) - factor*b(j)
       end do
    end do
end subroutine gaussian_elimination


!----------------------------------------!
! Back Substitution
! A is the matrix
! b is vector to augment with
! N is the rank
!----------------------------------------!
subroutine backsubstitution(A,b,N)
  real, allocatable, dimension(:,:), intent(INOUT) :: A
  real, allocatable, dimension(:), intent(INOUT) :: b
  integer, intent (in) :: N
  integer :: i,j
  real :: factor

  ! doing back substitution
  do j = N, (N-1), -1            ! j is column
     do i = j-1, 1, -1        ! i is column
        factor = A(i,j)/A(j,j)
        A(i,:) = A(i,:) - factor*A(j,:)
        b(i) = b(i) - factor*b(j)
     end do
  end do

  ! overwrite the solution vector to b
  do i = 1, N
     b(i) = b(i)/A(i,i)
  end do
end subroutine backsubstitution

subroutine decomposeL(A,N)
  real, allocatable, dimension(:,:), intent(INOUT) :: A
  integer, intent (in) :: N
  integer :: i,j
  real :: factor
  do j = 1, N
      do i = j+1,N
           factor = -A(i,j)/A(j,j)
           A(i,j) = factor
      enddo
  enddo
  !print*, "inside decomposeA"
end subroutine decomposeL

subroutine decomposeU(A,N)
  real, allocatable, dimension(:,:), intent(INOUT) :: A
  integer, intent (in) :: N
  integer :: i,j
  real :: factor
   do j = N, (N-1), -1            ! j is column
     do i = j-1, 1, -1        ! i is column
        factor = -A(i,j)/A(j,j)
        A(j,i) = factor
     end do
  end do
end subroutine decomposeU
end module set_up


program read_data
  use set_up
  implicit none

  !dynamic array and vector, size determined at runtime
  real, dimension (:,:), allocatable :: A
  real, dimension (:), allocatable :: row, b,row2

  !integer variables to hold rank
  integer :: i,j, rank1, rank2, n, m ,o, A_size,A_size2

  !Open the dat file 
  open (2, file = "A_1.dat")
  open(3,file = "B_1.dat")

  !Read the first line to get the rank
  read(2,*) rank1
  read(3,*) rank2

  !Build the A matrix and the b vector with the ranks
  call initMatrix (rank1,A)
  call initVector (rank2,b)

  !Use the remainder of the files to initalize the matrix and vector
 
  !populate Matrix 

  !make a vector for each row of A
  A_size = (rank1 * rank1)
  call initVector (rank1,row)


  !Read the rest of the file line by line
  !Each line is a row of A
  !Set each row of A
  !print each row for debugging
  do i=1, rank1
    read (2, *) row
    A(i,:) = row
  end do

  !populate Vector

  !Read the rest of the file line by line
  !Each line is entry in b
  !Set each entry of b
  !print each entry for debugging
  do i=1, rank2
    read (3, *) n
    b(i) = n
  end do

  !close the files 
  close(2)
  close(3)
  
  !Print the matrix and vector to stdout
  print *, " ", "Matrix A .."
  call printMatrix(A, rank1)
  print *, " ", "Vector b .."
  call printVector(b, rank2)
  print *, " ", "Augmented A with b.."
  call printAugmented(A, b, rank1)

  !Do Gaussian Elimination
  print *, "Gaussian elimination .."
  call gaussian_elimination(A,b, rank1)
  call printAugmented(A, b, rank1)

  !Do Back Substitution
  print *, "Back Substitution .."
  call backsubstitution(A,b,rank1)

  call printAugmented(A,b,rank1)
  print *, " "

  deallocate(A) 
  print *, "Gaussian elimination without Partial Pivoting .."
  open (4, file = "A_1.dat")
  read(4,*) rank1
  print*, rank1
  call initMatrix (rank1,A)
   !make a vector for each row of A
  A_size2 = (rank1 * rank1)
  call initVector (rank1,row2)


  !Read the rest of the file line by line
  !Each line is a row of A
  !Set each row of A
  !print each row for debugging
  do i=1, rank1
    read (4, *) row2
    A(i,:) = row2
  end do

  call printMatrix(A,rank1)
  print*, 

  call decomposeL(A,rank1)
  call printMatrix(A,rank1) 
  print*, 
  call decomposeU(A,rank1)
  call printMatrix(A,rank1)
  !deallocate memory for array and matrix
  deallocate (row)
  deallocate (A)
  deallocate (b)   

  stop
end program read_data
   
