module set_up

implicit none
contains 

  !----------------------------------------!
  ! Set create NXN matrix
  ! N is the input
  ! NxN array is the return value
  !----------------------------------------!
  subroutine initMatrix ( N, A )
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

end module set_up


subroutine printMatrix1(n, m)

implicit none
  integer, intent(in) :: n,m
  real, dimension(n,m) :: array
  integer :: i
  open (15, file = "A_1.dat")
  read(15,*)
  read(15,*) array 
  do i = 1,n
     print *, array(i,:)
  end do
  close(15)
  
end subroutine printMatrix1

subroutine printMatrix2(n)

implicit none
   integer, intent(in) :: n
   real, dimension(n) :: array
   integer :: i
   open (16, file = "B_1.dat")
   read(16,*)
   read(16,*) array
   do i =1,n
       print *, array(i)
   enddo
   close (16)
end subroutine printMatrix2

function gaussianElim(A,b)
end function gaussianElim 




program read_data
  use set_up
  implicit none

  !dynamic array and vector, size determined at runtime
  real, dimension (:,:), allocatable :: array
  real, dimension (:), allocatable :: vector
  
  !integer variables to hold rank
  integer :: i,j, rank1, rank2

  !Open the dat file 
  open (2, file = "A_1.dat")
  open(3,file = "B_1.dat")

  !Read the first line to get the rank
  read(2,*) rank1
  read(3,*) rank2

  !Build the A matrix and the b vector with the ranks
  call initMatrix (rank1,array)
  call initVector (rank2,vector)

  !Use the remainder of the files to initalize the matrix and vector
  !do something
  !do something

  !close the files 
  close(2)
  close(3)
  
  !Print the matrix and vector to stdout
  call printMatrix1(rank1,rank1)
  print *, " "
  call printMatrix2(rank2)

  !deallocate memory for array and matrix
  deallocate (array)
  deallocate (vector)  

  stop
end program read_data
   
