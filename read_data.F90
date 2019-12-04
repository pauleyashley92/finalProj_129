program read_data
        implicit none
        real, allocatable, dimension (:,:) :: A
        real, allocatable, dimension (:):: b
        integer :: i,j, rank
        !Open the dat file
        open (2, file = "A_1.dat")
        !Read the file then store into an array A and remember rank
        read(2,*) A 
        call printMatrix(A, 3,3)
end program read_data

subroutine printMatrix(array, n, m)

implicit none
  real, intent(in) :: array(n,m)
  integer, intent(in) :: n,m
integer :: i
  do i = 1,n
     print*, array(i,:)
  end do
end subroutine printMatrix

