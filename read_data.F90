program read_data

        implicit none
        integer :: i,j, rank1, rank2
        !Open the dat file
        open (2, file = "A_1.dat")
        !Read the file then store into an array A and remember rank
        read(2,*) rank1
        close(2)
        print * ,rank1
        open(3,file = "B_1.dat")
        read(3,*) rank2
        close(3)
        call printMatrix1(rank1,rank1)
        print *,
        print *,rank2
        call printMatrix2(rank2)

end program read_data

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
       print *, array(n)
   enddo
   close (16)
end subroutine printMatrix2
