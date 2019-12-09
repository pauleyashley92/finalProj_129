! linear_solve.f90
! Final Project for AM129 , Fall 2019
! Written by Ashley Pauley and Kevin Lo

module linear_solve
	contains
		subroutine l_solve(A_filename, B_filename)
		  use read_data
		  use backward_solve
		  use ge_elim
		  use write_to_screen
		  implicit none

		  !dynamic array and vector, size determined at runtime
		  real, dimension (:,:), allocatable :: A , A_copy, Ma
		  real, dimension (:), allocatable :: row, b,row2, b_copy, X
		  character(len = 7), intent(in) :: A_filename, B_filename

		  !integer variables to hold rank
		  integer :: i,j, rank1, rank2, n, m

		  !Open the dat file 
		  !open (2, file = "A_1.dat")
		  !open(3,file = "B_1.dat")
		   open (2, file = A_filename)
		   open(3,file = B_filename)

		  !Read the first line to get the rank
		  read(2,*) rank1
		  read(3,*) rank2

		  !Build the A matrix and the b vector with the ranks
		  call initMatrix (rank1,A)
		  call initMatrix (rank1,A_copy)
		  call initVector (rank2,b)
		  call initVector (rank2,b_copy)

		  !Use the remainder of the files to initalize the matrix and vector
		 
		  !populate Matrix 

		  !make a vector for each row of A
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
		    !print*,"printing n from lne 59", n
		    b(i) = n
		  end do

		  !close the files 
		  close(2)
		  close(3)
		  
		  !Make copies for part 2
		  A_copy = A
		  b_copy = b

		  print *, " ", "Augmented A with b.."
		  call printAugmented(A, b, rank1)

		  !Do Gaussian Elimination
		  print *, "Gaussian elimination with Partial Pivoting.."
		  call gaussian_elimination(A,b,rank1)
		  call printAugmented(A, b, rank1)
		  print *, " "

		  !Do Back Substitution
		  print *, "Back Substitution .."
		  call backsubstitution(A,b,rank1)
		  call printAugmented(A,b,rank1)
		  print *, " "

		  !-----------------------------! Part 2 LU decomp

		  call initMatrix (rank1, Ma)
		  call decomp(A_copy,b_copy,rank1,Ma)
		  print *, "U: "
		  call printMatrix(Ma,rank1) 
		  print *, " "
		  print *, "y: "
		  call printVector(b_copy,rank1) 
		  print *, " "

		  !U
		  call initVector(rank1, X)
		  print *, "x:  "
		  call backsub(Ma,X,b_copy, rank1)
		  call printVector(X,rank1)
		  print *, " "

		  !deallocate memory for arrays and matrices
		  deallocate (row)
		  deallocate (A)
		  deallocate (Ma)
		  deallocate (b)
		  deallocate (A_copy)
		  deallocate (b_copy)    
	end subroutine l_solve

end module linear_solve


!-----------------------------! Main
program main
	use linear_solve

	print *, "----------------------------------------"
	print *, " A_1.dat & B_1.dat"
	print *, " "
	call l_solve("A_1.dat", "B_1.dat")
	print *, "----------------------------------------"
	print *, " "

	print *, "----------------------------------------"
	print *, " A_2.dat & B_2.dat"
	print *, " "
	call l_solve("A_2.dat", "B_2.dat")
	print *, "----------------------------------------"
	print *, " "

	print *, "----------------------------------------"
	print *, " A_3.dat & B_3.dat"
	print *, " "
	call l_solve("A_3.dat", "B_3.dat")
	print *, "----------------------------------------"
	print *, " "

	print *, "----------------------------------------"
	print *, " A_4.dat & B_4.dat"
	print *, " "
	call l_solve("A_4.dat", "B_4.dat")
	print *, "----------------------------------------"
	print *, " "



end program main
