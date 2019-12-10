!ge_elim.f90
! Final Project for AM129 , Fall 2019
! Written by Ashley Pauley and Kevin Lo

module ge_elim
	contains 

		!----------------------------------------!
	    ! Gaussian Elimination with Pivoting
	    ! A is the matrix
	    ! b is vector to augment with
	    ! N is the rank
	    !----------------------------------------!
	    subroutine gaussian_elimination(A,b,N)
	      real, allocatable, dimension(:,:), intent(INOUT) :: A
	      real, allocatable, dimension(:), intent(INOUT) :: b
	      real, allocatable, dimension(:) :: Col
	      integer, intent (in) :: N
	      integer :: i,j, Max
	      real :: factor, temp

	      allocate ( Col(N) )

	      do j=1, (N-1)
	        !print *,"Elim loop: ", j, " "
	        !call printAugmented(A,b,N)
	        print *, " "
	        !Col = A(:,j)
	        call findMaxInColumn(Col,j,N,Max)
	        if (Max /= j) then
	            !interchange row K and j
	            !print *, "Swap the rows so max is on top.."
	            
	            Col = A(j, :)
	            A(j, :) = A(Max, :)
	            A(Max, :) = Col

	            temp = b(j)
	            b(j) = b(Max)
	            b(Max) = temp

	            !print *,"Aug Matrix A .."
	            !call printAugmented(A,b,N)
	        end if
	        if ( A(j,j) == 0 ) then
	          stop
	        end if
	        do i = j+1, N
	           factor = A(i,j)/A(j,j)
	           A(i,:) = A(i,:) - factor*A(j,:)
	           b(i) = b(i) - factor*b(j)
	        end do
	      end do

	      deallocate( Col )

	  end subroutine gaussian_elimination

	  	!----------------------------------------!
	    ! Standard Gaussian Elimination 
	    ! A is the matrix
	    ! b is vector to augment with
	    ! N is the rank
	    !----------------------------------------!
	    subroutine stdgaussian_elimination(A,b,N)
	      real, allocatable, dimension(:,:), intent(inout) :: A
	      real, allocatable, dimension(:), intent(inout) :: b
	      integer, intent (in) :: N
	      integer :: i,j
	      real :: factor

	      ! gaussian elimination
	      do j = 1, N-1           ! j is column
	         do i = j+1, N        ! i is row
	            factor = A(i,j)/A(j,j)
	            A(i,:) = A(i,:) - factor*A(j,:)
	            b(i) = b(i) - factor*b(j)
	         end do
	      end do

	  end subroutine stdgaussian_elimination

	  !----------------------------------------!
	  ! Find Max Column 
	  ! looks through the column of A
	  ! return the index of the row with the 
	  ! largest absolute value
	  !----------------------------------------!

	  subroutine findMaxInColumn(Col,index,N,Max)
	    real, allocatable, dimension(:), intent(in) :: Col
	    integer, intent (in) :: N, index
	    integer, intent(out) :: Max
	    integer :: i

	    !print *, "Column.. "
	    !print *, Col
	    !print *, " "
	    
	    Max = index
	    do i=(index+1), N
	      !print *, "Looking at element: ", Col(i)
	      !print *, " "
	      if( abs(Col(i)) > abs(Col(Max)) ) then
	        !print *, "Element was larger than max."
	        !print *, " "
	        Max = i
	        !print *, "Max in column: ", Col(i) , "At index: ", Max
	        !print *, " "
	      end if
	    end do

	    return
	  end subroutine findMaxInColumn

	  !----------------------------------------!
	  ! L Decomposition
	  ! A is the matrix
	  ! b is vector to augment with
	  ! N is the rank
	  !----------------------------------------!
	  subroutine decomp(A,b,N,M)
	  	  use read_data
	      real, allocatable, dimension(:,:), intent(inout) :: A, M
	      real, allocatable, dimension(:,:) :: ID, C
	      real, allocatable, dimension(:), intent(inout) :: b
	      real, allocatable, dimension(:):: b_temp
	      integer, intent (in) :: N
	      integer :: i,j,k, l
	      real :: factor

	      call initMatrix(N,ID)
	      call initMatrix(N,C)
	      call initVector(N,b_temp)

	      do j = 1, N         ! j is column
	        C(j,:) = 0        ! Set C to all zeros
	        ID(j,:) = 0       ! Identity Matix zeros
	        ID(j,j) = 1       ! Identity matrix diagonal
	      end do

	      do j = 1, N-1         ! j is column
	        M = ID
	        do i = j+1, N
	          M(i,j) = - A(i,j)/A(j,j)
	        end do
	        C = matmul(M, A)
	        b = matmul(M, b)
	        A = C
	      end do
	      M = A
	  
	  end subroutine decomp

end module ge_elim


