!write to screen

module write_to_screen
	contains
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

end module write_to_screen


