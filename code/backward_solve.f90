! backward_solve.f90
! Final Project for AM129 , Fall 2019
! Written by Ashley Pauley and Kevin Lo

module backward_solve
	contains	

	  !----------------------------------------!
	  ! Backsubstitution
	  !----------------------------------------!
	  subroutine backsub(U,X,Y,N)
	      real, allocatable, dimension(:,:), intent(inout) :: U
	      real, allocatable, dimension(:), intent(inout) :: X,Y
	      integer, intent (in) :: N
	      integer :: i,k
	      real :: sum

	      !print*,"printing U"
	      !print*,U(1,:)
	      !print*,U(2,:)
	      !print*,U(3,:)
	      !print*,U(4,:)

	      !print*,"+++++++++++++++++++"
	      !print*,"printing y"
	      !print*,y(:)
	      !print*,"+++++++++++++++++++"	      


	      !print*,N
	      X(N) = Y(N)/U(N,N)
	      !print*, X(N), Y(N)

	      !stop

	      do i = N-1, 1, -1        ! j is column
	        if (U(i,i) == 0) then
	          stop
	        end if
	        sum = 0.0
	        do k = (i + 1), N
	          sum = (sum + U(i,k)*X(k))
	        end do
	        X(i) = (Y(i)- sum)/U(i,i)
	      end do
	     
	  end subroutine backsub

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
	    real :: factor, s

	    !doing back substitution
	    do j = N, (N-1), -1            ! j is column
	       do i = j-1, 1, -1        ! i is row
	          factor = A(i,j)/A(j,j)
	          A(i,:) = A(i,:) - factor*A(j,:)
	          b(i) = b(i) - factor*b(j)
	       end do
	    end do

	    ! overwrite the solution vector to b
	    do i = 1, N
	       b(i) = b(i)/A(i,i)
	       A(i,i) = A(i,i)/A(i,i)
	    end do

	  end subroutine backsubstitution

end module backward_solve
