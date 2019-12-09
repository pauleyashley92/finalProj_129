! read_data.f90
! Final Project for AM129 , Fall 2019
! Written by Ashley Pauley and Kevin Lo

module read_data
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


end module read_data

   
