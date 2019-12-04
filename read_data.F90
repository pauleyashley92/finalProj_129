program read_data
        implicit none
        real, allocatable, dimension (:,:) ::A
        real, allocatable, dimension (:)::b
        integer :: i,j, rank

        open (unit = 2, file = "output.txt")
        read(2,*) rank
        allocate (A(rank,rank))
        close(2)

        do i = 1,rank,1
                read(3,*) A(i,:)
                print *, 'A= ', A
        enddo

end program
