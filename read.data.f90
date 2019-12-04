program read_data
        implicit none
        real, allocatable, dimension (:,:) ::A
        real, allocatable, dimension (:)::b
        integer :: i,j, rank
        
        open (unit = 2, file = "A_1.dat")
        read(2,*) rank
        allocate (A(rank,rank))
        close(2)

        do i = 1,rank,1
                read(2,*) A(i,:)
                write(2,*) A(i,:)
        enddo
        
end program
