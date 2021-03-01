program x_and_y

    implicit none
    integer :: i, x = 0, y = 0

    do i = 1, 25
        x = x + 1
        if (mod(i,5) == 0) cycle
        y = y + 1
    enddo

    print *, x
    print *, y
    
end program x_and_y