program readFile

    implicit none
    character(len=100) :: line
    integer :: ios, line_count = 0

    open (unit=12, file='input.txt', iostat=ios, status='old') 
    if (ios /= 0) then
        write(0,*) 'ERROR: Datei konnte nicht geöffnet werden!'
        stop
    end if

    write(6,*) 'Datei erfolgreich geöffnet.'
    do
        read(12,'(a)', iostat=ios) line
        ! Bei EOF wird ios /= 0 
        if (ios /= 0) exit
        write(*,'(a)') line
        line_count = line_count + 1
    end do
    close(12)
    print '(a11,i2)', 'linecount: ', line_count

end program readFile