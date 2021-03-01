program writeFile

    implicit none
    character(len=20) :: name
    integer :: age

    write (6,'(a)',advance='no') 'Please enter your name : '
    read  (5,'(a)') name
    write (6,'(a)',advance='no') 'Please enter your age : '
    read  (5,'(i4)') age

    open (unit=12, file='output.txt') 
    write(12,'(a,i4)') name, age
    close(12)
    
end program writeFile