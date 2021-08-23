Program ex0709
    implicit none
    integer :: students, i
    integer ,allocatable :: grades( : )
    write(*,*) 'How many strdents in this class?'
    read(*,*) students
    allocate(grades(students))
    do i = 1, students
        write(*, '(1X, A25, I2, A2)') 'Input grades of number', I, ':'
        read(*,*) grades(i)
    end do
    do i = 1, students
        if (grades(i) < 60) then
            write(*, '(1X, A10, I2, A8)') 'number', I, 'fail!'
        end if
    end do
    stop
end program ex0709