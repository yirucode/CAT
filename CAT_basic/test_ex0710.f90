Program ex0710
    implicit none
    integer :: students, i
    integer :: error
    integer ,allocatable :: grades( : )
    ! === class A ===
    write(*,*) 'How many strdents in this class A ?'
    read(*,*) students
    allocate(grades(students),stat = error) !向電腦要求需要的記憶體空間
    IF (error .NE. 0) PRINT *,'Out of memory'
    do i = 1, students
        write(*, '(1X, A25, I2, A2)') 'Input grades of number', I, ':'
        read(*,*) grades(i)
    end do
    do i = 1, students
        if (grades(i) < 60) then
            write(*, '(1X, A10, I2, A8)') 'number', I, 'fail!'
        end if
    end do
    deallocate(grades,stat = error) !釋放陣列記憶體空間
    IF (error .NE. 0) PRINT *,'Out of memory'
    ! === class B ===
    write(*,*) 'How many strdents in this class B ?'
    read(*,*) students
    allocate(grades(students),stat = error) !重新向電腦要求需要的記憶體空間
    IF (error .NE. 0) PRINT *,'Out of memory'
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
end program ex0710