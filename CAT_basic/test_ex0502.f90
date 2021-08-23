Program ex0502
    Implicit none
    Integer :: Grades
    Character(len=1) :: Level = '?'
    Write(*,*) 'Please input your Grades:'
    Read(*,*) Grades
    If ((Grades .LE. 100) .AND. (Grades .GE. 90)) then
        Level = 'A'
    else If ((Grades .LE. 89) .AND. (Grades .GE. 80)) then
        Level = 'B'
    else If ((Grades .LE. 79) .AND. (Grades .GE. 70)) then
        Level = 'C'
    else If ((Grades .LE. 69) .AND. (Grades .GE. 60)) then
        Level = 'D'
    else if (Grades .LT. 60) then
        Level = 'E'
    else
        write(*,*) 'Input error'
    end if
    write(*,*) 'You get : [' ,Level, ']'
    stop
end program ex0502