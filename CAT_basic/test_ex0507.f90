Program ex0507
    Implicit none
    Integer :: Grades
    Character (len = 1) :: Level
    Write(*,*) 'Please input your Grades:'
    Read(*,*) Grades
    Select case (Grades)
        Case (90:100)
            Level = 'A'
        Case (80:89)
            Level = 'B'
        Case (70:79)
            Level = 'C'
        Case (60:69)
            Level = 'D'
        Case (:59)
            Level = 'E'
        Case default
            Level = '?'
    End select
    write(*,*) 'You get : [' ,Level, ']'
    stop
end Program ex0507