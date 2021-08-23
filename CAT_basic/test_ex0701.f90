program ex0701
    implicit none
    INTEGER:: i
    INTEGER:: a(5) = (/0,(i,i=2,4),6/) ! →(0, 2, 3, 4, 6)
    WRITE(*,*) a
    a(1:5) = a(5:1:-1)  ! →(6, 4, 3, 2, 0)
    WRITE(*,*) a
    a(1:5:2) = 3
    WRITE(*,*) a(1:5:2) ! →a(1)=3、a(3)=3、a(5)=3
    stop
end program ex0701


