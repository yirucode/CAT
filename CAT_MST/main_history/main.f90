program main
    implicit none
    
    integer,parameter :: col=10
    real :: a(col),b(col),c(col)
    INTEGER:: i


    open(100,file="data/parameter.txt",status="old") 

    do i=1,col
        read(100,*) a(i),b(i),c(i)
    enddo

    WRITE(*,*) (a(i),i=1,5)


end program main