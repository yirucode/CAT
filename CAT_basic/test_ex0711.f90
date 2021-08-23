program ex0711
    implicit none
    INTEGER::i, j, k
    INTEGER:: a(3,3) = reshape((/(i,i=1,9)/), (/3,3/))
    INTEGER:: b(2,3) = reshape((/(i,i=1,6)/), (/2,3/)) 
    WRITE(*,*) 'a='
    do i=1,3
        WRITE(*,*) (a(i,k),k=1,3)
    end do
    WRITE(*,*) 'b='
    do j=1,2
        WRITE(*,*) (b(j,k),k=1,3)
    end do
    stop
end program ex0711


