Program ex0912
    ! 依選手的背號順序，輸入選手的打擊率
    implicit none
    character(len = 20), parameter :: input = 'Listdata'
    character(len = 20), parameter :: dataF = '(3F10.2)'
    integer :: i
    integer,parameter :: poolItem = 300
    real :: a(poolItem), b(poolItem), c(poolItem)
    ! 讀取資料：輸入試題參數
    open(100,file="data/parameter_300.txt",status="old") 
    do i=1, poolItem
        read(100,*) a(i),b(i),c(i) !三參數
    enddo
    open(10, file = input, form = 'formatted')
    ! i = 0
    ! do while (.true.)
    !     i = i + 1
    !     if (i > 50) exit
    !     write(10, fmt = dataF) a(i), b(i), c(i)
    ! end do
    do i=1, poolItem
        write(10, fmt = dataF) a(i), b(i), c(i)
    enddo

    stop
end program ex0912