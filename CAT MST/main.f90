program main
    implicit none
    
    integer,parameter :: length=40, col=300, row=10000 !題長, 題庫數, 嘗試人數
    integer,parameter :: stage=2, n_level=3, n_module=5, n_item=20 !level數, level內module數, module內題數

    integer,parameter :: n_iteminlevel=100 !n_module * n_item
    integer,parameter :: n_moduleinpool=15 !n_module * n_level

    integer :: level_go = 2
    integer :: stage_n

    integer :: id_num(col), level(col), content(col), modulev(col)
    real :: a(col),b(col),c(col)
    real::a_choose(row,length),b_choose(row,length),c_choose(row,length) !選題的試題參數

    INTEGER:: i

    open(100,file="data/parameter_MST.txt",status="old") 

    do i=1,col
        read(100,*) id_num(i),a(i),b(i),c(i),content(i),level(i), modulev(i)
    enddo

    WRITE(*,*) (a(i),i=1,10)


end program main