Program ex0910
    ! 依選手的背號順序，輸入選手的打擊率
    implicit none
    character(len = 20), parameter :: input = 'ListNew'
    integer, parameter :: players = 9, rec_length = 6
    integer :: player
    real :: hit_rate
    open(10, file = input, form = 'formatted', access = 'direct', &
    recl = rec_length)
    do while (.true.)
        write(*,*) 'Hit Number:'
        read(*,*) player
        if(player < 1 .or. player > players) exit
        write(*,*) 'Input hit rate:'
        read(*,*) hit_rate
        write(10, fmt = '(F4.2)', rec = player) hit_rate
    end do
    stop
end program ex0910