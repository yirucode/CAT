Program ex0909
    ! 由棒次來查尋打者打擊率
    implicit none
    character(len = 20), parameter :: input = 'List'
    integer, parameter :: players = 9
    integer :: player
    integer, parameter :: rec_length = 6
    real :: hit_rate
    open(10, file = input, form = 'formatted', access = 'direct', &
    recl = rec_length)
    do while (.true.)
        write(*,*) 'Number:'
        read(*,*) player
        if(player < 1 .or. player > players) exit !讀取檔案位置
        read(10, fmt = '(F4.2)', rec = player) hit_rate
        write(*, 100) 'Number ', player, 'hit_rate = ', hit_rate
        100 format(1X, A8, I2, A10, F5.2)
    enddo
    stop
end program ex0909

! (1) 開啟直接讀取檔時，open 敘述中的 access = 'direct'、及 'recl' 後的數值都不能省略。
! 這個數值是用來切分出檔案區塊大小使用的。
! (2) 在 DOS 作業系統中，文件檔中每一行的行尾都有兩個看不見的符號用來代表一行文字的結束。
! 所以真正一行的長度就是 "一行文字字元的數量再加上2"
! e.q. 在List 檔中每行長度 = 4 + 2 = 6
! 在unix 中，每一行的行尾只需一個結束符號，所以一行的長度就是 "一行文字字元的數量再加1"