Program ex0911
    ! 二進位檔：肉眼無法明白了解之亂碼檔，輸出的檔案無法看懂。
    ! 此程式是把輸入棒球選手打擊率的程式，改成使用二進位檔來運作的範例。
    ! form 設為 'unformatted'
    Implicit none
    character(len = 20), parameter :: output = 'List.bin'
    integer, parameter :: players = 9, rec_length = 4 !hit_rate 是單精度
    integer :: player 
    real :: hit_rate
    open(10, file = output, form = 'unformatted', access = 'direct', &
    recl = rec_length)
    do while (.true.)
        write(*,*) 'Hit Number :'
        read(*,*) player
        if (player < 1 .or. player > players) exit
        read(*,*) hit_rate
        write(10, rec = player) hit_rate
    end do
    stop
end program ex0911

! 可節省儲存的空間，儲存單精度的浮點數只需4個bytes。
! 若以文字檔來儲存同樣精準度的浮點數，其所需之byte 遠超過4個bytes。
! 因此，如果要存放 精確 及 大量 的資料時，使用二進位檔案是比較好的選擇。