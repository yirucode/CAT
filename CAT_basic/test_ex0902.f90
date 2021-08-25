Program read
    ! 讀取指定的檔案資料，並呈現出來
    implicit none
    character (len =20) :: filename
    integer :: nvals = 0
    integer :: status 
    !status > 0 表示讀取動作發生錯誤
    !status = 0 表示讀取動作正常
    !status < 0 表示檔案終了
    real :: value
    ! Get the file name and echo it back to the user.
    write(*,*) 'Please enter input file name:'
    filename = 'ListData'
    !read(*,*) filename
    write(*,10) filename
    10 format(1x, 'The input file name is : ', A)
    ! Open the file and check for errors on open
    open(3, file = filename, status = 'old', action = 'read', iostat = status)
    openif: If(status == 0) then
        ! open was OK. Read values
        readloop: do
            read(3, *, iostat = status) value
            if(status /= 0) Exit
            nvals = nvals + 1
            write(*, 20) nvals, value
            20 format(1x, 'Line', I6, ': Value = ', F10.4)
        end do readloop

        ! The while loop has terminated. Was it because of a read error or because
        ! of the end of the input file?
        readif: if(status > 0) then
            write(*, 30) nvals + 1
            30 format(1x, 'An error occurred reading line', I6)
        else
            write(*, 40) nvals
            40 format(1x, 'End of file reached. There were ', I6, ' values in the file.')
        end if readif
    else openif
            write(*, 50) status
            50 format(1x, 'Error opening file: IOSTAT = ',I6)
    end if openif
    close(3)
end program read