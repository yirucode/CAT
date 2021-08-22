subroutine subr_max(rawData,dataLength,maxv,place)
    ! input data: rawData, length
    ! output data: maxv,place
    integer:: i,dataLength,place
    real::rawData(length),maxv
    
    maxv=rawData(1) !紀錄最大值
    place=1 !紀錄相對位置
    do i=2,length 
        if(maxv < rawData(i))then
            maxv = rawData(i)
            place = i
        else
            maxv = maxv
            place = place
        end if
    end do
    
    return
    end