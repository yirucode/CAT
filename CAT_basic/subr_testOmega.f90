    !-----------------------
    ! Omega subroutine
    !-----------------------
    subroutine subr_testOmega(Omega,itemUsed,row,col,length,try,gamma) !(item,row,length,num,overlap)
        implicit none
        ! === output ===
        real, intent(out), dimension(row)::Omega
        ! === input data ===
        integer, intent(in) ::row, col
        integer, intent(in) ::length, try
        integer, intent(in) ::gamma
        integer, intent(in), dimension(row,col)::itemUsed !col = 題庫數、row = 人數
        ! === local variable ===
        real, external :: combination
        integer::i !計算用
        real::sumv !計算用
        ! === run code ===
        sumv=0.0
        do i=1,col
            sumv = sumv + (itemUsed(try,i)-itemUsed(try-1,i)) * combination(try-itemUsed(try,i),gamma)
        enddo
        if (try<=gamma)then
            Omega(try)=0
        else
            Omega(try) = REAL(try-gamma-1)/try * Omega(try-1) + REAL(gamma+1)/try &
            - sumv / (length * combination(try,gamma+1))
        endif
        return
    end subroutine subr_testOmega