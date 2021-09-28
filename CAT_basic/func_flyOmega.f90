    !-----------------------
    ! Omega fly function
    !-----------------------

    real function func_flyOmega(try,alpha,omegaMax,omegaPrevious,length) !(item,row,length,num,overlap)
    implicit none
    ! local value
    real, external :: combination
    ! input data
    real, intent(in)::omegaMax, omegaPrevious
    integer, intent(in)::try
    integer, intent(in)::alpha
    integer, intent(in)::length
    func_flyOmega = length * combination(try, alpha+1) * &
    (((try-alpha-1)/try) * omegaPrevious + ((alpha+1)/try) - omegaMax) 
    return
    end
    
    
    !-----------------------
    ! Omega subroutine
    !-----------------------
    subroutine subr_testOmega(numTest,numPool,alpha,itemUsed,length,try,Omega)
        implicit none
        ! === output ===
        real, intent(out), dimension(numTest)::Omega
        ! === input data ===
        integer, intent(in) ::numTest, numPool
        integer, intent(in) ::length, try
        integer, intent(in) ::alpha
        integer, intent(in), dimension(numPool,numTest)::itemUsed !numPool = 題庫數、numTest = 人數
        ! === local variable ===
        real, external :: combination
        integer, save::i !計算用
        real, save::sumv !計算用
        ! === run code ===
        sumv=0.
        do i=1,numPool
            sumv = sumv + (itemUsed(i,try)-itemUsed(i,try-1)) * combination(try-itemUsed(i,try),alpha)
        enddo
        if (try<=alpha)then
            Omega(try)=0
        else
            Omega(try) = REAL(try-alpha-1)/try * Omega(try-1) + REAL(alpha+1)/try &
            - sumv / (length * combination(try,alpha+1))
        endif
        return
    end subroutine subr_testOmega