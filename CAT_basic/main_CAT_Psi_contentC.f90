program CAT
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
    integer,parameter :: numPool = 300 !題庫數
    integer,parameter :: length = 40 !作答題長
    integer,parameter :: numContentType = 3
    ! === content target ===
    integer :: contentGoal
    integer :: contentTarget(numContentType) = (/14,13,13/)
    integer :: contentChange(numContentType) 
    real :: randContent
    real :: contentTP(numContentType)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    ! === true theta ===
    real :: thetaTrue(numTest) = 1. !真實能力值
    real :: thetaTrueMean !真實能力值之平均
    real :: thetaBegin = 0.
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    ! === 運算暫存用 ===
    real :: maxv !最大值 
    integer :: place
    real :: infor(numPool) !題庫各試題的訊息量
    integer :: usedPool(numPool, numTest) !紀錄試題是否被使用過
    integer :: usedSum(numPool, numTest) !試題被使用過的累加次數
    real :: randv(length, numTest)
    ! === output data ===
    integer :: resp(length, numTest) !作答反應
    integer:: place_choose(length, numTest) !選題的試題位置
    ! content 相關
    integer:: content_choose(length, numTest)
    integer, dimension(numContentType, numTest)::contentResult
    real, dimension(numContentType)::contentResultMean
    integer, dimension(numContentType)::contentResultMax
    integer, dimension(numContentType)::contentResultMin
    !選題的試題參數
    real:: a_choose(length, numTest),b_choose(length, numTest), &
    c_choose(length, numTest) 
    ! 試題使用率
    real:: usedRate(numPool)
    real:: usedRateMax
    real:: usedRateMean
    real:: usedRateVar
    ! 測驗重疊率參數
    real:: testOverlapData
    real:: testOverlap
    ! Omega
    real, dimension(numTest)::omegaOne
    real, dimension(numTest)::omegaTwo
    real, dimension(numTest)::omegaThree
    real:: omegaOneMean, omegaTwoMean, omegaThreeMean !平均
    real:: omegaOneMax, omegaTwoMax, omegaThreeMax 
    real:: omegaOneMin, omegaTwoMin, omegaThreeMin
    ! Psi
    real, dimension(numTest)::psiOne
    real, dimension(numTest)::psiTwo
    real, dimension(numTest)::psiThree
    real:: psiOneMean, psiTwoMean, psiThreeMean
    real:: psiOneMax, psiTwoMax, psiThreeMax
    real:: psiOneMin, psiTwoMin, psiThreeMin
    ! Psi 控制參數 
    integer:: alpha = 1
    real:: psiMax = 0.3
    !real, dimension(numTest):: psi
    ! Psi 控制過程中的各類指標
    real, external :: combination, func_deltaPsi
    real:: criteria 
    real:: delta(numTest)
    real, dimension(length, numTest):: deltaChange
    !real::lambda 
    real, dimension(numPool, numTest)::eta !item contribution
    real::eta_choose(length, numTest) !選中的eta
    ! 估計能力參數
    real::thetaHat(length, numTest)
    real::thetaHatMean !估計能力值的平均數
    real::thetaBias !估計能力值與真值的差之平均
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMSE !估計能力值的MSE
    ! item pool 的相關資料紀錄 ===
    real :: poolUsedRate
    ! === 存取時間 ===
    real (kind=8) t1 !開始時間
    real (kind=8) t2 !結束時間
    ! === output error ===
    integer :: ierror
    ! === 輸出資料格式設定 === 
    character(len = 20), parameter :: input = 'ListCAT_thetaHat.txt'
    character(len = 20), parameter :: dataINT = '(100I10)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataF = '(100F10.4)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataFS = '(100F10.2)' ! 隨著 length 改變而改變
    character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataPoolFS = '(500F10.2)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataContentReal = '(10F10.4)'  ! 隨著 content type number 改變
    character(len = 20), parameter :: dataContentInt = '(10I10)'
    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料：輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i) !三參數
    enddo
    close(100)
    ! 開始模擬
    do try = 1, numTest
        
        ! Psi 控制設定
        if (try <= alpha) then 
            do i=1,numPool
                eta(i, try) = 0 !combination(try-1,alpha) !mit=0 !選出符合條件者 予以施測
            enddo
        else
            do i=1,numPool 
                eta(i,try) = combination(try-(usedSum(i,try-1)+1),alpha) !mit=0 !選出符合條件者 予以施測
            enddo
        endif
        ! delta(try) = func_deltaPsi(try,alpha,psiMax,length)
        ! deltaChange(1, try) = delta(try) !紀錄delta的變化

        do  choose = 1, length
            
            if ( choose == 1 ) then
                contentChange = contentTarget ! 重設內容領域控制參數
            endif
            call subr_contentTargetP(contentChange, numContentType, contentTP)
            call random_number(randContent)
            !WRITE(*,*) randContent
            do i = 1, numContentType
                if (i.EQ.1) then
                    if ((randContent > 0) .AND. (randContent <= contentTP(i))) then
                        contentGoal = i
                    endif
                else
                    if ((randContent >= contentTP(i-1)) .AND. (randContent <= contentTP(i))) then
                        contentGoal = i
                    endif
                endif
            enddo
            contentChange(contentGoal) = contentChange(contentGoal)-1 ! 刪除選中的內容題數

            if (try<=alpha) then 
                if ( (choose == 1) ) then                
                    do i = 1, numPool
                        if ( content(i) == contentGoal ) then
                            infor(i) = information(thetaBegin, a(i), b(i), c(i))
                        else
                            infor(i) = 0
                        endif
                    enddo
                else
                    do i = 1, numPool
                        if ( ( usedPool(i, try) == 0 ) .AND. ( content(i) == contentGoal ) ) then
                            infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                        else
                            infor(i) = 0
                        endif
                    enddo
                endif
            else
                if ( choose == 1 ) then
                    ! criteria = deltaChange(choose,try)/length
                    ! do i = 1, numPool
                    !     if (eta(i,try) >= criteria) then
                    !         infor(i) = information(thetaBegin, a(i), b(i), c(i))
                    !     else
                    !         infor(i) = 0
                    !     endif
                    ! enddo
                    do i = 1, numPool
                        if ( content(i) == contentGoal ) then
                            infor(i) = information(thetaBegin, a(i), b(i), c(i))
                        else
                            infor(i) = 0
                        endif
                    enddo
                else
                    criteria = deltaChange(choose,try)/(length-(choose-1))
                    do i = 1, numPool
                        if ( ( usedPool(i, try) == 0 ) .AND. ( content(i) == contentGoal ) .AND. (eta(i,try) >= criteria) ) then
                            infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                        else
                            infor(i) = 0
                        endif
                    enddo
                endif
            endif
            call subr_maxvReal(infor, numPool, maxv, place_choose(choose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
            usedPool(place_choose(choose, try), try) = 1 !紀錄使用試題
            ! 紀錄使用的試題參數
            a_choose(choose, try) = a(place_choose(choose, try))
            b_choose(choose, try) = b(place_choose(choose, try))
            c_choose(choose, try) = c(place_choose(choose, try))
            content_choose(choose, try) = content(place_choose(choose, try))
            ! 紀錄與更新Psi控制指標
            eta_choose(choose,try) = eta(place_choose(choose, try),try)
            if ( choose+1 <= length ) then
                deltaChange(choose+1,try) = deltaChange(choose,try) - eta(place_choose(choose, try),try)
            else
                deltaChange(choose,try) = deltaChange(choose,try)
            endif
            ! 模擬作答反應
            call subr_resp(thetaTrue(try), &
            a_choose(choose, try),b_choose(choose, try),c_choose(choose, try),&
            resp(choose, try),randv(choose, try))
            ! EAP能力估計
            call subr_EAP(choose, &
            a_choose(1:choose, try),b_choose(1:choose, try),c_choose(1:choose, try),&
            resp(1:choose, try), thetaHat(choose, try))
        enddo
        ! 紀錄試題累計使用次數
        do i=1, numPool
            if ( try == 1 ) then
                usedSum(i,try) = usedPool(i,try)
            else
                usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try)
            endif
        enddo
        ! 計算每位受試者於不同內容領域中用了幾題
        do j=1,numContentType
            call subr_contentCount(content_choose(:,try),length,j,contentResult(j,try))
        enddo
    end do
    call cpu_time (t2) !結束計時
    ! thetaHat 計算
    call subr_aveReal(thetaTrue, numTest, thetaTrueMean)
    call subr_aveReal(thetaHat(length,:), numTest, thetaHatMean)
    thetaBias = thetaHatMean - thetaTrueMean
    call subr_varReal(thetaHat(length,:), numTest, thetaHatVar)
    call subr_mseReal(thetaHat(length,:), thetaTrue(:), numTest, thetaHatMSE)
    ! item used rate 計算
    call subr_itemUsedRate(usedPool, numTest, numPool, usedRate)
    call subr_maxvReal(usedRate, numPool, usedRateMax, place)
    call subr_aveReal(usedRate, numPool, usedRateMean)
    call subr_varReal(usedRate, numPool, usedRateVar)
    ! item pool 計算
    call subr_itemPoolUsedRate(usedPool, numTest, numPool, poolUsedRate)
    ! test overlap
    call subr_testOverlap(place_choose, numTest, length, testOverlapData)     ! 不知為何會受下面subr_maxvInt的影響，待查證
    testOverlap = testOverlapData
    ! content mean 計算
    do i = 1, numContentType
        call subr_aveIntToReal(contentResult(i,:), numTest, contentResultMean(i))
        call subr_maxvInt(contentResult(i,:), numTest, contentResultMax(i))
        call subr_minvInt(contentResult(i,:), numTest, contentResultMin(i))
    enddo
    ! == 計算Omega&Psi ==
    do try = 1, numTest
        call subr_testOmega(numTest,numPool,1,usedSum,length,try,omegaOne)
        call subr_testOmega(numTest,numPool,2,usedSum,length,try,omegaTwo)
        call subr_testOmega(numTest,numPool,3,usedSum,length,try,omegaThree)
        call subr_testPsi(numTest,numPool,1,usedSum,length,try,psiOne)
        call subr_testPsi(numTest,numPool,2,usedSum,length,try,psiTwo)
        call subr_testPsi(numTest,numPool,3,usedSum,length,try,psiThree)
    enddo
    call subr_aveReal(omegaOne, numTest, omegaOneMean)
    call subr_aveReal(omegaTwo, numTest, omegaTwoMean)
    call subr_aveReal(omegaThree, numTest, omegaThreeMean)
    call subr_maxvReal(omegaOne(2:numTest), numTest-1, omegaOneMax, place)
    call subr_maxvReal(omegaTwo(3:numTest), numTest-2, omegaTwoMax, place)
    call subr_maxvReal(omegaThree(4:numTest), numTest-3, omegaThreeMax, place)
    call subr_minvReal(omegaOne(2:numTest), numTest-1, omegaOnemin, place)
    call subr_minvReal(omegaTwo(3:numTest), numTest-2, omegaTwomin, place)
    call subr_minvReal(omegaThree(4:numTest), numTest-3, omegaThreemin, place)
    call subr_aveReal(psiOne, numTest, psiOneMean)
    call subr_aveReal(psiTwo, numTest, psiTwoMean)
    call subr_aveReal(psiThree, numTest, psiThreeMean)
    call subr_maxvReal(psiOne(2:numTest), numTest-1, psiOneMax, place)
    call subr_maxvReal(psiTwo(3:numTest), numTest-2, psiTwoMax, place)
    call subr_maxvReal(psiThree(4:numTest), numTest-3, psiThreeMax, place)
    call subr_minvReal(psiOne(2:numTest), numTest-1, psiOnemin, place)
    call subr_minvReal(psiTwo(3:numTest), numTest-2, psiTwomin, place)
    call subr_minvReal(psiThree(4:numTest), numTest-3, psiThreemin, place)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A10,A)') "method = ", " CAT with Psi + con"
    write(unit = 100, fmt = '(A10,F10.5)') "time = ", t2-t1
    write(unit = 100, fmt = '(A10,I10)') "test n = ", numTest
    write(unit = 100, fmt = '(A10,I10)') "pool n = ", numPool
    write(unit = 100, fmt = '(A10,I10)') "length = ", length
    write(unit = 100, fmt = '(A10,F10.5)') "psi max = ", psiMax ! 增加Psi的設定指標
    write(unit = 100, fmt = '(A10,I10)') "alpha = ", alpha ! 增加Psi的設定指標
    write(unit = 100, fmt = '(/,A)') "About thetaHat: "
    write(unit = 100, fmt = '(A10, F10.5)') "Mean = ", thetaHatMean
    write(unit = 100, fmt = '(A10, F10.5)') "Bias = ", thetaBias
    write(unit = 100, fmt = '(A10, F10.5)') "Var = ", thetaHatVar
    write(unit = 100, fmt = '(A10, F10.5)') "MSE = ", thetaHatMSE
    write(unit = 100, fmt = '(A10, F10.5)') "RMSE = ", thetaHatMSE**0.5
    write(unit = 100, fmt = '(/,A)') "About item used rate: "
    write(unit = 100, fmt = '(A10, F10.5)') "max = ", usedRateMax
    write(unit = 100, fmt = '(A10, F10.5)') "mean = ", usedRateMean
    write(unit = 100, fmt = '(A10, F10.5)') "var = ", usedRateVar
    write(unit = 100, fmt = '(/,A)') "About pool used: "
    write(unit = 100, fmt = '(A10, F10.5)') "Rate = ", poolUsedRate
    write(unit = 100, fmt = '(/,A)') "About test overlap: "
    write(unit = 100, fmt = '(A10, F10.5)') "overlap = ", testOverlap
    close(100)
    ! == theta hat ==
    open(unit = 100 , file = 'ListCAT_theta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "thetaHat = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataF) (thetaHat(j,i),j=1,length)
    end do
    close (100)
    ! == response ==
    open(unit = 100 , file = 'ListCAT_resp.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "response = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (resp(j,i),j=1,length)
    end do
    close(100)
    ! == item choose ==
    open(unit = 100 , file = 'ListCAT_item.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose item = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (place_choose(j,i),j=1,length)
    end do
    close(100)
    ! == content ==
    open(unit = 100 , file = 'ListCAT_content.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose content = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (content_choose(j,i),j=1,length)
    end do
    close(100)
    ! == content summary ==
    open(unit = 100 , file = 'ListCAT_contentSum.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "type = "
    write(unit = 100, fmt = dataContentINT) (j, j=1,numContentType)
    write(unit = 100, fmt = '(A)') "mean = "
    write(unit = 100, fmt = dataContentReal) (contentResultMean(j),j=1,numContentType)
    write(unit = 100, fmt = '(A)') "max = "
    write(unit = 100, fmt = dataContentINT) (contentResultMax(j),j=1,numContentType)
    write(unit = 100, fmt = '(A)') "min = "
    write(unit = 100, fmt = dataContentINT) (contentResultMin(j),j=1,numContentType)
    write(unit = 100, fmt = '(/,A)') "content sum = "
    do i=1,numTest
        write(unit = 100, fmt = dataContentINT) (contentResult(j,i),j=1,numContentType)
    end do
    close(100)
    open(unit = 100 , file = 'ListCAT_poolUsed.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) (usedPool(j,i),j=1,numPool)
    end do
    close(100)
    ! == pool used ==
    open(unit = 100 , file = 'ListCAT_poolUsedSum.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used sum = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) (usedSum(j,i),j=1,numPool)
    end do
    close(100)
    ! == Omega & Psi ==
    open(unit = 100 , file = 'ListCAT_testOmega&Psi.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(6A10)') "Omega 1", "Omega 2", "Omega 3", "Psi 1", "Psi 2", "Psi 3"
    write(unit = 100, fmt = '(A)') "Mean = "
    write(unit = 100, fmt = '(6F10.5)') omegaOneMean, omegaTwoMean, omegaThreeMean,&
    psiOneMean, psiTwoMean, psiThreeMean
    write(unit = 100, fmt = '(A)') "Max = "
    write(unit = 100, fmt = '(6F10.5)') omegaOneMax, omegaTwoMax, omegaThreeMax,&
    psiOneMax, psiTwoMax, psiThreeMax
    write(unit = 100, fmt = '(A)') "Min = "
    write(unit = 100, fmt = '(6F10.5)') omegaOneMin, omegaTwoMin, omegaThreeMin,&
    psiOneMin, psiTwoMin, psiThreeMin
    write(unit = 100, fmt = '(A)') "Last = "
    write(unit = 100, fmt = '(6F10.5)') omegaOne(numTest), omegaTwo(numTest), omegaThree(numTest),&
    psiOne(numTest), psiTwo(numTest), psiThree(numTest)
    write(unit = 100, fmt = '(/,A)') "data = "
    do i=1,numTest
        write(unit = 100, fmt = '(6F10.5)') omegaOne(i), omegaTwo(i), omegaThree(i),&
        psiOne(i), psiTwo(i), psiThree(i)
    end do
    close(100)
    ! == Psi Control Data ==
    open(unit = 100 , file = 'ListCAT_testPsiEta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "eta = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPoolFS) (eta(j,i),j=1,numPool)
    end do
    close(100) 
    open(unit = 100 , file = 'ListCAT_testPsiDeltaChange.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "delta change = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataFS) (deltaChange(j,i),j=1,length)
    end do
    close(100)
    stop
end program CAT

