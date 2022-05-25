program MST
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_MST_len10_1-2_P.txt" !parameter_MST_1-3-3-3_data_P.txt !data/parameter_MST_1-2-3-4_data_P.txt
    character(len = 50), parameter :: dataPath2 = "data/Normal_Population.txt"  !Population_Uniform.txt" !"data/Normal_Population.txt"
    ! === MST set ===
    integer,parameter :: numStages = 2
    integer, parameter :: maxModule = 20 !有平行測驗時記得改
    integer, parameter :: numItemInModule = 10
    !integer :: MSTdesign(numStages) = (/1,2,3,4/)
    integer :: MSTnump(numStages) = (/10,5/) !(/4,3,2,1/) (/3,1,1,1/) !每階段之每module平行測驗數
    
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
    integer,parameter :: numPool = 200 !200 !題庫數
    integer,parameter :: length = numStages*numItemInModule !作答題長
    integer,parameter :: numContentType = 3
    ! === MST set 2 ===
    real :: rand_module
    integer :: randToInt
    !real :: rand_module(numStages, numTest)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    integer:: stage(numPool)
    integer:: module(numPool) 
    integer:: level(numPool)
    integer:: pnum(numPool)
    integer:: itemID(numPool)
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
    real :: x
    ! === 運算暫存用 ===
    real :: maxv !最大值 
    integer :: place
    real :: infor(numPool) !題庫各試題的訊息量
    integer :: usedPool(numPool, numTest) !紀錄試題是否被使用過
    integer :: usedSum(numPool, numTest) !試題被使用過的累加次數
    real :: randv(length, numTest)
    ! === MST 的運算暫存 ===
    real :: inforSum(maxModule)
    ! integer :: usedModule(maxModule)
    ! === output data ===
    integer :: resp(length, numTest) !作答反應
    integer:: place_choose(length, numTest) !選題的試題位置
    integer:: placeModule_choose(numStages, numTest) !被選擇的Module
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
    real:: omegaOneVar, omegaTwoVar, omegaThreeVar
    ! Psi
    real, dimension(numTest)::psiOne
    real, dimension(numTest)::psiTwo
    real, dimension(numTest)::psiThree
    real:: psiOneMean, psiTwoMean, psiThreeMean
    real:: psiOneMax, psiTwoMax, psiThreeMax
    real:: psiOneMin, psiTwoMin, psiThreeMin
    real:: psiOneVar, psiTwoVar, psiThreeVar
    ! 估計能力參數
    ! real::thetaHat(length, numTest) 
    real::thetaHat(numStages, numTest) ! 因MST而有修改
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
    character(len = 20), parameter :: dataPool = '(500I10)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataContentReal = '(10F10.4)'  ! 隨著 content type number 改變
    character(len = 20), parameter :: dataContentInt = '(10I10)'
    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料
    ! 輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i),stage(i),module(i),&
        level(i),pnum(i),itemID(i) !三參數
    enddo
    close(100)
    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !三參數
    enddo
    close(100)
    ! 開始模擬
    call random_number(rand_module)
    randToInt = INT(1+FLOOR(MSTnump(choose)*rand_module))
    do try = 1,numTest
        do  choose = 1, numStages
            if (choose == 1) then 
                do i = 1, numPool
                    if ( (i>1).AND.(level(i-1).NE.level(i)) ) then 
                        call random_number(rand_module)
                        randToInt = INT(1+FLOOR(MSTnump(choose)*rand_module))
                    endif
                    if ( (stage(i)==choose).AND.(usedPool(i, try) == 0).AND.(pnum(i)==randToInt) ) then 
                        infor(i) = information(thetaBegin, a(i), b(i), c(i))
                    else
                        infor(i) = 0
                    endif
                enddo
            else
                do i = 1, numPool
                    if ( (i>1).AND.(level(i-1).NE.level(i)) ) then 
                        call random_number(rand_module)
                        randToInt = INT(1+FLOOR(MSTnump(choose)*rand_module))
                    endif
                    if ( (stage(i)==choose).AND.(usedPool(i, try) == 0).AND.(pnum(i)==randToInt) ) then
                        infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                    else
                        infor(i) = 0
                    endif
                enddo
            endif
            ! 計算 information in each module 的總和
            do i = 1,maxModule
                call subr_sumReal(infor(((i-1)*numItemInModule+1):(i*numItemInModule)),&
                numItemInModule,inforSum(i))
            enddo
            call subr_maxvReal(inforSum, maxModule, maxv, placeModule_choose(choose, try)) ! 求出最大訊息量與其題庫ID(紀錄使用的試題題號)
            do i = (placeModule_choose(choose, try)-1)*numItemInModule+1, placeModule_choose(choose, try)*numItemInModule
                usedPool(i,try) = 1 !紀錄使用試題
            enddo
            do j = (choose-1)*numItemInModule+1, choose*numItemInModule
                    place_choose(j,try) = (placeModule_choose(choose, try)-1)*numItemInModule+(j-(choose-1)*numItemInModule)
                    ! 紀錄使用的試題參數
                    a_choose(j, try) = a(place_choose(j,try))
                    b_choose(j, try) = b(place_choose(j,try))
                    c_choose(j, try) = c(place_choose(j,try))
                    content_choose(j, try) = content(place_choose(j,try))
                    ! 模擬作答反應
                    call subr_resp(thetaTrue(try), &
                    a_choose(j, try),b_choose(j, try),c_choose(j, try),&
                    resp(j, try),randv(j, try))
            enddo
            ! EAP能力估計
            i = choose*numItemInModule
            call subr_EAP(i, a_choose(1:i, try),b_choose(1:i, try), c_choose(1:i, try),&
            resp(1:i, try), thetaHat(choose, try))
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
            call subr_contentCount(content_choose(:,try),numStages*numItemInModule,j,contentResult(j,try))
        enddo
    enddo
    call cpu_time (t2) !結束計時
    ! thetaHat 計算
    ! 因MST而有修改
    call subr_aveReal(thetaTrue, numTest, thetaTrueMean)
    call subr_aveReal(thetaHat(numStages,:), numTest, thetaHatMean)
    thetaBias = thetaHatMean - thetaTrueMean
    call subr_varReal(thetaHat(numStages,:), numTest, thetaHatVar)
    call subr_mseReal(thetaHat(numStages,:), thetaTrue(:), numTest, thetaHatMSE)
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
    call subr_varReal(omegaOne(2:numTest), numTest-1, omegaOneVar)
    call subr_varReal(omegaTwo(3:numTest), numTest-2, omegaTwoVar)
    call subr_varReal(omegaThree(4:numTest), numTest-3, omegaThreeVar)
    call subr_aveReal(psiOne, numTest, psiOneMean)
    call subr_aveReal(psiTwo, numTest, psiTwoMean)
    call subr_aveReal(psiThree, numTest, psiThreeMean)
    call subr_maxvReal(psiOne(2:numTest), numTest-1, psiOneMax, place)
    call subr_maxvReal(psiTwo(3:numTest), numTest-2, psiTwoMax, place)
    call subr_maxvReal(psiThree(4:numTest), numTest-3, psiThreeMax, place)
    call subr_minvReal(psiOne(2:numTest), numTest-1, psiOnemin, place)
    call subr_minvReal(psiTwo(3:numTest), numTest-2, psiTwomin, place)
    call subr_minvReal(psiThree(4:numTest), numTest-3, psiThreemin, place)
    call subr_varReal(psiOne(2:numTest), numTest-1, psiOneVar)
    call subr_varReal(psiTwo(3:numTest), numTest-2, psiTwoVar)
    call subr_varReal(psiThree(4:numTest), numTest-3, psiThreeVar)

    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A10,A)') "method = ", " MST infor"
    write(unit = 100, fmt = '(A10,F10.5)') "time = ", t2-t1
    write(unit = 100, fmt = '(A10,I10)') "test n = ", numTest
    write(unit = 100, fmt = '(A10,I10)') "pool n = ", numPool
    write(unit = 100, fmt = '(A10,I10)') "length = ", length
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
    ! === 因MST而修改
    open(unit = 100 , file = 'ListCAT_theta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "thetaHat = "
    write(unit = 100, fmt = dataINT) (j, j=1,numStages)
    do i=1,numTest
        write(unit = 100, fmt = dataF) (thetaHat(j,i),j=1,numStages)
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
    ! == Module choose ==
    open(unit = 100 , file = 'ListCAT_module.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "choose module = "
    write(unit = 100, fmt = dataINT) (j, j=1,numStages)
    do i=1,numTest
        write(unit = 100, fmt = dataINT) (placeModule_choose(j,i),j=1,numStages)
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
    write(unit = 100, fmt = '(A)') "SD = "
    write(unit = 100, fmt = '(6F10.5)') omegaOneVar**0.5, omegaTwoVar**0.5, omegaThreeVar**0.5,&
    psiOneVar**0.5, psiTwoVar**0.5, psiThreeVar**0.5
    write(unit = 100, fmt = '(A)') "Last = "
    write(unit = 100, fmt = '(6F10.5)') omegaOne(numTest), omegaTwo(numTest), omegaThree(numTest),&
    psiOne(numTest), psiTwo(numTest), psiThree(numTest)
    write(unit = 100, fmt = '(/,A)') "data = "
    do i=1,numTest
        write(unit = 100, fmt = '(6F10.5)') omegaOne(i), omegaTwo(i), omegaThree(i),&
        psiOne(i), psiTwo(i), psiThree(i)
    end do
    close(100)
    stop
end program MST

