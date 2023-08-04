program main_Random_cont
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_300.txt"
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"!Uniform Normal
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數 10000
    integer,parameter :: numPool = 300 !題庫數
    integer,parameter :: length = 40 !作答題長 
    integer,parameter :: numContentType = 3
    ! === module set ===
    integer :: contentScale(numContentType) = (/2,2,1/)
    integer :: contentMultiplier = length/5
    ! === content target ===
    integer :: contentGoal = 0
    integer :: contentTarget(numContentType) != (/16,16,8/)
    integer :: contentTargetSum(numContentType) != (/16,32,40/)
    integer :: contentChange(numContentType) 
    integer :: contentPoolNum(numContentType) = (/120,120,60/)  ! 題庫中各內容領域的題數
    integer :: contentPoolNumSum(numContentType) != (/120,240,300/)
    real :: randContent
    real :: contentTP(numContentType)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    ! === true theta ===
    real :: thetaTrue(numTest) = 0.0 !真實能力值
    real :: thetaTrueMean = 0.0 !真實能力值之平均
    real :: thetaBegin = 0.0
    ! === function ===
    real, external :: information, probability, normalPDF
    ! === unknown data ===
    ! === 迴圈用 ===
    integer :: i,j
    integer :: try
    integer :: choose
    integer :: xi
    real :: x, y
    ! === 運算暫存用 ===
    real :: maxv !最大值
    integer :: sumv_Int 
    integer :: place
    real :: infor(numPool) !題庫各試題的訊息量
    real :: randv(length, numTest) ! response使用
    real :: randv_item
    ! === output data ===
    integer :: resp(length, numTest) !作答反應
    integer:: place_choose(length, numTest) !選題的試題位置
    ! content 相關 
    integer:: content_choose(length, numTest)
    integer, dimension(numContentType, numTest)::contentResult
    real, dimension(numContentType)::contentResultMean
    integer, dimension(numContentType)::contentResultMax
    integer, dimension(numContentType)::contentResultMin
    ! 選題的試題參數 
    real:: a_choose(length, numTest),b_choose(length, numTest), &
    c_choose(length, numTest) 
    ! 試題使用率；使用 = Administer
    integer :: usedPool(numPool, numTest) !紀錄試題是否被使用過
    integer :: usedSum(numPool, numTest) !試題被使用過的累加次數
    integer :: usedTotalSum(numTest)
    real:: usedRate(numPool) !PA
    real:: usedRateMax !maxPA
    real:: usedRateMean !PAmean
    real:: usedRateVar !PAvar
    !PoolUR；題數使用率
    real :: poolUsedRate 
    ! 測驗重疊率參數
    real:: testOverlapData
    real:: testOverlap
    ! 估計能力參數 
    real::thetaHat(length, numTest)
    real::thetaHatMean !估計能力值的平均數
    real::thetaBias !估計能力值與真值的差之平均
    real::thetaHatVar !估計能力值的變異數
    real::thetaHatMSE !估計能力值的MSE
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
    ! infor note
    real, dimension(length, numTest):: choose_inforTrue
    real, dimension(length, numTest):: choose_inforEstimate
    real, dimension(numTest):: choose_inforTrueSum
    real, dimension(numTest):: choose_inforEstimateSum
    real:: testMean_inforTrue
    real:: testMean_inforEstimate
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
    character(len = 20), parameter :: dataPool_T = '(A10,500I10)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataPoolFS = '(500F10.2)' ! 隨著 pool item number 改變而改變
    character(len = 20), parameter :: dataContentReal = '(10F10.4)'  ! 隨著 content type number 改變
    character(len = 20), parameter :: dataContentInt = '(10I10)'

    ! === run code ===
    call cpu_time (t1) !開始計時
    ! 讀取資料
    ! 輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i) !三參數
    enddo
    close(100)

    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !從資料讀取三參數
    enddo
    close(100)

    contentTarget = contentScale*contentMultiplier ! 模組內各領域試題題數
    do i = 1, numContentType
        if(i==1)then 
            contentTargetSum(i) = contentTarget(i)
            contentPoolNumSum(i) = contentPoolNum(i)
        else
            contentTargetSum(i) = contentTargetSum(i-1) + contentTarget(i)
            contentPoolNumSum(i) = contentPoolNumSum(i-1) + contentPoolNum(i)
        endif
    enddo
    
    ! 設定初始值
    do try = 1, numTest
        do i=1,numPool
            usedPool(i, try) = 0 
            usedSum(i, try) = 0
        enddo
        do i=1,length
            place_choose(i, try) = 0  
            resp(i, try) = 9
            a_choose(i, try) = 0
            b_choose(i, try) = 0
            c_choose(i, try) = 0
            content_choose(i, try) = 0
        enddo
        usedTotalSum(try) = 0
    enddo

    do try = 1, numTest
        do choose = 1, length

            ! 內容領域相關設定
            if ( choose == 1 ) then ! 不同module須重設的值
                contentChange = contentTarget ! 重設內容領域控制參數
            endif
            ! 選擇要施測的內容領域
            call subr_contentTargetP(contentChange, numContentType, contentTP) ! 計算各內容機率範圍
            call random_number(randContent) !隨機選擇要施測的內容領域
            do i = 1, numContentType
                if (i.EQ.1) then
                    if ((randContent > 0) .AND. (randContent <= contentTP(i))) then
                        contentGoal = i
                    endif
                else
                    if ((randContent > contentTP(i-1)) .AND. (randContent <= contentTP(i))) then
                        contentGoal = i
                    endif
                endif
            enddo
            contentChange(contentGoal) = contentChange(contentGoal)-1 ! 刪除"選中&施測"的內容題數

            ! 從選定的內容領域中隨機選題
            do while (.true.)
                call random_number(randv_item)
                xi = INT(randv_item * contentPoolNum(contentGoal)) + 1
                if (contentGoal == 1) then
                    place = xi
                else
                    place = contentPoolNumSum(contentGoal-1) + xi
                endif
                if ( usedPool(place, try) == 0 ) exit              
            enddo

            ! 成功選題後，紀錄選出的試題
            place_choose(choose, try) = place
            usedPool(place_choose(choose, try), try) = 1 !紀錄選出並使用的試題
            ! 紀錄使用的試題參數
            a_choose(choose, try) = a(place_choose(choose, try))
            b_choose(choose, try) = b(place_choose(choose, try))
            c_choose(choose, try) = c(place_choose(choose, try))
            content_choose(choose, try) = content(place_choose(choose, try))
            ! 模擬作答，並記錄作答反應
            call subr_resp(thetaTrue(try), &
            a_choose(choose, try),b_choose(choose, try),c_choose(choose, try),&
            resp(choose, try),randv(choose, try))
            ! EAP能力估計
            call subr_EAP(choose, &
            a_choose(1:choose, try),b_choose(1:choose, try),c_choose(1:choose, try),&
            resp(1:choose, try), thetaHat(choose, try))
        enddo
        
        ! 算出被施測(A)的累加次數、PA值
        do i=1, numPool
            if ( try == 1 ) then
                usedSum(i,try) = usedPool(i,try)
                usedTotalSum(try) = usedPool(i,try)
            else
                usedSum(i,try) = usedSum(i,try-1) + usedPool(i,try)
                usedTotalSum(try) = usedTotalSum(try) + usedPool(i,try)
            endif
        enddo

        ! 計算每位受試者於不同內容領域中用了幾題
        do i=1,numContentType
            call subr_contentCount(content_choose(:,try),length,i,contentResult(i,try))
        enddo

    enddo

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
    call subr_testOverlap(place_choose, numTest, length, testOverlapData) ! 不知為何會受下面subr_maxvInt的影響，待查證
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
    ! mean of infor 計算
    do i = 1, numTest
        do j = 1, length
            choose_inforTrue(j,i) = information(thetaTrue(i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            choose_inforEstimate(j,i) = information(thetaHat(length,i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            choose_inforTrueSum(i) = choose_inforTrueSum(i) + choose_inforTrue(j,i)
            choose_inforEstimateSum(i) = choose_inforEstimateSum(i) + choose_inforEstimate(j,i)
        enddo
    enddo
    call subr_aveReal(choose_inforTrueSum, numTest, testMean_inforTrue)
    call subr_aveReal(choose_inforEstimateSum, numTest, testMean_inforEstimate)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A30)') " Random_with_cont "
    write(unit = 100, fmt = '(A15,I15)') "stages", 1
    write(unit = 100, fmt = '(A15,F15.5)') "time", t2-t1
    write(unit = 100, fmt = '(A15,I15)') "test_n", numTest
    write(unit = 100, fmt = '(A15,I15)') "pool_n", numPool
    write(unit = 100, fmt = '(A15,I15)') "length", length
    write(unit = 100, fmt = '(/,A30)') "ThetaHat_of_Estimates: "
    write(unit = 100, fmt = '(A15, F15.5)') "Mean", thetaHatMean
    write(unit = 100, fmt = '(A15, F15.5)') "Bias", thetaBias
    write(unit = 100, fmt = '(A15, F15.5)') "Var", thetaHatVar
    write(unit = 100, fmt = '(A15, F15.5)') "MSE", thetaHatMSE
    write(unit = 100, fmt = '(A15, F15.5)') "RMSE", thetaHatMSE**0.5
    write(unit = 100, fmt = '(/,A30)') "Item_Exposure_Rate: "
    write(unit = 100, fmt = '(A15, F15.5)') "max", usedRateMax
    write(unit = 100, fmt = '(A15, F15.5)') "mean", usedRateMean
    write(unit = 100, fmt = '(A15, F15.5)') "var", usedRateVar
    write(unit = 100, fmt = '(/,A30)') "Pool_Used: "
    write(unit = 100, fmt = '(A15, F15.5)') "Rate", poolUsedRate
    write(unit = 100, fmt = '(/,A30)') "Test_Overlap: "
    write(unit = 100, fmt = '(A15, F15.5)') "overlap", testOverlap
    write(unit = 100, fmt = '(/,A30)') "Mean_of_Infor: "
    write(unit = 100, fmt = '(A15, F15.5)') "True", testMean_inforTrue
    write(unit = 100, fmt = '(A15, F15.5)') "Estimate", testMean_inforEstimate
    write(unit = 100, fmt = '(/,A30)') "Psi_max"
    write(unit = 100, fmt = '(A15, F15.5)') "Set", 1.0
    write(unit = 100, fmt = '(A15, I15)') "alpha", 1
    write(unit = 100, fmt = '(A15, F15.5)') "Max_1", psiOneMax
    write(unit = 100, fmt = '(A15, F15.5)') "Max_2", psiTwoMax
    write(unit = 100, fmt = '(A15, F15.5)') "Max_3", psiThreeMax
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
    ! == pool used ==
    open(unit = 100 , file = 'ListCAT_poolUsed.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) (usedPool(j,i),j=1,numPool)
    end do
    close(100)
    open(unit = 100 , file = 'ListCAT_poolUsedSum.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "pool used sum = "
    write(unit = 100, fmt = dataPool_T) "SUM",(j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPool) usedTotalSum(i),(usedSum(j,i),j=1,numPool)
    end do
    close(100)
    ! == Omega & Psi ==
    open(unit = 100 , file = 'ListCAT_testOmega&Psi.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(7A10)') "Stat.", "Omega1", "Omega2", "Omega3", "Psi1", "Psi2", "Psi3"
    write(unit = 100, fmt = '(A10,6F10.5)') "Mean", omegaOneMean, omegaTwoMean, omegaThreeMean,&
    psiOneMean, psiTwoMean, psiThreeMean
    write(unit = 100, fmt = '(A10,6F10.5)') "Max", omegaOneMax, omegaTwoMax, omegaThreeMax,&
    psiOneMax, psiTwoMax, psiThreeMax
    write(unit = 100, fmt = '(A10,6F10.5)') "Min", omegaOneMin, omegaTwoMin, omegaThreeMin,&
    psiOneMin, psiTwoMin, psiThreeMin
    write(unit = 100, fmt = '(A10,6F10.5)') "SD", omegaOneVar**0.5, omegaTwoVar**0.5, omegaThreeVar**0.5,&
    psiOneVar**0.5, psiTwoVar**0.5, psiThreeVar**0.5
    write(unit = 100, fmt = '(A10,6F10.5)') "Last", omegaOne(numTest), omegaTwo(numTest), omegaThree(numTest),&
    psiOne(numTest), psiTwo(numTest), psiThree(numTest)
    write(unit = 100, fmt = '(/,A)') "data = "
    do i=1,numTest
        write(unit = 100, fmt = '(6F10.5)') omegaOne(i), omegaTwo(i), omegaThree(i),&
        psiOne(i), psiTwo(i), psiThree(i)
    end do
    close(100)
    stop
end program main_Random_cont