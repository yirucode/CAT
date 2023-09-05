program D_MST_Psi
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_DMST_30.txt" !15(20/module) 30(10/module) 60(5/module)
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"
    ! === MST set ===
    integer, parameter :: numStages = 2
    integer, parameter :: maxLevel = 3
    integer, parameter :: numItemInModule = 10 !20 10 5 (每個module內的題數)
    integer, parameter :: numModuleInLevel = 10 !5 10 20 (每個level內的module數)
    integer, parameter :: maxModule = maxLevel*numModuleInLevel
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
    integer,parameter :: numPool = 300 !題庫數 300 !324
    integer,parameter :: length = numStages*numItemInModule !作答題長
    integer,parameter :: numContentType = 3
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    integer:: level(numPool)
    integer:: module(numPool) 
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
    ! Psi 控制參數 
    integer:: alpha = 1
    real:: psiMax = 0.3

    ! Psi 鬆綁參數
    real:: max_eta
    real:: place_maxEta
    integer:: count_NotZero = 0
    real, dimension(numPool, numTest):: choose_maxEta

    ! Psi 控制過程中的各類指標
    real, external :: combination, func_deltaPsi
    real, dimension(length, numTest):: deltaCriteria
    real, dimension(numPool, numTest)::eta !item contribution
    real::eta_choose(length, numTest) !選中的eta
    ! 因應alpha>1
    !integer,dimension(3)::alphaSet
    integer::max_alphaSet = 3
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
    ! infor note
    real, dimension(length, numTest):: choose_inforTrue
    real, dimension(length, numTest):: choose_inforEstimate
    real, dimension(numTest):: choose_inforTrueSum
    real, dimension(numTest):: choose_inforEstimateSum
    real :: testMean_inforTrue
    real :: testMean_inforEstimate
    ! 判斷題庫是否有試題可選 
    integer :: count_InforZero
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
    ! 讀取資料
    ! 輸入試題參數
    open(100, file= dataPath, status="old") 
    do i=1,numPool
        read(100,*) a(i),b(i),c(i),content(i),level(i),module(i) !三參數
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
    do try = 1,numTest
        
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
        
        do  choose = 1, numStages
            if (try <= alpha) then 
                if (choose == 1) then 
                    do i = 1, numPool
                        infor(i) = information(thetaBegin, a(i), b(i), c(i))
                    enddo
                else
                    do i = 1, numPool
                        if ( usedPool(i, try) == 0 ) then
                            infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                        else
                            infor(i) = 0
                        endif
                    enddo
                endif
            else
                count_InforZero = 0
                deltaCriteria(choose, try) = func_deltaPsi(try,alpha,psiMax,1) ! 設定Psi控制
                if (choose == 1) then 
                    do i = 1, numPool
                        if (eta(i,try) >= deltaCriteria(choose, try)) then
                            infor(i) = information(thetaBegin, a(i), b(i), c(i))
                        else
                            infor(i) = 0
                            count_InforZero = count_InforZero + 1 
                        endif
                    enddo
                else
                    do i = 1, numPool
                        if (( usedPool(i, try) == 0 ) .AND. (eta(i,try) >= deltaCriteria(choose, try))) then
                            infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                        else
                            infor(i) = 0
                            count_InforZero = count_InforZero + 1 
                        endif
                    enddo
                endif
                
                !標準太高，需降低標準以確保可作答的試題
                if (count_InforZero == numPool) then
                    count_NotZero = 0
                    do i = 1, numPool
                        if ( usedPool(i, try) == 0 ) then
                            count_NotZero = count_NotZero +1
                            choose_maxEta(i, try) = eta(i,try)
                        else
                            count_NotZero = count_NotZero +0
                            choose_maxEta(i, try) = 0
                        endif
                    enddo

                    ! 如果沒題目可選，則結束程式
                    if (count_NotZero == 0) then 
                        WRITE(*,*) "no item can be used"
                        WRITE(*,*) "try = ", try
                        WRITE(*,*) "choose = ", choose
                        ! 寫下問題資料細節
                        open(unit = 100 , file = 'ListCAT_debug.txt' , status = 'replace', action = 'write', iostat= ierror)
                        write(unit = 100, fmt = *) "try = ", try
                        write(unit = 100, fmt = *) "choose = ", choose
                        write(unit = 100, fmt = *) "contentGoal = ", "Nan"
                        write(unit = 100, fmt = *) "eta Criteria = ", deltaCriteria(choose, try)
                        write(unit = 100, fmt = *) "item ID = "
                        write(unit = 100, fmt = dataPool) (j, j=1,numPool)
                        write(unit = 100, fmt = *) "item content = "
                        write(unit = 100, fmt = dataPool) (content(j), j=1,numPool)
                        write(unit = 100, fmt = *) "item used = "
                        write(unit = 100, fmt = dataPool) (usedPool(j,try),j=1,numPool)
                        write(unit = 100, fmt = *) "item used sum = "
                        write(unit = 100, fmt = dataPool) (usedSum(j,try-1)+1,j=1,numPool)
                        write(unit = 100, fmt = *) "eta = "
                        write(unit = 100, fmt = dataPoolFS) (eta(j,try),j=1,numPool)
                        close(100)
                        stop ! 如果沒題目可選，則結束程式
                    else
                        call subr_maxvReal(choose_maxEta(:,try),numPool,max_eta,place_maxEta)
                        do i = 1, numPool
                            if (choose_maxEta(i,try) == max_eta) then
                                if ( (choose == 1) ) then
                                    infor(i) = information(thetaBegin, a(i), b(i), c(i))
                                else
                                    infor(i) = information(thetaHat(choose-1, try), a(i), b(i), c(i))
                                endif
                            else
                                infor(i) = 0
                            endif
                        enddo
                    endif
                endif
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
                    ! 紀錄與更新Psi控制指標
                    eta_choose(j,try) = eta(place_choose(j, try),try)
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
    ! if (alpha==1) then
    !     do i = 1,3
    !         alphaSet(i)=i
    !     enddo
    ! else
    !     do i = 1,3
    !         if (i > alpha) then 
    !             alphaSet(i)=i
    !         else
    !             alphaSet(i)=alpha
    !         endif
    !     enddo
    ! endif
    ! call subr_aveReal(omegaOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), omegaOneMean)
    ! call subr_aveReal(omegaTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), omegaTwoMean)
    ! call subr_aveReal(omegaThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), omegaThreeMean)
    ! call subr_maxvReal(omegaOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), omegaOneMax, place)
    ! call subr_maxvReal(omegaTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), omegaTwoMax, place)
    ! call subr_maxvReal(omegaThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), omegaThreeMax, place)
    ! call subr_minvReal(omegaOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), omegaOnemin, place)
    ! call subr_minvReal(omegaTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), omegaTwomin, place)
    ! call subr_minvReal(omegaThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), omegaThreemin, place)
    ! call subr_varReal(omegaOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), omegaOneVar)
    ! call subr_varReal(omegaTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), omegaTwoVar)
    ! call subr_varReal(omegaThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), omegaThreeVar)
    ! call subr_aveReal(psiOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), psiOneMean)
    ! call subr_aveReal(psiTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), psiTwoMean)
    ! call subr_aveReal(psiThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), psiThreeMean)
    ! call subr_maxvReal(psiOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), psiOneMax, place)
    ! call subr_maxvReal(psiTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), psiTwoMax, place)
    ! call subr_maxvReal(psiThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), psiThreeMax, place)
    ! call subr_minvReal(psiOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), psiOnemin, place)
    ! call subr_minvReal(psiTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), psiTwomin, place)
    ! call subr_minvReal(psiThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), psiThreemin, place)
    ! call subr_varReal(psiOne(alphaSet(1)+1:numTest), numTest-alphaSet(1), psiOneVar)
    ! call subr_varReal(psiTwo(alphaSet(2)+1:numTest), numTest-alphaSet(2), psiTwoVar)
    ! call subr_varReal(psiThree(alphaSet(3)+1:numTest), numTest-alphaSet(3), psiThreeVar)

    call subr_aveReal(omegaOne(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaOneMean)
    call subr_aveReal(omegaTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaTwoMean)
    call subr_aveReal(omegaThree(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaThreeMean)
    call subr_maxvReal(omegaOne(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaOneMax, place)
    call subr_maxvReal(omegaTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaTwoMax, place)
    call subr_maxvReal(omegaThree(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaThreeMax, place)
    call subr_minvReal(omegaOne(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaOnemin, place)
    call subr_minvReal(omegaTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaTwomin, place)
    call subr_minvReal(omegaThree(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaThreemin, place)
    call subr_varReal(omegaOne(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaOneVar)
    call subr_varReal(omegaTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaTwoVar)
    call subr_varReal(omegaThree(max_alphaSet+1:numTest), numTest-max_alphaSet, omegaThreeVar)
    call subr_aveReal(psiOne(max_alphaSet+1:numTest), numTest-max_alphaSet, psiOneMean)
    call subr_aveReal(psiTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, psiTwoMean)
    call subr_aveReal(psiThree(max_alphaSet+1:numTest), numTest-max_alphaSet, psiThreeMean)
    call subr_maxvReal(psiOne(max_alphaSet+1:numTest), numTest-max_alphaSet, psiOneMax, place)
    call subr_maxvReal(psiTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, psiTwoMax, place)
    call subr_maxvReal(psiThree(max_alphaSet+1:numTest), numTest-max_alphaSet, psiThreeMax, place)
    call subr_minvReal(psiOne(max_alphaSet+1:numTest), numTest-max_alphaSet, psiOnemin, place)
    call subr_minvReal(psiTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, psiTwomin, place)
    call subr_minvReal(psiThree(max_alphaSet+1:numTest), numTest-max_alphaSet, psiThreemin, place)
    call subr_varReal(psiOne(max_alphaSet+1:numTest), numTest-max_alphaSet, psiOneVar)
    call subr_varReal(psiTwo(max_alphaSet+1:numTest), numTest-max_alphaSet, psiTwoVar)
    call subr_varReal(psiThree(max_alphaSet+1:numTest), numTest-max_alphaSet, psiThreeVar)

    ! mean of infor 計算
    do i = 1, numTest
        do j = 1, length
            choose_inforTrue(j,i) = information(thetaTrue(i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            choose_inforEstimate(j,i) = information(thetaHat(numStages,i), a_choose(j, i), b_choose(j, i), c_choose(j, i))
            choose_inforTrueSum(i) = choose_inforTrueSum(i) + choose_inforTrue(j,i)
            choose_inforEstimateSum(i) = choose_inforEstimateSum(i) + choose_inforEstimate(j,i)
        enddo
    enddo
    call subr_aveReal(choose_inforTrueSum, numTest, testMean_inforTrue)
    call subr_aveReal(choose_inforEstimateSum, numTest, testMean_inforEstimate)
    ! === 輸出資料 ===
    open(unit = 100 , file = 'ListCAT_summary.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "D-MST_Psi"
    write(unit = 100, fmt = '(A10,I10)') "stages", numStages
    write(unit = 100, fmt = '(A10,F10.5)') "time", t2-t1
    write(unit = 100, fmt = '(A10,I10)') "test_n", numTest
    write(unit = 100, fmt = '(A10,I10)') "pool_n", numPool
    write(unit = 100, fmt = '(A10,I10)') "length", length
    write(unit = 100, fmt = '(/,A)') "ThetaHat_of_Estimates: "
    write(unit = 100, fmt = '(A10, F10.5)') "Mean", thetaHatMean
    write(unit = 100, fmt = '(A10, F10.5)') "Bias", thetaBias
    write(unit = 100, fmt = '(A10, F10.5)') "Var", thetaHatVar
    write(unit = 100, fmt = '(A10, F10.5)') "MSE", thetaHatMSE
    write(unit = 100, fmt = '(A10, F10.5)') "RMSE", thetaHatMSE**0.5
    write(unit = 100, fmt = '(/,A)') "Item_Exposure_Ratee: "
    write(unit = 100, fmt = '(A10, F10.5)') "max", usedRateMax
    write(unit = 100, fmt = '(A10, F10.5)') "mean", usedRateMean
    write(unit = 100, fmt = '(A10, F10.5)') "var", usedRateVar
    write(unit = 100, fmt = '(/,A)') "Pool_Used: "
    write(unit = 100, fmt = '(A10, F10.5)') "Rate", poolUsedRate
    write(unit = 100, fmt = '(/,A)') "Test_Overlap: "
    write(unit = 100, fmt = '(A10, F10.5)') "overlap", testOverlap
    write(unit = 100, fmt = '(/,A)') "Mean_of_Infor: "
    write(unit = 100, fmt = '(A10, F10.5)') "True", testMean_inforTrue
    write(unit = 100, fmt = '(A10, F10.5)') "Estimate", testMean_inforEstimate
    write(unit = 100, fmt = '(/,A)') "Psi_max"
    write(unit = 100, fmt = '(A10, F10.5)') "Set", psiMax
    write(unit = 100, fmt = '(A10, I10)') "alpha", alpha
    write(unit = 100, fmt = '(A10, F10.5)') "Max_1", psiOneMax
    write(unit = 100, fmt = '(A10, F10.5)') "Max_2", psiTwoMax
    write(unit = 100, fmt = '(A10, F10.5)') "Max_3", psiThreeMax
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
    ! == Psi Control Data ==
    open(unit = 100 , file = 'ListCAT_testPsiEta.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "eta = "
    write(unit = 100, fmt = dataPool) (j, j=1,numPool)
    do i=1,numTest
        write(unit = 100, fmt = dataPoolFS) (eta(j,i),j=1,numPool)
    end do
    close(100) 
    open(unit = 100 , file = 'ListCAT_testPsideltaCriteria.txt' , status = 'replace', action = 'write', iostat= ierror)
    write(unit = 100, fmt = '(A)') "delta change = "
    write(unit = 100, fmt = dataINT) (j, j=1,length)
    do i=1,numTest
        write(unit = 100, fmt = dataFS) (deltaCriteria(j,i),j=1,length)
    end do
    close(100)
    stop
end program D_MST_Psi

