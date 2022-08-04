program test
    implicit none
    ! === given data ====
    ! === 輸入資料設定 ===
    character(len = 50), parameter :: dataPath = "data/parameter_MST_len5_1-2-3-4_P.txt" !parameter_MST_1-3-3-3_data_P.txt !data/parameter_MST_1-2-3-4_data_P.txt
    character(len = 50), parameter :: dataPath2 = "data/Population_Normal.txt"  !Uniform Normal
    ! === MST set ===
    integer,parameter :: numStages = 4 !2 4
    integer, parameter :: maxModule =  20!有平行測驗時記得改 3 20 10
    integer, parameter :: numItemInModule = 5 !10
    !integer :: MSTdesign(numStages) = (/1,2,3,4/)
    integer :: MSTnump(numStages) = (/4,3,2,1/) !(/1,1/) !(/10,5/) (/1,1,1,1/) (/4,3,2,1/) (/3,1,1,1/) !每階段之每module平行測驗數
    integer :: MSTsum_items(numStages)
    ! === parameter ===
    integer,parameter :: numTest = 10000 !重複次數
    integer,parameter :: numPool = maxModule*numItemInModule !題庫數
    integer,parameter :: length = numStages*numItemInModule !作答題長
    integer,parameter :: numContentType = 3
    ! === 擴展試題 ===
    integer, parameter :: ori_numPool = 300
    integer :: ori_usedPool(ori_numPool, numTest) !紀錄試題是否被使用過
    integer :: ori_usedSum(ori_numPool, numTest) !試題被使用過的累加次數
    real:: ori_usedRate(ori_numPool)
    real:: ori_usedRateMax
    real:: ori_usedRateMean
    real:: ori_usedRateVar
    real:: ori_poolUsedRate
    ! === MST set 2 ===
    real :: rand_module
    integer :: randToInt
    !real :: rand_module(numStages, numTest)
    ! === item parameter ===
    real::a(numPool), b(numPool), c(numPool) !題庫試題參數
    integer:: content(numPool)
    integer:: stage(numPool)
    integer:: modules(numPool) 
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
        read(100,*) a(i),b(i),c(i),content(i),modules(i)!,&
        !level(i),pnum(i),itemID(i) !三參數
    enddo
    close(100)
    ! 輸入受試者真實能力值
    open(100, file= dataPath2, status="old") 
    read(100,*)  ! 跳過第一列
    do i=1,numTest
        read(100,*) x, thetaTrue(i) !三參數
    enddo
    close(100)

    do i=1,numStages  
        if (i==1)then
            MSTsum_items(i) = MSTnump(i)*i*numItemInModule
            do j = 1, MSTsum_items(i)
                stage(j) = i
            enddo
        else
            MSTsum_items(i) = MSTsum_items(i-1) + MSTnump(i)*i*numItemInModule
            do j = MSTsum_items(i-1)+1, MSTsum_items(i)
                stage(j) = i
            enddo
        endif
    enddo

    j = 1
    do i=1,numPool
        if (j > numItemInModule) then 
            j=1              
        endif 
        itemID(i) = j
        j = j+1       
    enddo

    j = 1
    do i=1,numPool
        if ((i > 1).AND.(modules(i-1) /= modules(i))) then
            j = j+1
            if (j > MSTnump(stage(i))) then 
                j=1              
            endif
        endif
        pnum(i) = j
    enddo

    j = 1
    do i=1,numPool
        if ((pnum(i) < pnum(i-1)).AND.(stage(i) == stage(i-1))) then 
            j = j + 1
        elseif(stage(i) /= stage(i-1))then
            j = 1
        endif
        level(i) = j
    enddo

    print *, stage,itemID,pnum, level


end program test