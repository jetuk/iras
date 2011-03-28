	subroutine readFlowFile(success)
	USE vars
	IMPLICIT NONE	
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
	!Local
	integer*2 iFlowFile,LineCounter,nRead
	integer*2 ierr,j
	logical*1 success
	character*10000 aLine
	!Call
	LOGICAL CountFileLines
!------------------------------------------------------------
	!Read flow data
	success=.false.
	iFlowFile = 22
      OPEN(UNIT=iFlowFile, FILE=TRIM(flwdataFileName), STATUS='old',
     &	 FORM='formatted', ERR= 999,ACTION='READ')
    		
      !Count the total number of .flw file
      LineCounter = 0
      IF(.NOT. CountFileLines(iFlowFile, LineCounter)) GOTO 999 
	
	!Allocate flowdata variable to number of lines of code, number of gauges nodes
	Allocate (flowdata(LineCounter,sysstat(NGAUGE1)))
	!Read inflows into memory from iras.flw file	
	nRead = 0       !no record read
	rewind (unit=iFlowFile)
	do WHILE (.TRUE.)
  	  READ(UNIT=iFlowFile,FMT='(A)',iostat=ierr) aLine
	  IF(ierr ==0) THEN 
		nRead=nRead+1
		READ(aLine,*)(FlowData(nRead,j), j=1, SysStat(NGAUGE1))	
        ELSE IF (ierr<0) THEN 
            EXIT
        else if (ierr>0) then
		continue	
	  END IF   
	end do
	sysstat(nRec)=nread

888   success = .true.
999   CLOSE(UNIT=iFlowFile)
      if (.not. success) then
        WRITE(*,*)'Flow data reading failed'
      end if
      return
	end subroutine

!**********************************************************
	
	Subroutine ReadFlwFactors(success)
!	Evgenii created this subroutine on 091104, it reads iras.flw
!	gauge flow factors for run into memory
      use vars
	implicit none
	INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  INPUT

      
!  OUTPUT
      LOGICAL*1 success
      !COMMON: flowdata
!  Local
      INTEGER*2 iFactfile,j,i,nr
	CHARACTER*10000 aLine
      CHARACTER*50 aVar
	character(len=50) GageFactName
!--------------------------------------------------------------
	success = .false.
	!Open flow factor files for gauge nodes with flow factors
!	do i=1,SysStat(NGAUGE1)
!		if (GageFact(i)) then
!		  GageFactName(i)=trim(GageName(i))//'.fac'
!		  outunit(i)=100+i
!		  OPEN(UNIT = outunit(i),FILE=GageFactName(i),STATUS='old',
!    &		  FORM='formatted')
!		endif
!	enddo

	Allocate(flowfactor(sysstat(NGAUGE1),sysstat(nRuns),12))
	
	!EVGENII - Must add check to make sure number of flowfactor records match number of runs
	!Initialize flow factos to 1
!	do j=1,gagmax
!		do k=1,12
!			flowfactor(j,k)=1.0
!		end do
!	end do
	iFactfile=100
	!Read flow factors into memory
  	do i=1,SysStat(NGAUGE1)
		!If Gauge node has flowfactors
		if (GageFact(i)) then
			!Name file
			GageFactName=trim(GageName(i))//'.fac'
		    !Read flow factors for each run from file into memory
			OPEN(UNIT = iFactfile,FILE=trim(GageFactName),
     &		      STATUS='old',FORM='formatted')

			do nr=1,sysstat(nRuns)		  			
			   READ(UNIT=iFactfile,FMT='(A)',ERR=999, END=999)aLine
     			   read(aLine,*)avar,(flowfactor(i,nr,j),j=1,12)
			end do 			
	    	CLOSE(UNIT=iFactfile)
		!If Gauge node doesnt have flowfactors
		else 
			do nr=1,sysstat(nRuns)		  
				do j=1,12; flowfactor(i,nr,j)=1 ;end do
			end do	
		  endif
	enddo


      success = .true.
 999  if (.not. success) then
        WRITE(*,*)'Flow factor reading failed ',GageFactName
      end if
      return
      end subroutine

!**********************************************************
	Subroutine readDemFile(success)
!	Evgenii created this subroutine on 091104, it reads iras.flw
!	  gauge flow data into memory
      USE vars

	implicit none

	INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  INPUT

      
!  OUTPUT
      LOGICAL*1 success
      !COMMON: flowdata
!  Local
	INTEGER*2 j,LineCounter, iDemFile
      INTEGER*2 nGagesInFile,nRead,ierr
	CHARACTER*10000 aLine
      CHARACTER*30 aVar
!  Call
	LOGICAL CountFileLines
!------------------------------------------------------------------------
      	!Read flow data
	success=.false.
	iDemFile = 33
      OPEN(UNIT=iDemFile, FILE='iras.dem', STATUS='old',
     &	 FORM='formatted', ERR= 999,ACTION='READ')
    		
      !Count the total number of .dem file
      LineCounter = 0
      IF(.NOT. CountFileLines(iDemFile, LineCounter)) GOTO 999 
	
	!Allocate flowdata variable to number of lines of code, number of gauges nodes
	Allocate (DemData(LineCounter,tDemSeries))
	!Read inflows into memory from iras.dem file	
	nRead = 0       !no record read
	rewind (unit=iDemFile)
	do WHILE (.TRUE.)
  	  READ(UNIT=iDemFile,FMT='(A)',iostat=ierr) aLine
	  IF(ierr ==0) THEN 
		nRead=nRead+1
		READ(aLine,*)(DemData(nRead,j), j=1, tDemSeries)	
        ELSE IF (ierr<0) THEN 
            EXIT
        else if (ierr>0) then
		continue	
	  END IF   
	end do

	if (sysstat(nRec)/=nread) then 
		write(*,*)'Demand time series not equal in length to flow time
     &	     time-series'
		goto 999
	end if
	
888   success = .true.
999   CLOSE(UNIT=iDemFile)
      if (.not. success) then
        WRITE(*,*)'Demand data reading failed'
      end if
      return
      end subroutine

	!Open .pol file. NOTE: the value of iDatafile should be different from others
      
	Subroutine readPolFile(success)
!	Evgenii created this subroutine on 091104, it reads iras.flw
!	  gauge flow data into memory
      USE vars

	implicit none
	
	INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  INPUT

      
!  OUTPUT
      LOGICAL*1 success
      !COMMON: flowdata
!  Local
	INTEGER*2 LineCounter
      INTEGER*2 nRead,ierr,ny,nn
	INTEGER*2 iPos, nNeedBufferedLinesInText

	CHARACTER*10000 aLine
      CHARACTER*30 aVar
!  Call
	LOGICAL CountFileLines

!Kang modify for improving performance
	!Initialize some gobal variables
!----------------------------------------------------------------
	
	iPolicyFile = 88
      OPEN(UNIT=iPolicyFile, FILE=TRIM(PolicyFileName), STATUS='old',
     &	 FORM='formatted', ERR= 999,ACTION='READ')
    		
      !Count the total number of .pol file
      LineCounter = 0
      IF(.NOT. CountFileLines(iPolicyFile, LineCounter)) GOTO 999
    		
      !If the total lines are less than the maximum of lines allowed for speed
      !allocate an array and then read all data into an array
      IF(LineCounter <= MaxLineForSpeed) THEN
        totalLinePolicyFile = LineCounter        
        IF(totalLinePolicyFile>0) THEN
            ALLOCATE (PolicyFileContent(totalLinePolicyFile))
        END IF
        
        !set the file position to the beginning of file
        REWIND(UNIT=iPolicyFile)
        
        LineCounter = 0
        
        DO WHILE (ALLOCATED(PolicyFileContent))
            READ(UNIT=iPolicyFile,FMT='(A)',iostat=ierr) aLine !Evgenii took out ERR=999
            IF(ierr ==0) THEN                 
                LineCounter = LineCounter+1
                PolicyFileContent(LineCounter) = TRIM(aLine)
            ELSE IF(ierr<0) THEN 
                EXIT
            END IF
          END DO
          
          pPolicyData => PolicyFileContent(:)
    	END IF
	success=.true.
999	continue
	End Subroutine
!************************************************************************
	Subroutine readINPFile(success)
      !This subroutine reads iras.inp	data into memory
      USE vars
	implicit none	
	INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  INPUT

      
!  OUTPUT
      LOGICAL*1 success
      !COMMON: flowdata
!  Local
	INTEGER*2 j,i,id,gagid,LineCounter, iDatafile
      INTEGER*2 nRead,ierr,ny,nn
	INTEGER*2 iPos, nNeedBufferedLinesInText

	CHARACTER*10000 aLine
      CHARACTER*30 aVar
!  Call
	LOGICAL CountFileLines

!Kang modify for improving performance
	!Initialize some gobal variables
!----------------------------------------------------------------
      !Kang add 20100629

	!write(*,*) 'Welcome to readinpdata. Enjoy your stay'
            
      cRead_Sim_Data = 0
      cSysEvap = 0
      cLinkRating = 0
      cLinkLoss = 0
      cCrossSection = 0
      cLinkRouting = 0
      cTransfer = 0
      cGWData = 0
      cNodeRating = 0
      cNodeEvap = 0
      cAllocation = 0
      cTarget = 0
      cTargetSource = 0
      cPowerPump = 0
      cReleaseRule = 0
      cGrpBalance = 0
      tRead_Sim_Data = 0



	NULLIFY(pFileData)
      nBufferedLines = 0
      nNeedBufferedLinesInText = 0
      Rating_BL = -1 
      Evaporation_BL = -1
      CrossSection_BL = -1
      Routing_BL = -1
      Groundwater_BL = -1
      Allocation_BL = -1
      Target_BL = -1
      Source_BL = -1
      Power_BL = -1
      Pump_BL = -1
      Rule_BL = -1
      Balance_BL = -1
      Performance_BL = -1
	  Cost_BL = -1      

      
      totalLinePolicyFile = 0
      NULLIFY(pPolicyData)
            
      bUseBufferedBinaryData = .FALSE.      
      nTarget  = 0
      NULLIFY(pTarget)
      nEvapor = 0
      NULLIFY(pEvapor)
      nRouting = 0
      NULLIFY(pRouting)
      nAllocation = 0
      NULLIFY(pAllocation)
      nSource = 0
      NULLIFY(pSource)
      nRating = 0
      NULLIFY(pRating)
      nCross = 0
      NULLIFY(pCross)
      nPerformance = 0
      NULLIFY(pPerformance)
      nCost = 0
      NULLIFY(pCost)
      nPower = 0
      NULLIFY(pPower)
      nPump = 0
      NULLIFY(pPump)
      nRule = 0
      NULLIFY(pRule)
      nDemRed=0				 !Added by Evgenii 101907 
      NULLIFY(pDemRed)		 !Added by Evgenii 101907 
      !Open .inp file. NOTE: the value of iDatafile should be different from others
	iDatafile = INPFileID



	!write(*,*) 'We are trying to open this file.'
	!write(*,*) 'Systemfilename is ', SysFilename
	!write(*,*) 'iDatafile is ', iDatafile
      !OPEN(UNIT=iDatafile, FILE=TRIM(SysFilename), STATUS='old',
    !&	 FORM='formatted', ERR= 999,ACTION='READ')
	REWIND (iDatafile) !JRK changed, 3 December 2010
	!write(*,*) 'We have opened this file.'
      
      !Count the total number of .inp file
      !Count the number of some parts in .inp file
      LineCounter = 0
      DO WHILE (.TRUE.)
        READ(UNIT=iDatafile,FMT='(A)',iostat=ierr) aLine !Evgenii took out ERR=999
        IF(ierr ==0) THEN 
            LineCounter = LineCounter+1
            IF(INDEX(aLine,headTarget)>0) THEN
                nTarget = nTarget+1
            ELSE IF(INDEX(aLine,headEvaporation)==1) THEN   
                nEvapor = nEvapor+1
            ELSE IF(INDEX(aLine,headRouting)==1) THEN   
                nRouting = nRouting+1
            ELSE IF(INDEX(aLine,headAllocation)==1) THEN   
                nAllocation = nAllocation+1
            ELSE IF(INDEX(aLine,headSource)==1) THEN   
                nSource = nSource+1
            ELSE IF(INDEX(aLine,headRating)==1) THEN   
                nRating = nRating+1
            ELSE IF(INDEX(aLine,headCross)==1) THEN   
                nCross = nCross+1
            ELSE IF(INDEX(aLine,headPerformance)==1) THEN   
                nPerformance = nPerformance+1
            ELSE IF(INDEX(aLine,headCost)==1) THEN   
                nCost = nCost+1
            ELSE IF(INDEX(aLine,headPower)==1) THEN   
                nPower = nPower+1
            ELSE IF(INDEX(aLine,headPump)==1) THEN   
                nPump = nPump+1
            ELSE IF(INDEX(aLine,headRule)==1) THEN   
                nRule = nRule+1
		  ELSE IF(INDEX(aLine,headDemRed)==1) THEN  !Added by Evgenii 101907  
                nDemRed = nDemRed+1

            ELSE
                nNeedBufferedLinesInText = nNeedBufferedLinesInText + 1
            END IF    
        ELSE IF (ierr<0) THEN 
            EXIT
        END IF    
      END DO
      
	!write(*,*) 'LineCounter before if ', LineCounter
	!write(*,*) 'MaxLineForSpeed ',MaxLineForSpeed
      !If the total lines are less than the maximum of lines allowed for speed
      !allocate an array and then read all data into an array
      IF(LineCounter <= MaxLineForSpeed) THEN
	  !write(*,*) 'nbufferedLines, before=', nBufferedLines
	  nBufferedLines = nNeedBufferedLinesInText
	  !write(*,*) 'nbufferedLines, after=', nBufferedLines
        bUseBufferedBinaryData = .TRUE.
        
        !Allocate memory of some parts of inp file
        !SysFileContent stores data in text format
        !TargetArray and EvapArray store data of "Target" in binary format
        IF(nBufferedLines>0) ALLOCATE (SysFileContent(nBufferedLines))
        IF(nTarget>0) ALLOCATE (TargetArray(nTarget))
        IF(nEvapor>0) ALLOCATE (EvapArray(nEvapor))
        IF(nRouting>0) ALLOCATE (RoutingArray(nRouting))
        IF(nAllocation>0) ALLOCATE (AlloArray(nAllocation))
        IF(nSource>0) ALLOCATE (SourceArray(nSource))
        IF(nRating>0) ALLOCATE (RatingArray(nRating))
        IF(nCross>0) ALLOCATE (CrossArray(nCross))
        IF(nPerformance>0) ALLOCATE (PerfArray(nPerformance))
        IF(nCost>0) ALLOCATE (CostArray(nCost))
        IF(nPower>0) ALLOCATE (PowerArray(nPower))
        IF(nPump>0) ALLOCATE (PumpArray(nPump))
        IF(nRule>0) ALLOCATE (RuleArray(nRule))
        IF(nDemRed>0) ALLOCATE (DemRedArray(nDemred))  !Added by Evgenii 101907 

        !set the file position to the beginning of file
        REWIND(UNIT=iDatafile)
        
        LineCounter = 0
        nTarget = 0
        nEvapor = 0
        nRouting = 0
        nAllocation = 0
        nSource = 0
        nRating = 0
        nCross = 0
        nCross = 0
        nPerformance = 0
        nCost = 0
        nPower = 0
        nPump = 0
        nRule = 0
        nDemred=0		 !Added by Evgenii 100719 
        DO WHILE (.TRUE.)
          READ(UNIT=iDatafile,FMT='(A)',iostat=ierr) aLine !Evgenii took out ERR=999
		IF(ierr ==0) THEN 
                iPos = INDEX(aLine,headTarget)
                IF(iPos>=1 .AND. ALLOCATED(TargetArray)) THEN

                    nTarget = nTarget + 1
                    iPos = iPos + LEN(headTarget)
                    strTemp = aLine(iPos:)
                    read(strTemp,*)TargetArray(nTarget)%GroupID,  
     &                                    TargetArray(nTarget)%Policy,
     &                                    TargetArray(nTarget)%CompType,
     &                                    TargetArray(nTarget)%NodeID, 
     &                                    TargetArray(nTarget)%targ,
     &                                    TargetArray(nTarget)%t_co, 
     &                                    TargetArray(nTarget)%EnvFlow,
     &								  TargetArray(nTarget)%RefilTrig,
     &   								    TargetArray(nTarget)%DemInc
                    
                    !IF(CountColumns(strTemp)==8) THEN
                    !  read(strTemp,*)aVar,aVar,aVar,aVar,aVar,aVar,aVar,
!     &              !                      TargetArray(nTarget)%RefilTrig
                    !ELSE
                    !    TargetArray(nTarget)%RefilTrig = 0.0
                    !END IF
				  
				                      
                CYCLE
                END IF
                
                iPos = INDEX(aLine,headEvaporation)
                IF(iPos==1 .AND. ALLOCATED(EvapArray)) THEN

                    nEvapor = nEvapor + 1
                    iPos = iPos + LEN(headEvaporation)
                    read(aLine(iPos:),*)EvapArray(nEvapor)%GroupID, 
     &                                    EvapArray(nEvapor)%Policy,
     &                                    EvapArray(nEvapor)%CompType,
     &                                    EvapArray(nEvapor)%ID, 
     &                                    EvapArray(nEvapor)%Evaporation
                    IF(EvapArray(nEvapor)%CompType == 2) THEN
                        read(aLine(iPos:),*)aVar,aVar,aVar,aVar,aVar,
     &                                    EvapArray(nEvapor)%LossMethod
                    ELSE
                        EvapArray(nEvapor)%LossMethod = 0
                    END IF
                CYCLE
                END IF
                
                iPos = INDEX(aLine,headRouting)
                IF(iPos>=1 .AND. ALLOCATED(RoutingArray)) THEN

                    nRouting = nRouting + 1
                    iPos = iPos + LEN(headRouting)
                    read(aLine(iPos:),*)RoutingArray(nRouting)%GroupID, 
     &                                RoutingArray(nRouting)%Policy,
     &                                RoutingArray(nRouting)%CompType,
     &                                RoutingArray(nRouting)%LinkID, 
     &                                RoutingArray(nRouting)%iMethod,
     &                                RoutingArray(nRouting)%L_NRTRES,
     &                                RoutingArray(nRouting)%L_a,
     &                                RoutingArray(nRouting)%L_b,
     &                                RoutingArray(nRouting)%L_c
                CYCLE
                END IF

                iPos = INDEX(aLine,headAllocation)
                IF(iPos>=1 .AND. ALLOCATED(AlloArray)) THEN

                    nAllocation = nAllocation + 1
                    iPos = iPos + LEN(headAllocation)
                    read(aLine(iPos:),*)AlloArray(nAllocation)%GroupID, 
     &                                AlloArray(nAllocation)%Policy,
     &                                AlloArray(nAllocation)%CompType,
     &                                AlloArray(nAllocation)%NodeID, 
     &                                AlloArray(nAllocation)%NodeOutput,
     &                                AlloArray(nAllocation)%LinkID,
     &                                AlloArray(nAllocation)%LinkAllo
                CYCLE
                END IF
                iPos = INDEX(aLine,headSource)
                IF(iPos>=1 .AND. ALLOCATED(SourceArray)) THEN
                    nSource = nSource + 1
                    iPos = iPos + LEN(headSource)
                    read(aLine(iPos:),*)SourceArray(nSource)%GroupID, 
     &                                SourceArray(nSource)%Policy,
     &                                SourceArray(nSource)%CompType,
     &                                SourceArray(nSource)%NodeID, 
     &                                SourceArray(nSource)%Supl_Node,
     &                                SourceArray(nSource)%Supl_Frac
                CYCLE
                END IF
                
                iPos = INDEX(aLine,headRating)
                IF(iPos>=1 .AND. ALLOCATED(RatingArray)) THEN
                    nRating = nRating + 1
                    iPos = iPos + LEN(headRating)
                    read(aLine(iPos:),*)RatingArray(nRating)%GroupID,
     &                                RatingArray(nRating)%Policy,
     &                                RatingArray(nRating)%CompType,
     &                                RatingArray(nRating)%ID, 
     &                                RatingArray(nRating)%ElevOrWidth,
     &                                RatingArray(nRating)%AreaOrEvapor,
     &                                RatingArray(nRating)%VolOrFlow
                        
                    IF(RatingArray(nRating)%CompType == 1) THEN
                       read(aLine(iPos:),*)aVar,aVar,aVar,aVar,aVar,
     &                                aVar,aVar,
     &                                RatingArray(nRating)%Seep,
     &                                RatingArray(nRating)%MaxQ,
     &                                RatingArray(nRating)%LakeQ
                    ELSE
                       RatingArray(nRating)%Seep = 0.0
                       RatingArray(nRating)%MaxQ = 0.0
                       RatingArray(nRating)%LakeQ = 0.0
                    END IF
                CYCLE
                END IF
  
                iPos = INDEX(aLine,headCross)
                IF(iPos==1 .AND. ALLOCATED(CrossArray)) THEN
                    nCross = nCross + 1
                    iPos = iPos + LEN(headCross)
                    read(aLine(iPos:),*)CrossArray(nCross)%GroupID, 
     &                                CrossArray(nCross)%Policy,
     &                                CrossArray(nCross)%CompType,
     &                                CrossArray(nCross)%LinkID, 
     &                                CrossArray(nCross)%BaseWidth,
     &                                CrossArray(nCross)%ChannelDepth, 
     &                                CrossArray(nCross)%LSlope,
     &                                CrossArray(nCross)%RSlope,
     &                                CrossArray(nCross)%UpLSlope,
     &                                CrossArray(nCross)%UpRSlope
                CYCLE
                END IF
                
               iPos = INDEX(aLine,headPerformance)
                IF(iPos>=1 .AND. ALLOCATED(PerfArray)) THEN
                    nPerformance = nPerformance + 1
                    iPos = iPos + LEN(headPerformance)
                    read(aLine(iPos:),*)PerfArray(nPerformance)%GroupID,
     &                              PerfArray(nPerformance)%Policy,
     &                              PerfArray(nPerformance)%NodeID, 
     &                              PerfArray(nPerformance)%thres_limit
                CYCLE
                END IF
                
                iPos = INDEX(aLine,headCost)
                IF(iPos==1 .AND. ALLOCATED(CostArray)) THEN
                    nCost = nCost + 1
                    iPos = iPos + LEN(headCost)
                    read(aLine(iPos:),*)CostArray(nCost)%GroupID,
     &                              CostArray(nCost)%Policy,
     &                              CostArray(nCost)%LinkID, 
     &                              CostArray(nCost)%FlowCost,
     &                              CostArray(nCost)%FlowEng,
     &							  CostArray(nCost)%AnnCostInc
                CYCLE
                END IF

               iPos = INDEX(aLine,headPower)
                IF(iPos>=1 .AND. ALLOCATED(PowerArray)) THEN
                    nPower = nPower + 1
                    iPos = iPos + LEN(headPower)
                    read(aLine(iPos:),*)PowerArray(nPower)%GroupID,
     &                              PowerArray(nPower)%Policy,aVar,
     &                              PowerArray(nPower)%NodeID, 
     &                              PowerArray(nPower)%HPCAP,
     &                              PowerArray(nPower)%PLANT_FACTOR,
     &                              PowerArray(nPower)%Intake_elev,
     &                              PowerArray(nPower)%Turbine_elev,
     &                              PowerArray(nPower)%Outlet_elev,
     &                              PowerArray(nPower)%ECONST,
     &                              PowerArray(nPower)%HpQMin
                CYCLE
                END IF

                iPos = INDEX(aLine,headPump)
                IF(iPos>=1 .AND. ALLOCATED(PumpArray)) THEN
                   nPump = nPump + 1
                    iPos = iPos + LEN(headPump)
                    read(aLine(iPos:),*)PumpArray(nPump)%GroupID,
     &                              PumpArray(nPump)%Policy,aVar,
     &                              PumpArray(nPump)%NodeID, 
     &                              PumpArray(nPump)%HPCAP,
     &                              PumpArray(nPump)%PLANT_FACTOR,
     &                              PumpArray(nPump)%Intake_elev,
     &                              PumpArray(nPump)%Turbine_elev,
     &                              PumpArray(nPump)%Outlet_elev,
     &                              PumpArray(nPump)%ECONST,
     &                              PumpArray(nPump)%HpQMin
                CYCLE
                END IF

                iPos = INDEX(aLine,headRule)
                IF(iPos>=1 .AND. ALLOCATED(RuleArray)) THEN
                    nRule = nRule + 1
                    iPos = iPos + LEN(headRule)
				  read(aLine(iPos:),*)RuleArray(nRule)%GroupID,
     &                              RuleArray(nRule)%Policy,
     &                              RuleArray(nRule)%NodeID, 
     &                              RuleArray(nRule)%res_rule(1),
     &                              RuleArray(nRule)%res_rule(2),
     &                              RuleArray(nRule)%res_rule(3),
     &                              RuleArray(nRule)%res_rule(4),
     &                              RuleArray(nRule)%res_rule(5),
     &                              RuleArray(nRule)%res_rule(6),
     &                              RuleArray(nRule)%res_rule(7),
     &                              RuleArray(nRule)%res_rule(8)                   				
                CYCLE
                END IF
                
			  iPos = INDEX(aLine,headDemRed)		   !Added by Evgenii 100719 
                IF(iPos>=1 .AND. ALLOCATED(DemRedArray)) THEN
                    nDemRed = nDemRed + 1
                    iPos = iPos + LEN(headDemRed)
                    read(aLine(iPos:),*)DemRedArray(nDemRed)%GroupID,
     &                            DemRedArray(nDemRed)%Policy,
     &                            DemRedArray(nDemRed)%NodeID, 
     &                            DemRedArray(nDemRed)%DemSourceID,
     &							DemRedArray(nDemRed)%Dem_Thres_limit,
     &                            DemRedArray(nDemRed)%DemRedAmt
     							


                CYCLE
                END IF


                LineCounter = LineCounter+1
                SysFileContent(LineCounter) = TRIM(aLine)
            ELSE IF(ierr<0) THEN 
                EXIT
            END IF
          END DO
          IF(ALLOCATED(SysFileContent)) pFileData => SysFileContent(:)
          IF(ALLOCATED(TargetArray)) pTarget => TargetArray(:)
          IF(ALLOCATED(EvapArray)) pEvapor => EvapArray(:)
          IF(ALLOCATED(RoutingArray)) pRouting => RoutingArray(:)
          IF(ALLOCATED(AlloArray)) pAllocation => AlloArray(:)
          IF(ALLOCATED(SourceArray)) pSource => SourceArray(:)
          IF(ALLOCATED(RatingArray)) pRating => RatingArray(:)
          IF(ALLOCATED(CrossArray)) pCross => CrossArray(:)
          IF(ALLOCATED(PerfArray)) pPerformance => PerfArray(:)
          IF(ALLOCATED(CostArray)) pCost => CostArray(:)
          IF(ALLOCATED(PowerArray)) pPower => PowerArray(:)
          IF(ALLOCATED(PumpArray)) pPump => PumpArray(:)
          IF(ALLOCATED(RuleArray)) pRule => RuleArray(:)
		IF(ALLOCATED(DemRedArray)) pdemRed => DemRedArray(:)	
					 !Added by Evgenii 100719 
	end if
	rewind (iDatafile)
	success=.true.
999	continue
	return
	end subroutine

!************************************************************************
!Kang add for improving performance.
      LOGICAL FUNCTION CountFileLines(iFile, iLines)
!  INPUT
      INTEGER*2 iFile
!  OUTPUT
      INTEGER*2 iLines
      LOGICAL bSuccess
!  LOCAL
      CHARACTER*256 aLine
!------------------------------------------------------------------------
      bSuccess = .FALSE.
      iLines = 0
      DO WHILE (.TRUE.)
        READ(UNIT=iFile,FMT='(A)',iostat=ierr) aLine !Evgenii took out ERR=999
        IF(ierr ==0) THEN 
            iLines = iLines+1
        ELSE IF (ierr<0) THEN 
            EXIT
        END IF    
      END DO
      
      bSuccess = .TRUE.
      
999   CountFileLines = bSuccess

      END FUNCTION
      
!************************************************************************
!Kang add for improving performance.
      INTEGER FUNCTION CountColumns(aLine)
!  INPUT
      CHARACTER*256 aLine
!  OUTPUT
      INTEGER nColumns
!  LOCAL
      INTEGER I, nLen
      LOGICAL bStart
      CHARACTER*256 strTemp
      CHARACTER aChar
!------------------------------------------------------------------------
      nColumns = 0
      strTemp = aLine
      nLen = LEN(strTemp)
      bStart = .FALSE.
      DO I=1, nLen
        aChar = strTemp(I:I)
        IF(ICHAR(aChar)==32 .OR. ICHAR(aChar)==9) THEN
            IF(bStart) THEN
                nColumns = nColumns + 1
                bStart = .FALSE.
            END IF
        ELSE 
            IF(ICHAR(aChar)==10 .OR. ICHAR(aChar)==13 .OR. 
     &         aChar=='!') THEN
                IF(bStart) THEN
                    nColumns = nColumns + 1
                    bStart = .FALSE.
                    EXIT
                END IF
            ELSE
                bStart = .TRUE.
            END IF     
        END IF         
      END DO
      
      CountColumns = nColumns

      END FUNCTION      