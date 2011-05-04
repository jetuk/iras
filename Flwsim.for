!Copyright (c) 2009, 2010 by University College London, Cornell University
!Authors:
!G Pegram, Daniel P. Loucks (dpl3@cornell.edu), Marshall Taylor, Peter French, Huicheng Zhou
!Evgenii Matrosov (evgenii.matrosov@ucl.ac.uk), Julien Harou (j.harou@ucl.ac.uk), Anil Dikshit
!This program is free software under the General Public Licence, GPL (>=v2)
!Read the 'GPL License.txt' file distributed with this source code for a full license statement.
!
!	 *************************************************************************************
      SUBROUTINE FLWSIM(PolicyGrp, YR, iDay, success)
C
C     NOTE SIMSYS also calls with DAYS(T) as DAYS Per this Time period
C          CONVRT now does the job that NODFLW previously performed
C          and produces the incremental and total unregulated flows
C          (as average daily values) for each within-year period,
C          year and replicate, for all nodes and writes those data
C          into a TVF formatted file.
C
C
C
C      USE:  Performs the sub-time step flow simulation
C            for a within-year time period T.
C            Loop over number of defined sub-time steps for this
C            time step (defined by SYSSTAT(time)), starting
C            at 1+SYSSTAT(NSUBTT) to account for possible backtracking.
C
C      CALLED BY:  SIMSYS (once per sub-time step)
C
C      CALLS:      SetCurrentPolicy,SetDefaultLoss,Read_Simulation_Data
C                  GetTotalFlow, GetIncrementalFlow, ReleaseFromDeficit,
C                  RELEASE, GetLoss, NodeInflowFromInlinks,NodeOutflow,
C                  OutflowAllocation,GWLinkNodeSim
C
C      OUTPUT:     ESTO for all nodes NN, EQLN for all links LN
C                  also: BQLN, TEVAPN, QINN,  INFLOW,
C                  TOTREL
C
C      NOTES:      Merged the program with the old SIMNOD.
C                  Changed to perform daily simulation.
C
C
C
C      Source file:      FLWSIM.FOR
C      Creation:         27 Feb 1989
C      Author:           G Pegram, dpl
C      Modifications (yymmdd):
C      Anil Dikshit      Changed QINN(NN) to QINN(NN,K)
C      DPL:              Changed to perform daily simulations. 911230
C      DPL:              Added quality and groundwater. 921017
C      PNF:921115        Dropped all K's.
C      MRT:921201        Cleaned - removed used of NODFLW.
C      MRT:930318        Added SEQ_NOD array and check to make sure
C                        that link is not a "loop" link before setting
C                        the daily end flow of the link to 0.0 at the
C                        start of the simulation time steps loop.
C                        Also, added limit to all diversion links so
C                        that link flow (rather from target demand or
C                        normal allocation) can not exceed the link
C                        capacity. (Capacity is max flow not volume).
C      PNF:930322        Allow makeup of inter-period deficit
C      MRT:930324        Some fixes on interperiod deficits.
C      PNF:931104        Add check for QT_FAIL
C      MRT:940316        Fixed link capacity limits to account for
C                        conversion from user to internal units.
C      MRT:940802        Bi-directional surface water links.
C      MRT:941024        Changed computation of Expected Inflows to Demand Nodes
C      PNF:941205        Message for QT_FAIL moved up to SIMSYS
C      PNF:941213        QT_FAIL does not disable quality
C      MRT:950102        Fixed error in demand link allocations for non-storage
C                        nodes which have multiple incoming links.
C      HCZ:200004        Read all simulation data from text files
C                        instead of the system database file (.SDB)
C                        which is a binary file.
!      EM:091101		   Took out gauge multipliers
!
!	 *************************************************************************************
C        DECLARATIONS:
      IMPLICIT NONE
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
   !INPUT
      !Kang modify 20100630
      INTEGER*2 PolicyGrp, YR, iDay
      !YR--current year, e.g. 1998, it can be 1,2,..., depending on year format in flow file
      !iDay--1,...,365
      !COMMON: CHARACTER*80 FlowFileName , SysFileName
      !        TNODES,Links,NODSEQ(J),DayPerTS
      !        ...
   !OUTPUT
      LOGICAL*1 success
C
C    Functions:
      INTEGER*2 FINTERP
C
C    Local variables:
      INTEGER*2 I, NX, NN, LI, LN, LO, ST, OUT,ioutnodes,ioutLinks,
     1          SEQ_NOD(NODMAX), J, NI, NO, InflowDay,STEP
      REAL*4    DINFLW(NODMAX), DSTO(NODMAX), DTREL(NODMAX),AW(NODMAX),
     1          SUPL_RELEAS(NODMAX), TOT_SUPL_R(NODMAX)
      REAL*4    DS, InStor, OutStor, REL
      REAL*4    DQINN, CAPACITY, ADJ, NON_DMD, FLO_IN,
     1          DCONSUMP, TMP_REL, AVAIL_CAP
      LOGICAL*1 HAVDIV, L_SOLVED(LNKMAX)
!     INFLOWTS(NN): total inflow within current time-step

!-------------------------------------------------------------------------
      success = .false.
      IF (STEPPRPRD .LT. 0) GO TO 9999

C     Initialize system power variable
      SYSPWR = 0.

C     Build an array which gives the simulation sequence order.
      DO I = 1, TNODES
        DO J = 1, TNODES
          IF(NODSEQ(J).EQ.I)SEQ_NOD(I) = J
        ENDDO
      ENDDO
C
C     Initialize total period link flows and energy variables
      DO LN = 1,LINKS
        BQLN(LN) = 0.
        EQLN(LN) = 0.
        ENERGY(LN) = 0.
        TOT_LVOL(LN) = 0.
        TLossL(LN) = 0.
      ENDDO
C
      DO NN = 1, TNODES
C        Define initial storage based on previous period's final storage.
         BSTO(NN) = ESTO(NN)
         DSTO(NN) = BSTO(NN)
         AW(NN)   = 0.0
C        Initialize total period evaporation and inflow variables.
         TEVAPN(NN) = 0.
         TSEEPL(NN) = 0.
         !inflow1(nn)=inflow(nn)
	   INFLOW(NN) = 0.
	   releaseTS(nn)=0. !Added by Evgenii 100608
         TOTREL(NN) = 0.
         DTREL(NN)  = 0.
         DINFLW(NN) = 0.
         CONSUMPTION(NN) = 0.0
	   TOT_SUPL_R1(NN)=TOT_SUPL_R(NN)
         TOT_SUPL_R(NN) =  0.0
	   STEP_DEFICIT(NN) = 0.0 !Added by Evgenii
      ENDDO
C ___________________________________________________________________
!	EVGENII - Activate four lines below (and calls for DayOutputNodeTS and DayOutputLinkTS
!	towards end of subroutine) for diagnostic sub-time step OUTPUT
	!ioutnodes=30 
	!ioutlinks=40                                                 
	!OPEN(UNIT = iOutnodes, FILE ='NodesTS.out', STATUS='replace') 
	!OPEN(UNIT = iOutlinks, FILE ='LinksTS.out', STATUS='replace') 
	!Evgenii 091106 changed NQinn into Qinn because no added flow is used below
!     compute natural flows for current day to NQINN(TNodes)

	Call GetTotalFlow(QInn,Success)  		   
      IF(.not.success) GOTO 9999      
 !-------Evgenii took following code out because its not needed now that gage multipliers are out, also no added flow is used 091101     
	!call GetIncrementalFlow(NQInn, Success) !This doesnt actually do anything but set the total flow to the natural flow Evgenii
      !IF(.not.success) GOTO 9999
      !Compute added flows for current day to AddQINN(TNodes)
      !Added flow is an additional flow, do not need to compute flow incremental
      ! Call GetTotalFlow(FlowFileName, 2, SysStat(Year),        !1-added flow !Evgenii took this out 090722
      !&           SysStat(Month),SysStat(Day), AddQInn, Success)
      !IF(.not.success) GOTO 9999
      !sum of incremental flows
      !do i = 1,TNodes
      ! Qinn(i) = NQinn(i) !+ AddQinn(i) Evgenii took out AddQinn 090729
      !end do
!-------------------------------------------------------------------------------------------------

      !Set new Policies for all nodes and links when policies need changed
      call SetCurrentPolicy(iday)

      !get default loss of system before simulation data reading                   
      call SetDefaultLoss(iday)

      !automatically check whether data reading is needed
      !Kang modify 20100630
      !call Read_Simulation_Data(SysFileName,PolicyGrp,success)
      call Read_Simulation_Data(PolicyGrp,success)
	
	call GetDemandFlow()

	!Add last time steps carryover deficit, added by Evgenii 030310
!	DO NN = 1, TNODES
!		IF (CAPN(NN) == 0.0 .and. LAST_STEP_DEFICIT(NN)>0.0)
!     &		DMD_TARG(NN)=DMD_TARG(NN)+	(NN)/DAYSPRPRD 
!	END DO
	
	!LAST_STEP_DEFICIT is converted into mil m3/day to match DMD_TARG units, Evgenii
!*** Begin daily simulation for each node in the sequence:
      DO STEP = 1,STEPPRPRD
	  sysstat(sstep)=step
	  DO LN = 1,LINKS
          L_SOLVED(LN) = .FALSE.
          DBQLN(LN) = 0.
          NI = NIN(LN)
          NO = NOUT(LN)
          IF (NI.GT.0 .AND. NO.GT.0) THEN
            IF (SEQ_NOD(NO) .GT. SEQ_NOD(NI)) DEQLN(LN) = 0.
          END IF
        ENDDO

C       Initialize supplemental releases for this step
        DO NN = 1,TNODES
          SUPL_RELEAS(NN) = 0.0
          InflowTS(NN) = 0
        END DO

        !Compute SUPL_RELEAS(NN),TOT_SUPL_R(NN) from step's deficit and target
	  call ReleaseFromDeficit(SUPL_RELEAS,TOT_SUPL_R,DSTO,STEP) 
       
	  !Obtain lake and reservoir target releases based on beginning of
        !day storages LESS (minimum release requirements, downstream deficit
        !requirements, and supply-driven release rules and balancing functions).
        CALL RELEASE(DSTO,SUPL_RELEAS,DTREL)
	 
        !Compute evaporation, seepage losses:
        !Update DSTO(NN), TEVAPN(NN), TSEEPL(NN)
        call GetLoss (DSTO)
C
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx	
C
!       Now compute total natural and surface water inflows and
C       resulting water available (DSTO) at each node.
        DO 300 NX = 1,TNODES
          NN = NODSEQ(NX)
          IF (NN .EQ. 0) GO TO 300
C
          !** Compute node inflow plus storage (after losses)
          DQINN = QINN(NN) * DayPerTS !Evgenii - QINN is gage flow
          DSTO(NN) = DSTO(NN) + DQINN 
C
C         Total net node inflow initialized to incremental inflow
          DINFLW(NN) = DQINN
C
C
          IF (GWNODE(NN)) GO TO 300             !Groundwater nodes
		!Evgenii - GWNODE also wetlands

          !get node inflow from all inlinks including GW links
          IF (TOTIN(NN) .gt. 0)            !node has inlinks
     &        call NodeInflowFromInlinks(NN,DSTO,DINFLW,AW,L_SOLVED)
C         Accumulate total inflow
          INFLOW(NN) = MAX(0.0, INFLOW(NN)+DINFLW(NN))
          InflowTS(NN) = MAX(0.,InflowTS(NN)+ DINFLW(NN))
		!inflow1(nn)=InflowTS(NN)
          !get node outflow including GW links
          call NodeOutflow(NN,DSTO,DTREL,AW,L_SOLVED)
C
C-----------------------------------------------------------------------
C         Now we have release from each sw node identified and final
C         storage volumes computed.
C
C         Following section allocates release to outgoing
C         surface water links and computes link hydropower.
C         Compute flows in links going from node. (based on DSTO)
          !DTREL(NN)--total release out of node NN
          call OutflowAllocation(NN,DTREL,DSTO,L_SOLVED)	
300     continue
C       over all nodes in sequence.
C
        !Compute gw link flows on links connecting two gw nodes.
        !Compute gw node storage
        call GWLinkNodeSim(DSTO,DINFLW,L_SOLVED)

C       Now all groundwater nodes and links have been simulated.
C       Now all storage volumes and flows and hydropower have been simulated.
C       Now all nodes and links have been simulated: Accumulated BQLN, EQLN
        DO LN = 1,LINKS
          BQLN(LN) = BQLN(LN) + DBQLN(LN)
          EQLN(LN) = EQLN(LN) + DEQLN(LN)
          TOT_LVOL(LN) = TOT_LVOL(LN) + LNKVOL(LN)*DAYPERTS
        ENDDO

	!   EVGENII - Activate below for diagnostic sub-time step OUTPUT
        !call DayOutputNodeTS(ioutNodes,iDay,NodSEQ,step,DSTO) 
	  !call DayOutputLinkTS(ioutLinks,iDay,step) 
	 ENDDO !End do for all steps per period !Evgenii
C
	  !Evgenii 102701 added do loop around STEP_DEFICIT calculation
	  !so that it would adjust STEP_DEFICIT for each node.
	if (sysstat(18)==730) then
		continue
	end if
 
	 DO NN=1,TNODES
	  If(DMDNODE(nn) .and. capn(nn)==0.0) then
	    !calculate shortage for time-step, evgenii 100623
		TSflw_DEFICIT(NN) = dmd_targ(nn)*DAYSPRPRD - INFLOW(NN) 
		TSflw_DEFICIT(NN) = MAX(0.0, TSflw_DEFICIT(NN))
	  !Adjust step-deficit for carryover fraction Evgenii
		!LAST_STEP_DEFICIT(NN) = STEP_DEFICIT(NN)*DMD_T_CO(NN) 
	  endif
	 ENDDO
C      Define final storage volumes ESTO, convert volume totals to daily rates
       DO NN = 1,TNODES
CMRT941107+
C       The following statements set ESTO to zero for non-storage nodes
C       and then assign ESTO equal to the computed final storage only in the
C       case of a storage node and then will not assign negative storages.
C       This approach is taken to solve numerical rounding problems where
C       the final DSTO has values of scale 1*10**-7 or so because of
C       interpolation of the allocation functions. There is danger that
C       these statements will mask another simulation error.  Whenever an
C       error is suspected replace them with ESTO(NN) = DSTO(NN) to check
C       it out.
C       ESTO(NN) = DSTO(NN)
        ESTO(NN) = 0.0
        IF(CAPN(NN).GT.0.0 .OR. GWNODE(NN))ESTO(NN) = MAX(0.0, DSTO(NN)) !Evgenii added .OR. GWNODE(NN)
CMRT941107-
	  
        !Convert units from /time-step /day

	  !Call performance measures calculations at end of time step, added by Evgenii 10616
	  call StoragePerformance()
	  !Link Cost calculations at end of time step, added by Evgenii 10713
	  call LinkCost_and_Power()

	  !Convert units from Mm3/time-step to what guage input units were

	  INFLOW(NN) = INFLOW(NN) /(GageUserUnit(1)*DAYSPRPRD)
        TEVAPN(NN) = TEVAPN(NN) /(GageUserUnit(1)*DAYSPRPRD)
        TSEEPL(NN) = TSEEPL(NN) /(GageUserUnit(1)*DAYSPRPRD)
        TOTREL(NN) = TOTREL(NN) /(GageUserUnit(1)*DAYSPRPRD)
        CONSUMPTION(NN) = CONSUMPTION(NN) /(GageUserUnit(1)*DAYSPRPRD)
	
	

       ENDDO
C
       DO LN = 1,LINKS
        ENERGY(LN) = ENERGY(LN) !/(GageUserUnit(1)*DAYSPRPRD)
        BQLN(LN)   = BQLN(LN) /(GageUserUnit(1)*DAYSPRPRD)
        EQLN(LN)   = EQLN(LN) /(GageUserUnit(1)*DAYSPRPRD)
        TOT_LVOL(LN) = TOT_LVOL(LN) !/DAYSPRPRD !Evgenii took out /DAYSPRPRD, volume shouldn't be converted
	  TLossL(ln)=  TLossL(ln)/(GageUserUnit(1)*DAYSPRPRD)

       ENDDO

	

C
C      Other values available after this routine:
C      BQLN, EQLN, TEVAPN, QINN, INFLOW, TOTREL, DBQLN, DEQLN, TSEEPL
 	 success = .true.
9999   RETURN
       END


!*************************************************************************
      subroutine NodeInflowFromInlinks(NN,DSTO,DINFLW,AW,L_SOLVED)
! Get node inflow from all inlinks
! Seperated from FLWSIM.FOR
      IMPLICIT NONE
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  CALL: GWLNKQ, HYDSIM
!  INPUT
      INTEGER*2 NN
      REAL*4    DSTO(NODMAX), DINFLW(NODMAX), AW(NODMAX)
      LOGICAL*1 L_SOLVED(LNKMAX)
      !COMMON: GWNODE(NN), TOTIN(NN), INFLOW(NN)
      !        INLINK(NN,LI),GWLINK(LN), DEQLN(LN), DBQLN(LN)
      !        NIN(LN), NOUT(LN), PowerLink(LN), HPCAP(LN)
!  OUTPUT
      !LOGICAL*1 L_SOLVED(LNKMAX)
      !REAL*4    DSTO(NODMAX), DINFLW(NODMAX), INFLOW(NN), INFLOWTS(NN)
      !COMMON:   DEQLN(LN), DBQLN(LN), TOTREL(NN)
!  Local
      INTEGER*2 LI, LN, S_NODE 
!-------------------------------------------------------------------------

      !** Find incoming surface water link flows for surface water nodes.
C         Note: Up stream most nodes have no incoming links.
C               Down stream nodes will have their ending link flows
C               defined except for last links in loop.  For loops,
C               this program will pick up previous day's value.
C                        (small error, hopefully!)
          IF (TOTIN(NN) .le. 0) return
      ! get flow from surface water links
          DO LI = 1,TOTIN(NN)
              LN = INLINK(NN,LI)
              IF (.NOT. GWLINK(LN))THEN
              DSTO(NN) = DSTO(NN)+DEQLN(LN) 
              DINFLW(NN) = DINFLW(NN) + DEQLN(LN)
             ENDIF
          ENDDO
C
      !** Now DSTO is total water available at each node from surface
C         water system.  (Not including gw inputs and outputs.)
C         Compute net inflows from groundwater links, if any
          DO LI = 1,TOTIN(NN)
            LN = INLINK(NN,LI)
            !Evgenii - 090818 if GWlink(i)=true, it just means its  bi-directional, not neccessirally GW LINK!
		  IF (GWLINK(LN)) THEN 
C             Only solve the link if it has not previously been
C             solved and only compute hydropower/pumping on
C             unsolved links.
              IF(.NOT. L_SOLVED(LN))THEN
C               Compute gw link flows based on current values of DSTO
                IF(GWNODE(NIN(LN)))AW(NIN(LN))=DSTO(NIN(LN)) 
                AW(NIN(LN))=MAX(0.0,AW(NIN(LN)))
                CALL GWLNKQ(LN,AW(NIN(LN)),DSTO(NOUT(LN)),DBQLN(LN))
                DEQLN(LN) = DBQLN(LN)
              ENDIF
              IF (DEQLN(LN) .GT. 0.) THEN !Evgenii - flow is positive so its inflow 
C                Actual inflow.  Update inflow
                 INFLOW(NN) = INFLOW(NN)+DEQLN(LN)
                 InflowTS(NN) = InflowTS(NN)+ DEQLN(LN)
              ELSE  !flow  is negative, so its release
C                Actual outflow.  Update outflow (DEQLN here is <0)
                 TOTREL(NN) = TOTREL(NN)-DEQLN(LN)
              END IF
C
              !**Compute hydropower and pumping
              IF (.NOT.L_SOLVED(LN).and.
     1            ((PowerLink(LN) .and.HPCAP(LN).GT.0.)
     &            .or.(PumpLink(LN) .and.PConst(LN).GT.0.))) THEN
                CALL HYDSIM(LN,DSTO(NIN(LN)),DSTO(NOUT(LN)),DBQLN(LN))
              END IF
C
C             Update storage at node
              DSTO(NN) = DSTO(NN) + DEQLN(LN)
              L_SOLVED(LN) = .TRUE. !Evgenii 100710 moved L_SOLVED(LN) = .TRUE. into if loop bc only gw links are solved here
		   END IF !End if GWlink (bi-directional)
           
          ENDDO
      end subroutine


!*************************************************************************
      subroutine NodeOutflow(NN,DSTO,DTREL,AW,L_SOLVED)
! Get node inflow from all inlinks
! Seperated from FLWSIM.FOR
!    Evgenii- Computes total outflow of non-aquifer, non-wetland links.
!			First computes bi-directional links then surface water.
!			For surface water it releases either the previously 
!			determined releases (DTREL), if there isnt enough to 
!			to cover DTREL it releases all the storage (DTSO)
!		    or if node is over capacity (CAPN) it releases the amount 
!			over capacity or DTREL which ever is greater
!			It updates DTREL by adding the release to it
! 
      IMPLICIT NONE
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  CALL: GWLNKQ, HYDSIM
!  INPUT
      INTEGER*2 NN
      REAL*4    DSTO(NODMAX),DTREL(NODMAX),AW(NODMAX)
      LOGICAL*1 L_SOLVED(LNKMAX)
      !COMMON: GWNODE(NN), TOTOUT(NN)
      !        GWLINK(LN), DEQLN(LN), DBQLN(LN)
      !        NIN(LN), NOUT(LN), PowerLink(LN), HPCAP(LN)
!  OUTPUT
      !LOGICAL*1 L_SOLVED(LNKMAX)
      !REAL*4    DSTO(NODMAX),DTREL(NODMAX),AW(NODMAX),INFLOW(NN),INFLOWTS(NN)
      !COMMON:   DEQLN(LN), DBQLN(LN), TOTREL(NN)
!  Local
      INTEGER*2 LO, LN 
      REAL*4    DS
!-------------------------------------------------------------------------
          IF (TOTOUT(NN) .le. 0) goto 555
          DO LO = 1,TOTOUT(NN)
              LN = OUTLNK(NN,LO)
              IF (GWLINK(LN)) THEN
C               Only solve the link if it has not previously been
C               solved and only compute hydropower/pumping on
C               unsolved links.
                IF(.NOT. L_SOLVED(LN))THEN
C                 Compute gw link flows based on current values of DSTO
                  IF(GWNODE(NOUT(LN))) AW(NOUT(LN))=DSTO(NOUT(LN))
                  AW(NOUT(LN))=MAX(0.0,AW(NOUT(LN)))
                  CALL GWLNKQ(LN,DSTO(NIN(LN)),AW(NOUT(LN)),DBQLN(LN))
                ENDIF
                DEQLN(LN) = DBQLN(LN)
                IF (DEQLN(LN) .GT. 0.) THEN
C                 Actual outflow.  Update outflow
                  TOTREL(NN) = TOTREL(NN)+DEQLN(LN)
                ELSE
C                 Actual inflow.  Update inflow (DEQLN here is <0)
                  INFLOW(NN) = INFLOW(NN)-DEQLN(LN)
                  InflowTS(NN) = InflowTS(NN)-DEQLN(LN)
                END IF
C               Compute hydropower and pumping
                IF (.NOT.L_SOLVED(LN).and. 
     1                ((PowerLink(LN) .and.HPCAP(LN).GT.0.)
     &                .or.(PumpLink(LN) .and.PConst(LN).GT.0.))) THEN
                  CALL HYDSIM(LN,DSTO(NIN(LN)),DSTO(NOUT(LN)),DBQLN(LN))
                END IF
C               Update storage at node
                DSTO(NN) = DSTO(NN) - DEQLN(LN)
                L_SOLVED(LN) = .TRUE. !Evgenii 100710 moved L_SOLVED(LN) = .TRUE. into if loop bc only gw links are solved here
			END IF
              
          ENDDO
555       continue
C         Update the available_water which may be used in next
C         time steps estimates of transfers on bi-directional
C         surface water links.
          AW(NN) = DSTO(NN)
C         Groundwater link flows at storage node computed.
C         Surface water node outflow to be computed next.
C
C         Determine surface water node releases:
C         Determine final storage volumes at sw node = DSTO
          IF(CAPN(NN).GT.0.0)THEN
            DS = DSTO(NN) - DTREL(NN)
            IF(DS .LT. 0.) THEN
                !If not enough for DTREL, release all storage
			  DTREL(NN) = DSTO(NN) 
                !DSTO(NN) = 0.
            ELSE IF(DS .GT. CAPN(NN)) THEN 
                !This releases any amount over node capacity
		  	  DTREL(NN) = DSTO(NN)-CAPN(NN) 
                !DSTO(NN) = CapN(NN)     after allocation update dsto()
             !Evgenii- code below can be used when reseviors should not be filled above their targets
		   !ELSE IF(DS .GT. DMD_TARG(NN)) THEN 
                !This releases any amount over node capacity
		  !	   DTREL(NN) = DSTO(NN)-DMD_TARG(NN) 
                !DSTO(NN) = CapN(NN)     after allocation update dsto()        		  
		  else
              !DSTO(NN) = DSTO(NN)-DTREL(NN) 
            END IF
          ELSE
C            A non-storage node
             DTREL(NN) = MAX(0.0, DSTO(NN))
          END IF
		releaseTS(nn)=dtrel(nn) !100608 Evgenii added subtimestep node release
      end subroutine





!*************************************************************************
      subroutine OutflowAllocation(NN,DTREL,DSTO,L_SOLVED)
! Allocates release to outgoing surface water links
! and computes link hydropower.
! Compute flows in links going from node. (based on DSTO)
! Seperated from FLWSIM.FOR
      USE VARS
	IMPLICIT NONE
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
!  CALL:
      INTEGER*2 FINTERP    !Function
      !SIMLNK(),HYDSIM()
!  INPUT
      INTEGER*2 NN
      REAL*4    DTREL(NODMAX),DSTO(NODMAX)
      LOGICAL*1 L_SOLVED(LNKMAX)
      !COMMON: DayPerTS, TOTOUT(NN), OUTLNK(NN,1)
      !        ALLOC_PTS(NN),NODE_ALLO(*,NN),NODE_CONSUMP(*,NN)
      !        GWLINK(LN),DMDLINK(LN),LINDIV(OUTLNK(NN,1),DEQLN(LN),DBQLN(LN)
      !        LINK_VTOL(1,LN),PowerLink(LN),HPCAP(LN),BQLN(LN),NOUT(LN)
      !        STEP_DEFICIT(NN)
!  OUTPUT
      !LOGICAL*1 L_SOLVED(LNKMAX)
!  Local
      INTEGER*2 i, LO, LN, OUT, ST, nNat,j !091113 added nNat for total natural links
      REAL*4    Rel, DCONSUMP,TMP_REL,AVAIL_CAP,ADJ,InStor,OutStor
      REAL*4    FLO_IN, NON_DMD, OrigRel,TMP_DEFICIT(nodmax),DDEM
	  Real::    TotDiv(lnkmax)
      LOGICAL*1 HAVDIV
!-------------------------------------------------------------------------
 		adj=0.0
	!Consumption by node itsself
         	if (sysstat(19)==2 .and. nn==2) then
			continue
		end if
		REL = DTREL(NN)
          DCONSUMP = 0.0
          nNat=0  !Evgenii 091113 added nNat for total natural links count (non-diversion)
	    !unit conversion: 10^6 m^3/time-step -> 10^6 m^3/day
		DDEM=0.0
	
		!convert from mil mil m3/time-step to mil m3/day
		TMP_REL = REL / DayPerTS
         	DO LO = 1,TOTOUT(NN)
               LN = OUTLNK(NN,LO)
		     !nNat for total natural links Evgenii, 091113 
			 IF (.NOT. GWLINK(LN) .and. .not.lindiv(ln))nNat=nNat+1  
		enddo
		!Evgenii added cons_node so it only goes into loop for consumption nodes 090826
		IF(ALLOC_PTS(NN).GT.1 .and. cons_node(nn))THEN 
             ST = FINTERP(3,NODE_ALLO(1,NN),NODE_CONSUMP(1,NN),
     1                    ALLOC_PTS(NN), TMP_REL, DCONSUMP)
            
		   IF(ST.EQ.SUCCES)DCONSUMP=MAX(0.0, DCONSUMP)
C            DCONSUMP is returned in user units.  Convert to internal units.
             DCONSUMP = DCONSUMP * DayPerTS
             
		   DCONSUMP=min(REL, DCONSUMP) !Evgenii 100211 added check to make sure enough for consumption
		   REL = REL - DCONSUMP
             REL = MAX(0.0, REL)
             !Accumulate total consumption at node
		   CONSUMPTION(NN) = CONSUMPTION(NN) + DCONSUMP 
          ENDIF

C     Accumulate total release variable.
          TOTREL(NN) = TOTREL(NN) + DTREL(NN) - DCONSUMP
          !update node storage
          IF (TOTOUT(NN) .LE. 0) THEN
            DSTO(NN) = DSTO(NN)-DTREL(NN)
            GO TO 300
          ELSE IF (TOTOUT(NN) .EQ. 1 ) THEN
C           If single diversion link, go to allocation function
            IF (LINDIV(OUTLNK(NN,1))) GO TO 220
            LN = OUTLNK(NN,1)
C           If groundwater link, skip allocation process
            IF (GWLINK(LN)) GO TO 300
            DBQLN(LN)=REL
            REL = 0.0
          END IF
C
C         Multiple links or single diversion link
C         Compute allocations to non gw link(s) using allocation fn.
C         Applies to multiple outgoing links and/or single diversion link.
220       HAVDIV = .FALSE.
          IF (OUTLNK(NN,1) .GT. 0) HAVDIV = LINDIV(OUTLNK(NN,1)) 
          IF (TOTOUT(NN).GT.1 .OR. HAVDIV) THEN !Evgenii - If 2 or more links or one and its a divesion go into loop
C           If any link allocation points, allocate flows
!		   Evgenii commented line out below and placed futher below, so if demand only links exist, they do not need
!            IF (ALLOC_PTS(NN).GT.1) THEN 
C              Any demand links coming out of this node must be
C              satisfied first
             DO LO = 1,TOTOUT(NN)
                LN = OUTLNK(NN,LO)
                IF (.NOT. GWLINK(LN)) THEN
                   DBQLN(LN) = 0.0                   
				 IF (DMDLINK(LN)) THEN
				   OUT = NOUT(LN)
C                    If out node has storage, adjust by deficit
		           !Evgenii changed STEP_DEFICIT below to TMP_DEFICIT 100127
				   !so that TMP_DEFICIT can be adjusted below w/o adjusting STEP_DEFICIT
				   !Evgenii 100702 put ADJ = STEP_DEFICIT(out) in if loop to allow for demand links not connected to demand nodes to work
				   if (DMDNODE(out) .and. STEP_DEFICIT(out)>0.0) then
				     ADJ = STEP_DEFICIT(out) !TMP_DEFICIT(OUT)
				   !Evgenii- If downstream node not a demand node, but this demand link is defined as a source link 
				   !(from source: line in iras.inp) for a demand node that is not directly downstream (only one link that is not directly upstream of demand node can be a source at this time)
				   else
				     DO i = 1,tnodes
					   do j=1,MXSUPLY
						 if(SUPL_NODE(j,i)==linkid(ln).and.
     &						 Source_Type(j,i)==2) then
	                      ADJ=STEP_DEFICIT(I)*SUPL_FRAC(j,i)
				         endif
	                   end do
				     end do
                     end if
   					 	 
				
				
C                    If passing flow requirement, also add the target
C                    IF (CAPN(OUT).LE.0.)ADJ=ADJ+DMD_TARG(OUT)
CMRT950102 The previous line is in error because it causes the link's out-node
C          target to be allocated to the link even if the it were to be met by
C          other sources... (An alternative approach would be to let STEP_DEFICIT
C          take on negative values to denote that we have already provided a 
C          surplus of water to this node.
                     AVAIL_CAP = MAX(0.0, CapL(LN)- BQLN(LN)) !CapL is the capacity of link per time-step and BQLN accumulates each sub-time step
                     ADJ = MIN( REL, ADJ, AVAIL_CAP)! , AVAIL_CAP 100615 evgenii took out available capacity limitatin from demand links
                     !Evgenii added if statement below in case link is demand AND diversion link, this way link Cap is adhered to
				   !if (LINDIV(LN))then 
	                !AVAIL_CAP = MAX(0.0, CapL(LN)- BQLN(LN)) !CapL is the capacity of link per time-step and BQLN accumulates each sub-time step
				    !ADJ = MIN(REL, AVAIL_CAP) 
				    !Add this to total diverted to link so that it is seen in the diversion link calculation below
				!	TotDiv(ln)=TotD iv(ln)+ADJ  
				   !end if
				   ADJ = MAX(0.0, ADJ)
                     DBQLN(LN) = ADJ
				   DDEM=DDEM+ADJ !Evgenii 100223 added DDEM to count total demand link allocation when considering diversions 
                     REL = REL - ADJ
	
                    !Evgenii 091024 put line below in so step deficit is reduced when it recieves inflow
				  !        this allows for multiples demand links going to the same demand node
				  !Evgenii 100707 put if then else to replace simple STEP_DEFICIT(out)=STEP_DEFICIT(out)-ADJ line so new source links 
				  !would pick up deficit reduction from allocation
				  if (DMDNODE(out)) then
					STEP_DEFICIT(out)=STEP_DEFICIT(out) -ADJ 
				  else 
				     DO i = 1,tnodes
					   do j=1,MXSUPLY
						 if(SUPL_NODE(j,i)==linkid(ln).and.
     &						 Source_Type(j,i)==2) then
							 STEP_DEFICIT(I)=STEP_DEFICIT(I)-ADJ
				         endif
	                   end do
				     end do
                     end if
				    	
				  !STEP_DEFICIT(out)=STEP_DEFICIT(out)							     	
				 END IF ! End Demand if 
                END IF ! End Not GW Link 
             END DO ! End do for all outlinks
            IF (ALLOC_PTS(NN).GT.1) THEN 
		!EVGENII MOVED ABOVE LINE FROM WHERE IT IS COMMENTED OUT ABOVE. THIS IS SO THAT IF ONLY DEMAND LINKS 
		!ARE COMING OUT OF THE NODE, THEY ARE CALCULATED WITHOUT THE NEED FOR DIVERSION FUNCTIONS
		
		!Now solve DIVERSION FUNCTIONS
C              Allocate balance of non-demand flow at node NN
C              Note that consumption is already taken out but
C              the interpolation process reaccounts for consumption
C              therefore add this node's consumption back in before
C              doing non-demand allocations.
               NON_DMD = REL + DCONSUMP +DDEM  !Evgneii 100223 add DDEM to include total demand link allocation when considering diversions 
               TMP_REL = NON_DMD / DayPerTS   
			 DO LO = 1,TOTOUT(NN)
                  LN = OUTLNK(NN,LO)
                  IF (.NOT. GWLINK(LN))THEN
				  ST = FINTERP(3,NODE_ALLO(1,NN),LINK_VTOL(1,LN)
     1                     ,ALLOC_PTS(NN), TMP_REL, FLO_IN)
                    FLO_IN = FLO_IN * DayPerTS
                    IF (FLO_IN.GT.REL) FLO_IN = REL
C                         If diversion, restrict flow to user input capacity.
				  IF (LINDIV(LN))THEN
                      AVAIL_CAP = MAX(0.0, CapL(LN)-BQLN(LN)) !-TotDiv(ln)Evgenii 100705 added -TotDiv(ln) too account for any allocations in demand allocation calculation
                      FLO_IN = MIN(FLO_IN,AVAIL_CAP)
                    ENDIF !End if LinDIV 
                    REL = REL - FLO_IN
C                         ^ release from node NN remaining after alloc. to LN
                    DBQLN(LN) = DBQLN(LN) + FLO_IN
				ENDIF 
!283				Continue
               ENDDO
            ENDIF !Ends if Allocation points > 0 Evgenii
C          All links going from node NN
           END IF !Ends If 2 or more links or one and its a divesion Evgenii
C
C          Check to see that mass balance is maintained, if appropriate.
C          Not all water allocated if REL > 0. Split it between non-diversion
C          links evenly (if any are available).  Compute hydropower if applicable.
         
	   !Evgenii 901114 changed original IRAS code for allocations to natural links. Originally IRAS allocated all remaing
	   !release to a single natural link, and didn't allocate any to the remaining natural links. Now IRAS distributes all 
	   !remaining flow evenly among the natural links
	     OrigRel=rel !901114 Evgenii added OrigRel, it is total release before natural link allocations
		 IF (TOTOUT(NN) .GT. 0) THEN
            DSTO(NN) = DSTO(NN) - DCONSUMP
            DO LO = 1,TOTOUT(NN)
               LN = OUTLNK(NN,LO)
               IF(.NOT. GWLINK(LN))THEN
                 IF(.NOT.LINDIV(LN).and.nNat>0)THEN !901114 Evgenii added .and.nNat >0
                   DBQLN(LN) = DBQLN(LN) + (origrel/nNat) !901114 Evgenii changed DBQLN(LN) + REL into DBQLN(LN) + (origrel/nNat)
                   REL=REL-(origrel/nNat) !901114 Evgenii changed  REL=0.0 into REL=REL-(origrel/nNat)
				!Excess going in link LN
                 ENDIF
C
C                Find end of link flow
                 CALL SIMLNK(LN,DBQLN(LN),DEQLN(LN))
C                Accumulate link flows for period
                 DSTO(NN) = DSTO(NN) - DBQLN(LN)
C
C                Compute hydropower
               
			  IF (.NOT.L_SOLVED(LN).and.
     1              ((PowerLink(LN) .and.HPCAP(LN).GT.0.)
     &              .or.(PumpLink(LN) .and.PConst(LN).GT.0.))) THEN			   
                   InStor = (DSTO(NN) + BSTO(NN) ) / 2.
                   OutStor = (DSTO(NOUT(LN))+BSTO(NOUT(LN)))/2.
                   CALL HYDSIM(LN,InStor,OutStor,DBQLN(LN))
                 END IF
               ENDIF
            ENDDO
C           If release still > 0 then no non-diversion links were
C           found to put it in, dump it from the system
            IF(REL.NE.0.0)THEN
               DSTO(NN) = DSTO(NN) - REL
               REL = 0.0
            ENDIF
           END IF
300        CONTINUE
      end subroutine


