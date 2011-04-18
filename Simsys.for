!Copyright (c) 2009,2010 by University College London, Cornell University
!Authors:
!G Pegram, Daniel P. Loucks (dpl3@cornell.edu), Marshall Taylor, Evgenii Matrosov (evgenii.matrosov@ucl.ac.uk),
!Julien Harou (j.harou@ucl.ac.uk), Peter French, Huicheng Zhou
!This program is free software under the General Public Licence, GPL (>=v2)
!Read the 'GPL License.txt' file distributed with this source code for a full license statement.
!
!	 *************************************************************************************

!This file includes:
! SIMULATION
! SIMSYS
! GetMonthDay

      PROGRAM SIMULATION
! CALL:
!  * ReadSimDef()
!  * GetUserUnits()
!  * read_network_data()
!  * ReadGageYearUnits()
!  * SIMSYS()
      !Kang remove 20100629 because the following statement is not supported on linux
    !  USE DFLIB
	USE VARS
	IMPLICIT NONE
      
	INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
 
!  Local

	LOGICAL*1 success
	real*4 time_end, time_begin
	real*4 time_sim_beg,time_sim_end
	integer*2 sim,r
	
!------------------------------------------------------------------------     
      !Begin running time calculation, Evgenii
	CALL CPU_TIME (time_begin) 
	
	
	!System initialization
	call INITSYS(time_begin)
	r=0
	sysstat(run)=0 
	do sim=1,sysstat(nRuns) 
		 call cpu_time(time_sim_beg)
		 r=r+1
		 CALL  SIMSYS (success)
		 !Print performance output file
		 call PerformanceOutput()				 
		 CALL CPU_TIME ( time_sim_end )
		 write(*,*)'Run number ',r,' ',time_sim_end-
     &			time_sim_beg,'From start: ',time_sim_end-
     &			time_begin
	end do !End of flow factor runs loop	

		
	
	
	if (.not.success) then
		WRITE(*,*)''
		WRITE(*,*)'Some errors occured in simulation!'
	end if
	!Close INP and POL files
	CLOSE(UNIT=INPFileID);CLOSE(UNIT=iPolicyFile)
	call de_allocate_vars()
888	CALL CPU_TIME ( time_end )
	WRITE(*,*)'Time of operation was ',time_end - time_begin,'seconds' 
	 	 
      STOP

      END PROGRAM

!	 *************************************************************************************
C
      SUBROUTINE INITSYS(time_begin)
 	!This subroutine initializes the system. It is run once, even in the case of multiple
	!runs. It initializes system variables, reads the input files into memory and allocates
	!allocatable arrays.

	USE vars
      implicit none	
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
	!Local
	logical*1 success
	real*4 time_begin,time_end_readfile
	integer*2 i,ly
 !------------------------------------------------------------------------ 
	DefFile		   = 'iras.def'				    !System defintions, Evgenii
	FlowFileName   = 'iras.gag'					!Gauge file now just gauge conversion factor defintions, Evgenii
      PolicyFileName = 'iras.pol'					!Seasonal and annual parameter changes defined in policy file, Evgenii
      SysFileName    = 'iras.inp'					!Network definitions, Evgenii
	UnitsFileName  = 'iras.unt'					!Evgenii added units file
	flwdatafilename = 'iras.flw'				!Evgenii added flw file, these are time-series flows at gauge nodes that used to be in Gage file.

	!read file names and year policyGrp
      CALL ReadSimDef(success)			
      if (.not. success) then 
		write(*,*)'Error in reading iras.def file'
		stop
	end if
		
      


	!Get units conversion multipliers which convert user units to internal unit just after having been read	(iras.unt)
      call GetUserUnits(success)
	IF(.not.success)then
        WRITE(*,*)'Error in reading units file:', UnitsFileName 
      STOP
      END if
  

 !     read network data from text file: iras.inp (INP file is opened and closed once here, performance loss- Evgenii 101115)	
	CALL read_network_data (success) 
      if (.not.success) then
        WRITE(*,*)'Network data reading failed from file:',sysFilename
        STOP
      END if

	!Read INP file
	call readINPFile(success)
	if ( .not.success) then																			
			WRITE(*,*)'Error in reading INP file'
			STOP
      END if
     	
	!Read Pol file
	call readPolFile(success)
	if ( .not.success) then																			
			WRITE(*,*)'Error in reading POL file'
			STOP
      END if


	CALL allocate_vars (success) 
      if (.not.success) then
        WRITE(*,*)'Variable allocation failed'
        STOP
      END if

	!following needs network data
      
	!Evgenii commented out call to ReadGageYearUnits, no longer needed in IRAS 2010
	!Now readGageUnits called directly (instead of from ReadGageYearUnits)
	!call ReadGageYearUnits(success)
	
	!read IRAS.gag gauge conversion factors and determine which gauges have flowfactors if any (iras.gag)
	call readGageUnits(success)   
      if (.not.success) then
        WRITE(*,*)'Error in reading gage file:',FlowFilename
        STOP
      END if

	!Read the iras.flw file into memory
	call readFlowFile(success)
	if ( .not.success) then																			
			WRITE(*,*)'Error in reading flow file'
			STOP
      END if

	!Read demand file if there demand time-series
	if (tDemSeries>0) then
		call readDemFile(success)
		if ( .not.success) then																			
			WRITE(*,*)'Error in reading demand file'
			STOP
		END if
	end if
	!Make directory Out
	!Kang remove 20100629 because the following statement is not supported on linux
	!makeDR=MAKEDIRQQ('Out') 


	
	!Read flow factors for run from guage node specific factor files (gaugename.fac)
	call ReadFlwFactors(success)
	if ( .not.success) then																			
		WRITE(*,*)'Error in reading factor file'
		STOP
      END if

	
	!Kang add 20100629
	CALL CPU_TIME (time_end_readfile)
	WRITE(*,*)'Time of reading param files was ',
     &         time_end_readfile - time_begin,'seconds' 
	
      success = .false.
	sysstat(nleap)=0      
	!Leap years added 101801 by Evgenii
	!Initialize total leap years in simulation system variable
	!find which years are leap yeras
	ly=sysstat(yrstart)

	!Do for all years
	do i=1,(SYSSTAT(YREND)-SYSSTAT(YRSTART)+1)
		!Identify leap years
		LeapYear(i)=.false.
		IF(MOD(ly,100)/=0.AND.MOD(ly,4)==0)then  
     			LeapYear(i)=.true.
			sysstat(nleap)=sysstat(nleap)+1	
		else if	(MOD(ly,400)==0)then
			LeapYear(i)=.true.
			sysstat(nleap)=sysstat(nleap)+1	
		endif
		!advance to next year
		ly=ly+1
	enddo 
		!Set total days in timestep, Evgenii changed this to NPER, in previous IRAS hardwired to one 1
	DAYSPRPRD =  SYSSTAT(NPER) 
		
	!STEPPRPRD: number of sub-time steps in time-step
	!if it is less than nMinSteps, then set to nMinSteps
 	STEPPRPRD = max(SYSSTAT(NSUBTT),nMinSteps) !Evgenii replaced MAX(nMinSteps,10.0) 091001 by current code
		
	!Number of days per sub-time step
	DAYPERTS = DAYSPRPRD/STEPPRPRD	!Number of days per sub-time step
	
	!Find total number of days in simulation period (including leap years)
	SYSSTAT(NUMDAYS)=(SYSSTAT(YREND)-SYSSTAT(YRSTART)+1)*365
     &	+SYSSTAT(nLeap) !Total number of days in simulation period
	
	!read default evaporation rates for surface nodes from iras.inp (subroutine found in Read_sim_Data.for)
	call readSysEvaporation(success)
	
	return
	end
!	 *************************************************************************************
	SUBROUTINE SIMSYS (success)
! revised from old SIMSYS in order to read flow data from a text file
!       flow data file: IRAS.GAG
! It calls the following subroutines:
!  * ReadFlwFactors
!  * InitPolicy
!  * readSysEvaporation
!  * read_year_policies
!  * InitVariables
!  * Read_Simulation_Data
!  * InitLinkRouting
!  * GetMonthDay
!  * FLWSIM()
!  * DayOutputText()
!  *DayOutputNode
!  *DayOutputLink

 	USE vars
      implicit none
	
      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
C
C    Arguments:
!  Input:
      !COMMON: CHARACTER*80 PolicyFileName,FlowFileName, SysFileName
!  Output:
      LOGICAL*1     success
C    Local variables:
      !Kang add 20100629
	REAL:: time_begin, time_end, time_temp
	INTEGER*2 iDatafile, iFlowfile
	CHARACTER*256 aLine
	CHARACTER*20  aVar
      INTEGER*2 iOutFile,ioutNodes, ioutlinks,sim,i
      INTEGER*2 iYear, iDay,idayY,iMonth,iDayM,ileap    
	character(len=30) NodesFileName,LinksFileName
	character(len=10)xrun
C
!  CALL:
      LOGICAL CountFileLines   !Function !Kang modify for improving performance
      INTEGER CountColumns     !Function !Kang modify for improving performance
!------------------------------------------------------------------------ 

      iOutFile = 20
	ioutNodes = 22 !Evgenii added ioutNodes for nodes output 090721
	iOutLinks = 24 !Evgenii added iOutlinks for links output 090721

	!Debuging output file
	!open (UNIT=iOutFile, FILE='IRASTMP.OUT', STATUS='Replace')
      !WRITE(iOutFile,*)"IRAS OUTPUT"
	

	
	!Loop for total number of runs (flow factor runs), Evgenii 0911

		sysstat(run)=sysstat(run)+1
		SysStat(SimRec)=0
		xrun=' '
		write(xrun,*)sysstat(run)
		xrun=adjustl(xrun)
		
		!Add run number to output file name (gaugename.out)
		LinksFileName='links_run'//trim(xrun)//'.out'
		NodesFileName='nodes_run'//trim(xrun)//'.out'
		
		!Open links and nodes out file for new run, Evgenii 090721 
		OPEN(UNIT=iOutLinks,FILE=trim(LinksFileName),STATUS='replace') 
		OPEN(UNIT=iOutNodes,FILE=trim(NodesFileName),STATUS='replace') 
		!For date debugging
		!OPEN(UNIT=200,FILE=trim('debug.txt'),STATUS='replace')
		
	  !Kang add 20100629
	  !CALL CPU_TIME (time_begin)
	  	
		!Initialize policy variables (subroutine found in initsys.for)
		call InitPolicy
	
		!read the first year policies (subroutine found in initsys.for)
      	call read_year_policies(PolicyGrpID(1),success) 
      	if (.not. success)GOTO 9999
		!PolicygroupID was specified in iras.def, it describes the year each policy is applicable 
	 
		call InitVariables
	
		!read all simulation data for policyGrp(1) because of InitLinkRouting() (subroutine found in read_sim_data.for)
        !Kang modify 20100630
 		!call Read_Simulation_Data(SysFileName,PolicyGrpID(1),success) !read_sim_data.for
          call Read_Simulation_Data(PolicyGrpID(1),success) !read_sim_data.for
 		
 		!Kang add 20100629
 		!CALL CPU_TIME ( time_end )
 		!time_total = time_end - time_begin
	
		call InitLinkRouting  !related to initial volume & number of reservoirs for routing

!***		Loop over number of runs and simulation YEARS ...   
!------------Code below heavily changed by Evgenii for variable time step 090706----------------	
		
	!   Initialize total leap years passed to 0
		ileap=0
		iday=0
	!	Initialize day in year (between 1 and 365, 366 for leap)
		sysstat(time)=0
		sysstat(sstep)=0
	
		!Initialize simulation with first year
		iyear=SYSSTAT(YRSTART)  
		SYSSTAT(YEAR) = iYear
		
		!Total years simulated
		SYSSTAT(SIM_YEAR)=1 

	    sysstat(ntimestep)=0
		!Do from day 1 to total number of days in simulation, in increments of time period
		do iDay = 1,SYSSTAT(numdays),SYSSTAT(NPER) 
			sysstat(nday)=iday
		    sysstat(ntimestep)=sysstat(ntimestep)+1
			nTS=sysstat(ntimestep)
			if(sysstat(ntimestep)==(sysstat(nrec)+1))then
				WRITE(*,*)'Simulation Ended, no more flow record'
				goto 301
			end if
			!change year when timestep brings a new year
			!For normal years
			IF (.not.leapyear(SYSSTAT(SIM_YEAR)).and.  
     &             (SysStat(Time)+SYSSTAT(NPER))>365)then
     				 call annualPerformance()
				 iyear=iyear+1  
				 SYSSTAT(YEAR)=iYear
				 !update simulation year
				 SYSSTAT(SIM_YEAR) =SYSSTAT(YEAR)-SYSSTAT(YRSTART) + 1 
				 
				 call InitPolicy 
			     call read_year_policies(  !Read policies for new year
     &                       PolicyGrpID(SysStat(Sim_Year)),success)
				 
			!leap year
			else if (leapyear(SYSSTAT(SIM_YEAR)).and.  
     &             (SysStat(Time)+SYSSTAT(NPER))>366)then	
				 call annualPerformance()
				 iyear=iyear+1  
				 SYSSTAT(YEAR)=iYear 
				 !add to total years simulated
				 ileap=ileap+1
				 !update simulation year
				 SYSSTAT(SIM_YEAR) =SYSSTAT(YEAR)-SYSSTAT(YRSTART) + 1  
				 call InitPolicy 
			     call read_year_policies(  !Read policies for new year
     &                       PolicyGrpID(SysStat(Sim_Year)),success)
				 
			Endif
			!Set day in year (between 1 and 365)
			SysStat(Time) = iDay-(SYSSTAT(SIM_YEAR)-1)*365
			!Adjust for leap years simulated
			SysStat(Time)=SysStat(time)-ileap
			idayY=SysStat(Time)
			     

			!Get date			
			 !For leap years  
			if (leapyear(SYSSTAT(SIM_YEAR)))then
				call GetMonthDayleap(SysStat(Time), SysStat(Month), 
     &				SysStat(Day))  
			 !For normal years
			else
      			call GetMonthDay(SysStat(Time), SysStat(Month),
     &			  SysStat(Day))
			end if
			iMonth = SysStat(Month); iDayM = SysStat(Day)
			!for date debugging
			!WRITE(200,*)iYEAR,iMonth,iDayM,sysstat(time),iday,ileap	

C		  FLWSIM performs the fMonthlow simulation over the
C		  specified number of simulation time steps: one step = n days
		  !Simulation data are related to simulation day.
		  !So, data such as allocation, rating are read if needed in FLWSIM()
		  
		  !Advance by one time step
		  SysStat(SimRec)=SysStat(SimRec)+1

		  CALL FLWSIM(PolicyGrpID(SysStat(Sim_Year)),
     &	           iYear,idayY,success)         

			 IF(.not.success) GOTO 9999 
		     !Kang remove because I am afraid it maybe have side effect on performance
		     !WRITE(*,6010)iYEAR,iMonth,iDayM

!            **Write simulation results for a within-year period
!		   Evgenii- If statement below is only for IRAS diagnostics, not needed for  simulatins 
!		   if (SYSSTAT(SIM_YEAR)==1)    !only output results in tmp out file for first year 
!    &		Call DayOutputText(iOutFile,iYEAR,iMonth,iDayM, NodSEQ)
         
			!Evgenii - Write time-step results to Nodes outfile 090721
			call DayOutputNode(ioutNodes,iDay) 
			!Evgenii - Write time-step results to New Links outfile 090721
			call DayOutputLink(ioutLinks,iDay) 
200		 CONTINUE
C		  End of within-year loop
C
300		ENDDO

	!close (iOutFile); !close (ioutNodes); close (ioutLinks)			
301   success = .true.
9999  continue      
      return
C
6010  FORMAT('+','Year: ',I5,', Month: ',I2, ', Day: ',I2)

6011  FORMAT('+','Day: ',I5,',Year: ',I5)
999	WRITE(*,*)'Error in reading gage flow!'
      END
C
C


!************************************************************************
      subroutine GetMonthDay(nDays, iMonth, iDay)
!  INPUT
      Integer*2 nDays
!  OUTPUT
      INTEGER*2 iMonth, iDay
!------------------------------------------------------------------------
      iMonth = 1
      iDay = 1
     
!
	
	SELECT CASE (nDays)
          CASE (1:31)
            iMonth = 1; iDay = nDays
          CASE (32:31+28)
            iMonth = 2; iDay = nDays - 31
          CASE (60:59+31)
            iMonth = 3; iDay = nDays - 59
          CASE (91:90+30)
            iMonth = 4; iDay = nDays - 90
          CASE (121:120+31)
            iMonth = 5; iDay = nDays - 120
          CASE (152:151+30)
            iMonth = 6; iDay = nDays - 151
          CASE (182:181+31)
            iMonth = 7; iDay = nDays - 181
          CASE (213:212+31)
            iMonth = 8; iDay = nDays - 212
          CASE (244:243+30)
            iMonth = 9; iDay = nDays - 243
          CASE (274:273+31)
            iMonth = 10; iDay = nDays - 273
          CASE (305:304+30)
            iMonth = 11; iDay = nDays - 304
          CASE (335:365)
            iMonth = 12; iDay = nDays - 334
      END SELECT
      end subroutine
		 

!************************************************************************
      subroutine GetMonthDayLeap(nDays, iMonth, iDay)
!Evgenii modified GetMonthDay for leap years 101809
!For Leap Years
!  INPUT
      Integer*2 nDays
!  OUTPUT
      INTEGER*2 iMonth, iDay
!------------------------------------------------------------------------
      iMonth = 1
      iDay = 1
     
!
	
	SELECT CASE (nDays)
          CASE (1:31)
            iMonth = 1; iDay = nDays
          CASE (32:31+29)
            iMonth = 2; iDay = nDays - 31
          CASE (61:59+31)
            iMonth = 3; iDay = nDays - 60
          CASE (92:90+30)
            iMonth = 4; iDay = nDays - 91
          CASE (122:120+31)
            iMonth = 5; iDay = nDays - 121
          CASE (153:151+30)
            iMonth = 6; iDay = nDays - 152
          CASE (183:181+31)
            iMonth = 7; iDay = nDays - 182
          CASE (214:212+31)
            iMonth = 8; iDay = nDays - 213
          CASE (245:243+30)
            iMonth = 9; iDay = nDays - 244
          CASE (275:273+31)
            iMonth = 10; iDay = nDays - 274
          CASE (306:304+30)
            iMonth = 11; iDay = nDays - 304
          CASE (337:366)
            iMonth = 12; iDay = nDays - 335
      END SELECT
      end subroutine
		 
		 
