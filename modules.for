	!Copyright (c) 2009,2010 by University College London, Cornell University
	!Authors:
	!Evgenii Matrosov (evgenii.matrosov@ucl.ac.uk)

	Module vars
	!Declares global allocatable variables
	IMPLICIT NONE
	SAVE
	!iras.flw variables
	real, allocatable, dimension(:,:):: flowdata
	real, allocatable, dimension(:,:):: flwfactmax
	integer, allocatable, dimension(:,:,:):: YearFailEvent

	!iras demand timeseries variables
	character(50), allocatable, dimension(:)::DemName
	logical, allocatable, dimension(:):: DemTimeSeries
	integer, allocatable, dimension(:):: DemIDObs
	real, allocatable, dimension(:,:):: DemData


	!IRAS *.fac file variable
	real , allocatable, dimension(:,:,:)::flowfactor

	!Kang add 20100629
        INTEGER*4   cRead_Sim_Data,cSysEvap,cLinkRating,cLinkLoss
        INTEGER*4   cCrossSection,cLinkRouting,cTransfer,cGWData
        INTEGER*4   cNodeRating,cNodeEvap,cAllocation,cTarget
        INTEGER*4   cTargetSource,cPowerPump,cReleaseRule,cGrpBalance
	  INTEGER*4   cDemRed   !Added by Evgenii 100719
        REAL*4      tRead_Sim_Data
        CHARACTER*256 strTemp

        !Kang add for improving performance
        !Store the Min and Max ID of current policy for Node
        INTEGER*2  MinNodePolicyID, MaxNodePolicyID
        !Store the Min and Max ID of current policy for Link
        INTEGER*2  MinLinkPolicyID, MaxLinkPolicyID
        CHARACTER*256, POINTER::pFileData(:)
        !The total lines of .inp file and the maximum of lines allowed for speed
        !If the total lines are less than the maximum of lines allowed for speed
        !then all data of .inp file are read and stored into an array 
        !at the beginning. After that, the program read directly from the array.
        !Otherwise, the .inp file is opened at the beginning, however, the array 
        !for buffering all data of the .inp file is not used (nBufferedLines==0).
        !NOTE: when nBufferedLines==0 and before use File UNIT to read data, 
        !     "rewind" should be called to set the file position to the beginning of file 
        INTEGER*4 nBufferedLines,MaxLineForSpeed,Rating_BL
        INTEGER*4 Evaporation_BL,CrossSection_BL,Routing_BL
        INTEGER*4 Groundwater_BL,Allocation_BL,Target_BL,Source_BL
        INTEGER*4 Power_BL,Pump_BL,Rule_BL,Balance_BL,Performance_BL
	  INTEGER*4 Cost_BL
	  INTEGER*4 DemRed_BL	!Added by Evgenii 100719
										
        PARAMETER (MaxLineForSpeed = 65535)
        !The following structures are to store data of some parts accesssed frequently
        !in the inp file. If the performace need to improve further, others can be added 
        !structure defintion of target
        TYPE ELEMENT_TARGET
		  SEQUENCE 
            INTEGER*2   :: GroupID, Policy, CompType, NodeID
            REAL*4      :: Targ, T_CO, EnvFlow, RefilTrig, DemInc
        END TYPE ELEMENT_TARGET
        !structure defintion of evaporation
        !CompType=1 for node; CompType=2 for link
        !It's better that Node and Link use different key words
        !(e.g. "NodeEvaporation" and "LinkEvaporation" because the data is different).  
        TYPE ELEMENT_EVAPORATION
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, ID        !for both Node and Link
            REAL*4      :: Evaporation
            INTEGER*2   :: LossMethod                           !only for Link
        END TYPE ELEMENT_EVAPORATION
        !structure defintion of link routing
        TYPE ELEMENT_LROUTING
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, LinkID, iMethod
            REAL*4      :: L_NRTRES,L_a, L_b,L_c
        END TYPE ELEMENT_LROUTING
        !structure defintion of Allocation
        TYPE ELEMENT_ALLOCATION
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, NodeID
            REAL*4      :: NodeOutput
            INTEGER*2   :: LinkID
            REAL*4      :: LinkAllo
        END TYPE ELEMENT_ALLOCATION
        !structure defintion of Source
        TYPE ELEMENT_SOURCE
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, NodeID, Supl_Node
            REAL*4      :: Supl_Frac
        END TYPE ELEMENT_SOURCE
        !structure defintion of rating
        !CompType=1 for node; CompType=2 for link
        !It's better that Node and Link use different key words
        !(e.g. "NodeRating" and "LinkRating" because the data is different).  
        TYPE ELEMENT_RATING
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, ID        !for both Node and Link
            REAL*4      :: ElevOrWidth, AreaOrEvapor, VolOrFlow !for both Node and Link
            REAL*4      :: Seep, MaxQ, LakeQ                    !only for Node            
        END TYPE ELEMENT_RATING
        !structure defintion of CrossSection
        TYPE ELEMENT_CROSS
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, CompType, LinkID
            REAL*4      :: BaseWidth, ChannelDepth, LSlope, RSlope
            REAL*4      :: UpLSlope, UpRSlope
        END TYPE ELEMENT_CROSS
        !structure defintion of Performance
        TYPE ELEMENT_PERFORMANCE
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, NodeID
            REAL*4      :: thres_limit
        END TYPE ELEMENT_PERFORMANCE
        !structure defintion of Cost
        TYPE ELEMENT_COST
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, LinkID
            REAL*4      :: FlowCost, FlowEng,AnnCostInc
        END TYPE ELEMENT_COST
        !structure defintion of Power and Pump
        !Both Power and Pump have the same data structure
        TYPE ELEMENT_POWER_PUMP
            SEQUENCE 
		    INTEGER*2 :: GroupID, Policy, NodeID
            REAL*4    :: HPCAP,PLANT_FACTOR,Intake_elev,Turbine_elev
            REAL*4    :: Outlet_elev,ECONST,HpQMin            
        END TYPE ELEMENT_POWER_PUMP
        !structure defintion of Rule
        TYPE ELEMENT_RULE
            SEQUENCE 
		    INTEGER*2 :: GroupID, Policy, NodeID
            REAL*4    :: res_rule(8)
        END TYPE ELEMENT_RULE

	  TYPE ELEMENT_DEMRED	   !Added by Evgenii 100719
            SEQUENCE 
		  INTEGER*2   :: GroupID, Policy, NodeID
            INTEGER*2   :: DemSourceID
		  REAL*4      :: Dem_Thres_limit,DemRedAmt
        END TYPE ELEMENT_DEMRED

        LOGICAL::bUseBufferedBinaryData
        INTEGER*4::nTarget
        TYPE (ELEMENT_TARGET), POINTER::pTarget(:)
        INTEGER*4::nEvapor
        TYPE (ELEMENT_EVAPORATION), POINTER::pEvapor(:)
        INTEGER*4::nRouting
        TYPE (ELEMENT_LROUTING), POINTER::pRouting(:)
        INTEGER*4::nAllocation
        TYPE (ELEMENT_ALLOCATION), POINTER::pAllocation(:)
        INTEGER*4::nSource
        TYPE (ELEMENT_SOURCE), POINTER::pSource(:)
        INTEGER*4::nRating
        TYPE (ELEMENT_RATING), POINTER::pRating(:)
        INTEGER*4::nCross
        TYPE (ELEMENT_CROSS), POINTER::pCross(:)
        INTEGER*4::nPerformance
        TYPE (ELEMENT_PERFORMANCE), POINTER::pPerformance(:)
        INTEGER*4::nCost
        TYPE (ELEMENT_COST), POINTER::pCost(:)
        INTEGER*4::nPower
        TYPE (ELEMENT_POWER_PUMP), POINTER::pPower(:)
        INTEGER*4::nPump
        TYPE (ELEMENT_POWER_PUMP), POINTER::pPump(:)
        INTEGER*4::nRule
        TYPE (ELEMENT_RULE), POINTER::pRule(:)
	  INTEGER*4::nDemRed
	  TYPE (ELEMENT_DEMRED), POINTER::pDemRed(:)

        
        !Here define globally the value of different fields in the inp file
        !for convenience of pragramming and avoid some mistypes
        CHARACTER(len=7)::headTarget = 'Target:'
        CHARACTER(len=12)::headEvaporation = 'Evaporation:'
        CHARACTER(len=8)::headRouting = 'Routing:'
        CHARACTER(len=11)::headAllocation = 'Allocation:'
        CHARACTER(len=7)::headSource = 'Source:'
        CHARACTER(len=7)::headRating = 'Rating:'
        CHARACTER(len=13)::headCross = 'CrossSection:'
        CHARACTER(len=12)::headPerformance = 'Performance:'
        CHARACTER(len=5)::headCost = 'Cost:'
        CHARACTER(len=6)::headPower = 'Power:'
        CHARACTER(len=5)::headPump = 'Pump:'
        CHARACTER(len=5)::headRule = 'Rule:'
	  CHARACTER(len=7)::headDemRed = 'DemRed:'

        integer*2 iPolicyFile
        INTEGER*4 totalLinePolicyFile
        CHARACTER*256, POINTER::pPolicyData(:)

	!iras.inp variables
	integer*2 INPFileID
	CHARACTER*256, DIMENSION(:), ALLOCATABLE, TARGET::SysFileContent
	CHARACTER*256, DIMENSION(:), ALLOCATABLE, TARGET::PolicyFileContent	
	TYPE (ELEMENT_TARGET),DIMENSION(:), ALLOCATABLE, TARGET::TargetArray
	TYPE (ELEMENT_EVAPORATION),DIMENSION(:), ALLOCATABLE, TARGET::EvapArray
	TYPE (ELEMENT_LROUTING),DIMENSION(:), ALLOCATABLE, TARGET::RoutingArray
	TYPE (ELEMENT_ALLOCATION),DIMENSION(:), ALLOCATABLE, TARGET::AlloArray
	TYPE (ELEMENT_SOURCE),DIMENSION(:), ALLOCATABLE, TARGET::SourceArray
	TYPE (ELEMENT_RATING),DIMENSION(:), ALLOCATABLE, TARGET::RatingArray
	TYPE (ELEMENT_CROSS),DIMENSION(:), ALLOCATABLE, TARGET::CrossArray
	TYPE (ELEMENT_PERFORMANCE),DIMENSION(:), ALLOCATABLE, TARGET::PerfArray
	TYPE (ELEMENT_COST),DIMENSION(:), ALLOCATABLE, TARGET::CostArray
	TYPE (ELEMENT_POWER_PUMP),DIMENSION(:), ALLOCATABLE, TARGET::PowerArray
	TYPE (ELEMENT_POWER_PUMP),DIMENSION(:), ALLOCATABLE, TARGET::PumpArray	
	TYPE (ELEMENT_RULE),DIMENSION(:), ALLOCATABLE, TARGET::RuleArray	
	TYPE (ELEMENT_DEMRED),DIMENSION(:), ALLOCATABLE,TARGET::DemRedArray		!Evgenii 100719
 	
	END MODULE vars
	 
     	
