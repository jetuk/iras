!Copyright (c) 2008, 2009 by University College London, Cornell University
!Authors:
!Daniel P. Loucks (dpl3@cornell.edu), Marshall Taylor, Peter French, 
!Evgenii Matrosov evgenii.matrosov@ucl.ac.uk), Julien Harou (j.harou@ucl.ac.uk), HC Zhou
!This program is free software under the General Public Licence, GPL (>=v2)
!Read the 'GPL License.txt' file distributed with this source code for a full license statement.
C+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS
C
C      LINK.INC
C      (Prerequisites are FND_VARS.INC and SIMDIM.INC)
C
C+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS+IRIS
C
C
C     Link name
      CHARACTER*50  LNAME(LNKMAX)
      integer*2 LinkID(LNKMAX)
C     Inflow node and outflow node
      INTEGER*2   NIN(LNKMAX),  NOUT(LNKMAX)
      Integer linkroute(LNKMAX) !Evgenii 090720 Linkrout method (for link output)
C     Initial storage volume condition of reservoirs and links.
      REAL*4  STOILT(LNKMAX )
C     Link capacity
      REAL*4  CAPL(LNKMAX)
C     Link Length
      REAL*4  LNKLEN(LNKMAX)
C
      LOGICAL*1   LINDIV(LNKMAX)      !Is link a diversion?
      LOGICAL*1   GWLINK(LNKMAX)      !Is this a groundwater link?
      LOGICAL*1   DMDLINK(LNKMAX)     !Is this a demand link?
      Logical     iflinkloss(LNKMAX)  !Loss on link if LinkLossmethod=0? Evgenii 090721
C     Detention Volume of the Link Number for water quality calculations.
!      INTEGER*2  DTNVOL(LNKMAX) DtnVOl no longer used
C
C     Link threshold (lower and upper)
      REAL*4  L_THRESHOLD(2,LNKMAX)
C
C     Hydropower
      REAL*4  HPCAP(LNKMAX)          !Rated Power of the plant
      REAL*4  ENERGY(LNKMAX)         !Accumulated energy produced
      REAL*4  ECONST(LNKMAX)         !Conversion constant
      REAL*4  PLANT_FACTOR(LNKMAX)   !Plant factor (fraction of time running)
      REAL*4  HPQMIN(LNKMAX)         !Minimum power producing flow
      REAL*4  HPQMax(LNKMAX)         !Maximum power producing flow
      REAL*4  HEAD(LNKMAX)           !Rated head or constant head for
C                                       "run of river" plants
      REAL*4  Intake_ELEV(LNKMAX)    !intake elevation
      REAL*4  TURBINE_ELEV(LNKMAX)   !Turbine elevation
      REAL*4  Outlet_ELEV(LNKMAX)    !Outlet elevation
      REAL*4  PCONST(LNKMAX)         !Pump energy conversion constant
      LOGICAL*1 PowerLink(LNKMAX)
      LOGICAL*1 PumpLink(LNKMAX)

C
      COMMON /LINK_1/ NIN, NOUT, STOILT, CAPL, LINDIV,
     1                 L_THRESHOLD, GWLINK, DMDLINK, !DTNVOL, DtnVOl no longer used
     1                HPCAP, ENERGY,TURBINE_ELEV, PCONST,
     2                ECONST, PLANT_FACTOR, HPQMIN, HEAD, LNKLEN,
     &                Intake_elev, Outlet_Elev, HPQMax,
     &                PowerLink, PumpLink, iflinkloss,linkroute
      COMMON /LINK_2/ LNAME, LinkID
C
C
C
C    LINKSIM.INC
C     Prerequisite is SIMDIM.INC
C
C
       INTEGER*2
C               Number of allocation points for each node
     1           ALLOC_PTS(NODMAX),
C               Number of evaporation vs. volume points for each link
     1           LEVAP_PTS(NODMAX),
C               Number of area vs. volume points for each stor. node
     1           NARVO_PTS(NODMAX),
C               Number of group balancing functions
     1           BALAN_PTS(NODMAX),
C               Number of points for link's groundwater allocation table
     1           GWALO_PTS(LNKMax),
C               Number of lake flows
     1           LAKEQ_PTS(NODMAX),
C               Time of travel points for a link
     1           TTV_PTS(LNKMAX),
C               Average flow rate points for a link
     1           AVQ_PTS(LNKMAX),
C               Number of supply nodes associated with a demand site
     1           SUPLY_PTS(NODMAX),
C               Supply nodes associated with a demand site
     1           SUPL_NODE(MXSUPLY,NODMAX)
C
C    Time periods for each node/link at which certain XRD data last found
       INTEGER*2
     1           LAST_CI,
     1           LAST_CL,
     1           LAST_CN,
     1           LAST_EV(LNKMAX),
     1           LAST_FL(LNKMAX),
     1           LAST_GW(LNKMAX),
     1           LAST_VT(LNKMAX),
     1           LAST_AQ(NODMAX),
     1           LAST_BA(NODMAX),
     1           LAST_LQ(NODMAX),
     1           LAST_RV(NODMAX),
C	           Routing method
     1           L_method(LNKMAX)
C
       REAL*4
C               Number of routing reservoirs for each link this and last period
     1           L_NRTRES(LNKMAX),
     1           LAST_NRTRES(LNKMAX),
     1           TLossL(LNKMAX),
     1           LastWidth(LNKMAX),
     1           LastDepth(LNKMAX),
     1           LastVelocity(LNKMAX),
C               Link evaporation function, each link
     1           LINK_EVAP(IAGMAX,LNKMAX),
C               Link flow function, each link
     1           LINK_FLOW(IAGMAX,LNKMAX),
C               Link volume function, each link (for diversions)
     1           LINK_VTOL(IAGMAX,LNKMAX),
C               Link time of travel function, each link
     1           LINK_TTV(IAGMAX,LNKMAX),
!               Link width
     &          LinkWidth(IAGMAX,LNKMAX),
C               Link average flow function, each link
     1           LINK_AVQ(IAGMAX,LNKMAX),
C               Link groundwater allocation data
!     1           LINK_GWAL(IAGMAX,LNKMAX),
C               Routing CI's for all links at a time period
     1           L_a(LNKMAX),
C               Routing CL's for all links at a time period
     1           L_b(LNKMAX),
C               Routing CN's for all links at a time period
     1           L_c(LNKMAX),
C               Routing subvolumes for all links at a time period
     1           SUB_LVOL(MAX_NRTRES, LNKMAX),
C               Node evaporation (rate/area for all time steps)
!     1           NODE_EVAP(TMAX,NODMAX),
     1           NODE_EVAP(NODMAX),
C               Node allocation
     1           NODE_ALLO(IAGMAX,NODMAX),
C               Node consumption allocation
     1           NODE_CONSUMP(IAGMAX,NODMAX),
C               Node elevation
     1           NODE_ELEV(IAGMAX,NODMAX),
C               Node seepage
     1           NODE_SEEP(IAGMAX,NODMAX),
C               Node volume
     1           NODE_VOL(IAGMAX,NODMAX),
C               Node area
     1           NODE_AREA(IAGMAX,NODMAX),
C               Node lake flows
     1           NODE_LAKEQ(IAGMAX,NODMAX),
!               Node max res release (for balance function) !Evgenii added this 090804
     1           Node_MaxQ(IAGMAX,NODMAX),     
C               Node release rule volumes
     1           NODE_RRVOL(IAGMAX,NODMAX),
C               Fraction of supply met by associated supply node
     1           SUPL_FRAC(MXSUPLY,NODMAX),
C               Demand target required for a given period.
     1           DMD_TARG(NODMAX),
C               Demand target carryover (fraction carried over from prev. prd.)
     1           DMD_T_CO(NODMAX),
C               Deficit during current time step
     1           STEP_DEFICIT(NODMAX),
C               Last time step's remaining deficit carryover
     1		   LAST_STEP_DEFICIT(NODMAX)
!			  Demand target annual increase
      Real*4      DemInc(NODMAX) !added by Evgenii 100723
     
      !groundwater transfer for bi-directional
      !for policy data
      real*4    GWFromVol(IAGMAX*IAGMAX)
      real*4    GWToVol(IAGMAX*IAGMAX)
      real*4    GWFlowFromTo(IAGMAX*IAGMAX)
      !for Darcy Law
      integer*2 GWMethod(LNKMAX)
      real*4 GWK(LNKMAX),GWElev(LNKMAX),GWLength(LNKMAX),GWWidth(LNKMAX)

      !Loss computing
      integer*2 LossMethod(LNKMAX)
      real*4 LinkLoss(LNKMAX)
      real*4 BaseWidth(LNKMAX),ChannelDepth(LNKMAX)
      real*4 LSlope(LNKMAX), RSlope(LNKMAX)
      real*4 UpLSlope(LNKMAX), UpRSlope(LNKMAX)

C	
	!For Costing and Energy - Evgenii 100713

	real:: FlowCost(LNKMAX),FlowEng(LNKMAX),EngTot(LNKMAX),
     &	EngTotHydSim(LNKMAX),AnnualEng(lnkmax),AnnualCost(lnkmax),
     &	CostTot(LNKMAX),TotPower(LNKMAX),AnnCostInc(lnkmax),
     &    maxannualcost(lnkmax),GlobannCost(lnkmax),totflow(lnkmax)  
	COMMON /Cost/FlowCost,FlowEng,EngTot,EngTotHydSim,CostTot,
     1	TotPower,AnnualEng,AnnualCost,maxannualcost,GlobannCost,
     &	totflow
      COMMON /LINKSIM/
     1           ALLOC_PTS, LEVAP_PTS, NARVO_PTS,
     1           GWALO_PTS, SUPLY_PTS, LastVelocity, TLossL,
     1           BALAN_PTS, LAKEQ_PTS, TTV_PTS, AVQ_PTS,LinkWidth,
     1           SUPL_NODE, L_NRTRES, LAST_NRTRES,LastWidth,LastDepth,
     1           LINK_EVAP, LINK_FLOW, LINK_VTOL, LINK_TTV, LINK_AVQ,
     1           L_method,  L_a,  L_b,  L_C, SUB_LVOL,
     1           NODE_EVAP, NODE_ALLO, NODE_ELEV, NODE_SEEP,
     1           NODE_VOL,  NODE_AREA, NODE_RRVOL,Node_MaxQ,
     1           NODE_LAKEQ,SUPL_FRAC, DMD_TARG, DMD_T_CO,DemInc,
     1           STEP_DEFICIT, NODE_CONSUMP,LAST_STEP_DEFICIT
C
      COMMON /LNKSIM2/
     1           LAST_CI, LAST_CL, LAST_CN,
     1           LAST_EV, LAST_FL, LAST_GW, LAST_VT,
     1           LAST_AQ, LAST_BA, LAST_LQ, LAST_RV

      COMMON /GW_1/ GWFromVol, GWToVol, GWFlowFromTo
      COMMON /GW_2/ GWMethod,GWK,GWElev,GWLength,GWWidth
      COMMON /LOSS_1/ LossMethod,LinkLoss,baseWidth,channelDepth
      COMMON /LOSS_2/ LSlope, RSlope,UpLSlope,UpRSlope,AnnCostInc
C
