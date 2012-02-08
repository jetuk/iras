!Copyright (c) 2000 Cornell University
!Authors:
!Daniel P. Loucks (dpl3@cornell.edu), Marshall Taylor, Peter French
!This program is free software under the General Public Licence, GPL (>=v2)
!Read the 'GPL License.txt' file distributed with this source code for a full license statement.
!
!	 *************************************************************************************

C
      SUBROUTINE SELSEQ()
C
C      USE:      Used to define the sequence of nodes to be
C                simulated in IRIS
C
C      INPUT:    None
C
C      OUTPUT:     NNSEQ (from database read), NODES
C
C      NOTES:      Based on algorithm developed by J. Andreu
C
C      Modifications:
C      MRT  9201         Major mods to incorporate database manager.
C      MRT  921022       Updated for looping networks as per DPL's most
C                        recent revisions.
!	 PL   000518	   Last channge
!	 *************************************************************************************

C
!     Input or known global variables:
!       TNodes, Links
!       TotIn(), TotOut()
!       InLink(i,j), OutLnk(i,j)
!     Output global variable:
!       NodSeq()-Sequence of nodes to be simulated in IRAS

!	 *************************************************************************************
      implicit none

      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
C
C      Local Variables:
       INTEGER*4   ST, NN, LL, LN, SL(LNKMAX)
       INTEGER*4   FLAG, CHFLAG, LOOPND, SN(NODMAX)
       INTEGER*4   I, TYPE
       INTEGER*4   NSEQ(NODMAX)
C
!------------------------------------------------------------------------
C
      TYPE   = 0
      LOOPND = 0
C
C     Check system's status.
      IF (TNODES .LE. 0 ) RETURN
C
C
C     Initialize variables. No sequence defined yet.
  1   CONTINUE
      DO I = 1,TNODES
         NSEQ(I) = 0
         SN(I) = 0
      END DO
C
      DO LN = 1,LINKS
        SL(LN) = 0
      END DO
      NODES = 0
      CHFLAG = 0
C
C     Define sequence.
  10  CONTINUE
      IF (CHFLAG .EQ. 2) CHFLAG = 3   !  no change last two passes
      IF (CHFLAG .EQ. 0) CHFLAG = 2   !  no change last pass
      IF (CHFLAG .EQ. 1) CHFLAG = 0   !  at least one node added last pass
      DO 20 NN = 1,TNODES
C        Check if already in sequence.
         IF (SN(NN) .EQ. 1) GO TO 20
C        Check if initial node with no incoming links.
         IF(TOTIN(NN) .EQ. 0) THEN
            NODES = NODES + 1
            NSEQ(NODES) = NN   
            SN(NN) = 1
            CHFLAG = 1
C
C           Flag all outgoing links from this node
            DO LL = 1,TOTOUT(NN)
               LN = OUTLNK(NN,LL)
               SL(LN) = 1
            END DO
            GO TO 20
         END IF
C
C        Not a start node. Check if all incoming links flaged.
         FLAG = 0
         DO LL = 1,TOTIN(NN)
            LN = INLINK(NN,LL)
            IF(SL(LN) .NE. 1) FLAG = 1
         END DO
C
C        If flag is still 0 this means all incoming links come
C        from nodes already in the sequence.
C        If flag is 1 and CHFLAG is 3, indicates a end-of-loop node
C        Highlight this node and leave highlighted.
         IF(FLAG.EQ.0 .OR. (FLAG.EQ.1 .AND. CHFLAG.EQ.3)) THEN
            NODES = NODES + 1
            NSEQ(NODES) = NN
            SN(NN) = 1
            IF(CHFLAG .EQ. 3) LOOPND = LOOPND + 1
            CHFLAG = 1
C
C           Flag all outgoing links
            DO LL = 1,TOTOUT(NN)
               LN = OUTLNK(NN,LL)
               SL(LN) = 1
            END DO
            GO TO 20
         END IF
C
  20  CONTINUE
C
      IF (CHFLAG .EQ. 3 )GO TO 9999
      IF (NODES .LT. TNODES) GO TO 10
C

 9999 CONTINUE
!      ST = WR_FND(SYSGRP,NNSEQ,0,0,NSEQ,TNODES, 1, TYPE)
      do i = 1, TNodes
        NODSEQ(i) = Nseq(i)
      end do
	
	RETURN
      END

!	 *************************************************************************************
      SUBROUTINE PrintSeq()
C
C      USE:     Prints the nodes sequence to file seq.out
C               
C      INPUT:    None
C
C      OUTPUT:     none
C
C      NOTES:      Evgenii Matrosov 261011
C
!	 *************************************************************************************
      implicit none

      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
C
C      Local Variables:
       INTEGER*4   ioutSeq,i

!------------------------------------------------------------------------
      
      OPEN(UNIT=ioutSeq, FILE='seq.out',STATUS='replace') 
      do i=1,tnodes
        write(ioutSeq, fmt=10)NodeID(NodSeq(i)),nname(NodSeq(i))
      end do

      close (ioutSeq)      
10    FORMAT(I20,A50)    

      return  
      end
!	 *************************************************************************************      
      SUBROUTINE SetUserSeq()
C
C      USE:     Prints the nodes sequence to file seq.out
C               
C      INPUT:    None
C
C      OUTPUT:     none
C
C      NOTES:      Evgenii Matrosov 261011
C
!	 *************************************************************************************
      implicit none

      INCLUDE 'IRAS_SYS.INC'
      INCLUDE 'NODE.INC'
      INCLUDE 'LINK.INC'
C
C      Local Variables:
       INTEGER*4   userID(nodmax),ioutSeq,i,j,LineCounter
       logical*1 success,CountFileLines
!------------------------------------------------------------------------
      
      OPEN(UNIT=ioutSeq, FILE='iras.seq',STATUS='old') 
      IF(.NOT. CountFileLines(ioutSeq, LineCounter)) GOTO 999 
      do i=1,tnodes
        read(ioutSeq, *)userID(i)
      end do
      do i=1,tnodes
        do j=1,tnodes
            if(userID(i)==nodeid(j)) then           
                NODSEQ(i)=j
            end if
        end do
      end do
      success=.true.
999   close (ioutSeq)      
      if (.not.success) then
        write(*,*)'The number of nodes in the iras.seq file do not 
     &    match the number of nodes in the iras.inp file'
      end if

      return  
      end 