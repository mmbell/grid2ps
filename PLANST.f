      SUBROUTINE PLANST(IUNIT,NLEV,ILHD,NST,IOTYPE)
C
C     READS AND UNPACKS THE LEVEL HEADER
C
      PARAMETER (LBF=10)
      DIMENSION ILHD(LBF),ITEM(LBF)
      DATA MODE,NTYPE,IBIT,NBITS,NSKIP/1,2,0,16,0/
      ISIXT(NUM)=(NUM-1)/4 + 1.01
      LEN=ISIXT(LBF)
      IVAL = LBF
      CALL CREAD(IUNIT,ILHD,IVAL)
      CALL GBYTES(ILHD,ITEM,IBIT,NBITS,NSKIP,LBF)
      CALL ASDPMD(ITEM(1),ILHD(1),3)
      CALL ALTER(ITEM(4),ILHD(4),6)
      IF (NLEV.NE.ILHD(5)) GOTO 50
      RETURN
 40   CONTINUE
      CALL TAPMES(IUNIT,NST)
      RETURN
 50   CONTINUE
      NST=4
      GOTO 40
      END
