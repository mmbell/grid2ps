

   FFLAGS  = -g -I/usr/local/include
   CC = gcc-4.8
   FC = gfortran
   OBJS = CIN.o COUT.o Char2Int.o ReadInp.o ALTER.o ASDPMD.o \
   AssignVar.o BLHED.o CEDERX.o CEDREAD.o CEDUTIL.o CRTHIN.o \
   FETCHZ.o IMHSUM.o LOCFLDID.o PLACEPLANE.o PLANST.o ReadVar.o \
   SAVEARRY.o SHIRBL.o TAPMES.o UserSub.o TextRead.o WritePS.o \
   contour.o main.o CDATE.o CEDLOG.o gbytes.o sbytes.o i1mach.o \
   ReadNetCDF.o

   grid2ps: $(OBJS)
	gfortran-4.8 -v $(FFLAGS) $(OBJS) $(FMISC) -o $@ -L/usr/local/lib -lnetcdff 
 
