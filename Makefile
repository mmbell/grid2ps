

   FFLAGS  = -g -I/usr/local/include
   CC = gcc
   FC = gfortran-4.2
   OBJS = CIN.o COUT.o Char2Int.o ReadInp.o ALTER.o ASDPMD.o \
   AssignVar.o BLHED.o CEDERX.o CEDREAD.o CEDUTIL.o CRTHIN.o \
   FETCHZ.o IMHSUM.o LOCFLDID.o PLACEPLANE.o PLANST.o ReadVar.o \
   SAVEARRY.o SHIRBL.o TAPMES.o UserSub.o TextRead.o WritePS.o \
   contour.o main.o CDATE.o CEDLOG.o gbytes.o sbytes.o i1mach.o \
   ReadNetCDF.o

   grid2ps: $(OBJS)
	gfortran-4.2 -v $(FFLAGS) $(OBJS) $(FMISC) -o $@ -L/usr/local/lib -lnetcdff 
 
