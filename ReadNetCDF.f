C Subroutines to read & massage the data supplied by user
**************************************************
	Subroutine ReadNetCDF (id, nid, panel)

        Implicit none

        Include 'common1.h'
        Include 'common2.h'
        Include 'common3.h'
		Include 'netcdf.inc'
c	Include 'structure.h'

		character*10 idim,jdim,kdim
		Integer nid,id(nid),panel
		Integer i, retval, x, y, z
		Integer ncid, ndims
		Integer varid(4), dimid(4)
		Integer dataDims(NF_MAX_VAR_DIMS), dimLen(NF_MAX_VAR_DIMS)
		Real, dimension(512) :: dim1
		Real, dimension(512) :: dim2
		Real, dimension(512) :: dim3
c		Real, dimension(:), allocatable :: dim1
c		Real, dimension(:), allocatable :: dim2
c		Real, dimension(:), allocatable :: dim3

c		Real, dimension(512,512,128,128) :: data1
c		Real, dimension(512,512,128,128) :: data2
c		Real, dimension(512,512,128,128) :: data3
c		Real, dimension(512,512,128,128) :: data4

	    Real, dimension(:,:,:,:), allocatable :: data1
        Real, dimension(:,:,:,:), allocatable :: data2
	    Real, dimension(:,:,:,:), allocatable :: data3
	    Real, dimension(:,:,:,:), allocatable :: data4

C This subroutine reads in a 510 element header array that contains
C information regarding the data (array id) and stores the data
C into a 3 dimensional array fld_data(x, y, fld), where x is the
C horizontal grid, y is the vertical grid and fld is the field index.
C fld ranges from 1 to 4 where
C   1 = horizontal wind component
C   2 = vertical wind component
C   3 = 1st contour field
C   4 = 2nd contour field

C Initialize the header array
		DO i = 1, nid
		   id(i) = -999
		ENDDO

C Open the data file
	    retval = nf_open(data_file, NF_NOWRITE, ncid)
	    if (retval .ne. nf_noerr) call handle_err(retval)

C Get the shape of the data, and allocate the memory
		if (nf_inq_varid(ncid, 'x', dataDims(1)).eq.nf_noerr) then
			idim = 'x'
			SCAN_MODE = 'PPI'; id(16) = 20560; id(17) = 18720
		else if (nf_inq_varid(ncid, 'radius',
     X    dataDims(1)).eq.nf_noerr) then
			idim = 'radius'
                     IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
			SCAN_MODE = 'POLA'; id(16) = 20559; id(17) = 19521
			math_flag = 'Y'
                     ELSE
			SCAN_MODE = 'PPI'; id(16) = 20560; id(17) = 18720
                     ENDIF
		else
			call handle_err(-1)
		end if
C ASSIGN SCAN MODE
		call intswap(id(16),id(16))
		call intswap(id(17),id(17))
        scan_mode(1:1) = char(id(16))
	    scan_mode(2:2) = char(id(16)/256)
        scan_mode(3:3) = char(id(17))
	    scan_mode(4:4) = char(id(17)/256)
		if (nf_inq_varid(ncid, 'y', dataDims(2)).eq.nf_noerr) then
			jdim = 'y'
		else if (nf_inq_varid(ncid, 'theta',
     X    dataDims(2)).eq.nf_noerr) then
			jdim = 'theta'
		else
			call handle_err(-1)
		end if

		if (nf_inq_varid(ncid, 'altitude',
     X    dataDims(3)).eq.nf_noerr) then
			kdim = 'altitude'
		else if (nf_inq_varid(ncid, 'z',
     X    dataDims(3)).eq.nf_noerr) then
			kdim = 'z'
		else
			call handle_err(-1)
		end if

	    retval = nf_inq_vardimid(ncid, dataDims(1), dimid(1))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_vardimid(ncid, dataDims(2), dimid(2))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_vardimid(ncid, dataDims(3), dimid(3))
	    if (retval .ne. nf_noerr) call handle_err(retval)

	    retval = nf_inq_dimlen(ncid, dimid(1), dimLen(1))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_dimlen(ncid, dimid(2), dimLen(2))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_dimlen(ncid, dimid(3), dimLen(3))
	    if (retval .ne. nf_noerr) call handle_err(retval)
C Assume time dimension is one for now
		dimLen(4) = 1

c		allocate (dim1(dimLen(1)))
c		allocate (dim2(dimLen(2)))
c		allocate (dim3(dimLen(3)))
        allocate (data1(dimLen(1),dimLen(2),dimLen(3),dimLen(4)))
        allocate (data2(dimLen(1),dimLen(2),dimLen(3),dimLen(4)))
		allocate (data3(dimLen(1),dimLen(2),dimLen(3),dimLen(4)))
        allocate (data4(dimLen(1),dimLen(2),dimLen(3),dimLen(4)))

	    retval = nf_get_var_real(ncid, dataDims(1), dim1)
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_get_var_real(ncid, dataDims(2), dim2)
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_get_var_real(ncid, dataDims(3), dim3)
	    if (retval .ne. nf_noerr) call handle_err(retval)

C Get varids of the data variables, based on name.
	    retval = nf_inq_varid(ncid, str(1), varid(1))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_varid(ncid, str(2), varid(2))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_varid(ncid, str(3), varid(3))
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_inq_varid(ncid, str(4), varid(4))
	    if (retval .ne. nf_noerr) call handle_err(retval)

C Read the data.
	    retval = nf_get_var_real(ncid, varid(1), data1)
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_get_var_real(ncid, varid(2), data2)
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_get_var_real(ncid, varid(3), data3)
	    if (retval .ne. nf_noerr) call handle_err(retval)
	    retval = nf_get_var_real(ncid, varid(4), data4)
	    if (retval .ne. nf_noerr) call handle_err(retval)

		id(160) = dim1(1)*100.0
		id(161) = dim1(dimLen(1))*100.0
		id(162) = dimLen(1)
		id(163) = (dim1(2)-dim1(1))*1000.0
		IF (scan_mode.EQ.'POLA') THEN
			id(165) = dim2(1)*64.0
			id(166) = dim2(dimLen(2))*64.0
			id(167) = dimLen(2)
			id(168) = (dim2(2)-dim2(1))*64.0
        else
			id(165) = dim2(1)*100.0
			id(166) = dim2(dimLen(2))*100.0
			id(167) = dimLen(2)
			id(168) = (dim2(2)-dim2(1))*1000.0
	    endif
		id(170) = dim3(1)*100.0
		id(171) = dim3(dimLen(3))*1000.0
		id(172) = dimLen(3)
		id(173) = (dim3(2)-dim3(1))*1000.0

C Fill in the following elements of the CEDRIC header array..
C	id(16) = SCAN_MODE (char 1 & 2) ... see below
C	id(17) = SCAN_MODE (char 3 & 4) ... see below
C	id(33) = COORDINATE ORIGIN LATITUDE DEGREES
C	id(34) = COORDINATE ORIGIN LATITUDE MINUTES
C	id(35) = COORDINATE ORIGIN LATITUDE SECONDS * 100
C	id(36) = COORDINATE ORIGIN LONGITUDE DEGREES
C	id(37) = COORDINATE ORIGIN LONGITUDE MINUTES
C	id(38) = COORDINATE ORIGIN LONGITUDE SECONDS * 100
C	id(40) = DEGREES CLOCKWISE FROM NORTH TO X-AXIS * 64
C	id(41) = X COORDINATE OF HORIZONTAL AXIS ORIGIN * 100 [id(68)]
C	id(42) = Y COORDINATE OF HORIZONTAL AXIS ORIGIN * 100 [id(68)]
C	id(68) = GENERAL SCALING FACTOR (SF = 100)
C	id(69) = ANGLE SCALING FACTOR (CF = 64)
		id(68) = 100
		id(69) = 64
C	id(116) = YEAR BEGIN
C	id(117) = MONTH BEGIN
C	id(118) = DAY BEGIN
C	id(119) = HOUR BEGIN
C	id(120) = MINUTE BEGIN
C	id(121) = SECOND BEGIN
C	id(122) = YEAR END
C	id(123) = MONTH END
C	id(124) = DAY END
C	id(125) = HOUR END
C	id(126) = MINUTE END
C	id(127) = SECOND END
C	id(160) = MINIMUM X (KM) * 100 [id(68)]
C	id(161) = MAXIMUM X (KM) * 100 [id(68)]
C	id(162) = NUMBER OF X'S
C	id(163) = SPACING IN X (M)
C	id(165) = MINIMUM Y (KM) * 100 [id(68)]
C	id(166) = MAXIMUM Y (KM) * 100 [id(68)]
C	id(167) = NUMBER OF Y'S
C	id(168) = SPACING IN Y (M)
C	id(165) = MINIMUM Z (M)
C	id(166) = MAXIMUM Z (M)
C	id(167) = NUMBER OF Z'S
C	id(168) = SPACING IN Z (M)
C Note: only need id(16), id(17) & id(40) if plotting a
C flight track
C 	SCAN_MODE = 'PRI'; id(16) = 20562; id(17) = 18720
c 	SCAN_MODE = 'PPI'; id(16) = 20560; id(17) = 18720
C 	SCAN_MODE = 'COP'; id(16) = 17231; id(17) = 20512
C 	SCAN_MODE = 'FIX'; id(16) = 17993; id(17) = 22560
C 	SCAN_MODE = 'VAD'; id(16) = 22081; id(17) = 17440
C 	SCAN_MODE = 'CPL'; id(16) = 17232; id(17) = 19488
C 	SCAN_MODE = 'CPN'; id(16) = 17232; id(17) = 20000
C 	SCAN_MODE = 'CRT'; id(16) = 17234; id(17) = 21536
C 	SCAN_MODE = 'AIR'; id(16) = 16713; id(17) = 21024
C 	SCAN_MODE = 'POLA'; id(16) = 20559; id(17) = 19521

C Finally, fill in the data array fld_data(x,y,fld)
	    IF (fix_axis.EQ.'Z'.OR.fix_axis.EQ.'z') THEN
	       fix_beg=id(170)/float(1000) !convert to km
	       fix_inc=id(173)/float(1000) !convert to km
	    ELSEIF (fix_axis.EQ.'Y'.OR.fix_axis.EQ.'y') THEN
	 	   IF (scan_mode.EQ.'POLA') THEN
	               fix_beg=id(165)/float(id(69))  !angle scale factor
	            ELSE
	               fix_beg=id(165)/float(id(68))  !general scale factor
	 	   ENDIF
	       fix_inc=id(168)/float(1000) !convert to km
	    ELSEIF (fix_axis.EQ.'X'.OR.fix_axis.EQ.'x') THEN
	       fix_beg=id(160)/float(id(68)) !scale factor
	       fix_inc=id(163)/float(1000)   !convert to km
	    ENDIF

       IF (level_flag.EQ.'D'.OR.level_flag.EQ.'d') THEN
         level=nint(((plots(panel)-fix_beg)/fix_inc)+1)
       ENDIF
	   IF ( fix_axis.EQ.'Z' ) THEN
   		DO x = 1, dimLen(1)
   		   DO y = 1, dimLen(2)
			fld_data(x, y, 1) = data1(x,y,level,1)
			fld_data(x, y, 2) = data2(x,y,level,1)
			fld_data(x, y, 3) = data3(x,y,level,1)
			fld_data(x, y, 4) = data4(x,y,level,1)
   		   ENDDO
   		ENDDO
	   ELSEIF ( fix_axis.EQ.'Y' ) THEN
  		DO x = 1, dimLen(1)
  		   DO z = 1, dimLen(3)
			fld_data(x, z, 1) = data1(x,level,z,1)
			fld_data(x, z, 2) = data2(x,level,z,1)
			fld_data(x, z, 3) = data3(x,level,z,1)
			fld_data(x, z, 4) = data4(x,level,z,1)
  		   ENDDO
  		ENDDO
	   ELSEIF ( fix_axis.EQ.'X' ) THEN
c scalefld(num)=id(180+(5*(k-1)))
 		DO y = 1, dimLen(2)
 		   DO z = 1, dimLen(3)
			fld_data(y, z, 1) = data1(level,y,z,1)
			fld_data(y, z, 2) = data2(level,y,z,1)
			fld_data(y, z, 3) = data3(level,y,z,1)
			fld_data(y, z, 4) = data4(level,y,z,1)
 		   ENDDO
 		ENDDO
        ENDIF
	    retval = nf_close(ncid)
	    if (retval .ne. nf_noerr) call handle_err(retval)

	    Return
	    End

        Subroutine handle_err(errcode)
	    implicit none
	    include 'netcdf.inc'
	    integer errcode

	    print *, 'Error: ', nf_strerror(errcode)
	    stop 2
        end
