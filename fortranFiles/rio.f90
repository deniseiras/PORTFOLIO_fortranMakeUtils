!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################


subroutine history_start(name_name)

  ! This routine initializes the model from the history file

  use grid_dims
  use var_tables
  use io_params
  use mem_grid
  use ref_sounding
  use moduleUsed

!srf- bug at history runtype with carma radiation
  USE mem_aerad, ONLY: nwave

  implicit none

  include "files.h"

  character (len=*), intent(IN) :: name_name

  integer :: ngrids1,ioutput1  &
       ,nnxp1(maxgrds),nnyp1(maxgrds),nnzp1(maxgrds),nzg1,nzs1,npatch1

  integer :: iyr,imn,idy,itm,ie,maxarr,ngr,nc
  character (len=f_name_length) :: hnameinh !,prefix
  character (len=2) :: cng
  integer, external :: cio_i,cio_f
  integer,save :: iunhd=11


  ! Open the input history header file and read some of the info.

  nc=len_trim(hfilin)
!  hnameinh=hfilin(1:nc-9)//'.vfm'
  hnameinh=hfilin(1:nc-9)//'.bin'

  call rams_f_open(iunhd,hfilin(1:nc),'FORMATTED','OLD','READ',0)

  ie=cio_i(iunhd,1,'ngrids',ngrids1,1)
  ngridsh=ngrids1
  ie=cio_i(iunhd,1,'nnxp',nnxp1,ngrids1)
  ie=cio_i(iunhd,1,'nnyp',nnyp1,ngrids1)
  ie=cio_i(iunhd,1,'nnzp',nnzp1,ngrids1)
  ie=cio_i(iunhd,1,'npatch',npatch1,1)
  ie=cio_i(iunhd,1,'nzg',nzg1,1)
  ie=cio_i(iunhd,1,'nzs',nzs1,1)
  ie=cio_i(iunhd,1,'ioutput',ioutput1,1)
  ie=cio_f(iunhd,1,'time',time,1)

  ! Get the 1-d reference state

  do ngr=1,ngridsh
     write(cng,1) ngr
1    format(i2.2)
     ie=cio_f(iunhd,1,'u01dn'//cng,u01dn(1,ngr),nnzp(ngr))
     ie=cio_f(iunhd,1,'v01dn'//cng,v01dn(1,ngr),nnzp(ngr))
     ie=cio_f(iunhd,1,'pi01dn'//cng,pi01dn(1,ngr),nnzp(ngr))
     ie=cio_f(iunhd,1,'th01dn'//cng,th01dn(1,ngr),nnzp(ngr))
     ie=cio_f(iunhd,1,'dn01dn'//cng,dn01dn(1,ngr),nnzp(ngr))
     ie=cio_f(iunhd,1,'rt01dn'//cng,rt01dn(1,ngr),nnzp(ngr))
  enddo

  ! Put these into regular arrays (for moving grids)
  ie=cio_i(iunhd,1,'ninest',ninest,ngrids1)
  ie=cio_i(iunhd,1,'njnest',njnest,ngrids1)

  ! Check time on file for time requested

  if(abs(time-timstr).gt..1*dtlong)then
     print*,' !!! History start error                     !!!'
     print*,' !!! Requested time does not match file time !!!'
     print*,' !!! Requested time, file time               !!!'
     print*,' !!! TIMSTR,time,dtlong=',timstr,time,dtlong
     print*,' !!! TIMSTR(m,h,d)=',time/60.,time/3600.,time/86400.
     stop 'HISTORY_START time error'
  endif

  ! Find maximum size of any array on history file. Allocate scratch array of
  ! this size.

  maxarr=0
  do ngr=1,ngridsh
     maxarr=max(maxarr,nnxp1(ngr)*nnyp1(ngr)*nnzp1(ngr)  &
          ,nnxp1(ngr)*nnyp1(ngr)*nzg1*npatch1 &
          ,nnxp1(ngr)*nnyp1(ngr)*nzs1*npatch1 &
	  ,nnxp1(ngr)*nnyp1(ngr)*nwave)
  enddo

  ! read stuff here

  call hist_read_start(maxarr,hnameinh,iunhd)

  !print*,'back from read'
  close(iunhd)

end subroutine history_start

!******************************************************************************

subroutine hist_read_start(maxarr, hnamein, iunhd)

  use an_header
  use grid_dims
  use var_tables
  use mem_grid
  use node_mod, only: &
       mzp, mxp, myp,  & ! INTENT(IN)
       izu, jzv,       & ! INTENT(IN)
       mynum,          & ! INTENT(IN)
       ibcon,          & ! INTENT(IN)
       nmachs,         &   ! INTENT(IN)
       mchnum, &
       master_num, &
       nodemxp, &
       nodemyp, &
       nodemzp, &
       nodei0, &
       nodej0
  use mem_aerad, only: &
       nwave
  use ParLib, only: &
       parf_bcast
  USE ReadBcst, ONLY: &
       Broadcast

  implicit none
  include 'interface.h'

  integer, parameter :: i64 = selected_int_kind(14) !Kind for 64-bits Integer Numbers
  integer, intent(IN) :: maxarr, iunhd
  character(len=f_name_length), intent(IN) :: hnamein

  integer :: ngr,nv,nvh,i
  integer(kind=i64) :: npts, nptsh
  character type*1,post*10,fmt*3
  real, allocatable :: scr(:)
  !
  TYPE scridim
      real, allocatable :: scr(:,:,:,:)
  END TYPE scridim

  integer :: inhunt=10

  type (head_table), allocatable,save :: hr_table(:)
  TYPE (scridim), DIMENSION(7) :: srcRead
  INTEGER :: ia,iz,ja,jz,npointer,idim_type
  INTEGER :: m1,m2,m3,ngridl,nvalues,nvtabl
  INTEGER(kind=i64)  :: nzl,nxl,nyl,n4,j,k
  CHARACTER(LEN=16) :: cstring

  allocate (scr(maxarr))
  allocate (srcRead(2)%scr(1,nnxp(1),nnyp(1),1))
  allocate (srcRead(3)%scr(nnzp(1),nnxp(1),nnyp(1),1))
  allocate (srcRead(4)%scr(nzg,nnxp(1),nnyp(1),npatch))
  allocate (srcRead(5)%scr(nzs,nnxp(1),nnyp(1),npatch))
  allocate (srcRead(6)%scr(1,nnxp(1),nnyp(1),npatch))
  allocate (srcRead(7)%scr(1,nnxp(1),nnyp(1),nwave))

!LFR Begin
  ia = nodei0(mynum,1)+1
  iz = nodei0(mynum,1)+nodemxp(mynum,1)
  ja = nodej0(mynum,1)+1
  jz = nodej0(mynum,1)+nodemyp(mynum,1)
  m1=nodemzp(mynum,1)
  m2=nodemxp(mynum,1)
  m3=nodemyp(mynum,1)
!LFR END

  !  Read variable header info

  rewind(iunhd)

  IF (mchnum==master_num) THEN
     read(iunhd,*) nvtabl
  END IF
  CALL Broadcast(nvtabl, master_num, "nvtabl")

  allocate (hr_table(nvtabl))
  do nv=1,nvtabl

      IF (mchnum==master_num) THEN
          read(iunhd,*)  hr_table(nv)%string   &
          ,hr_table(nv)%npointer  &
          ,hr_table(nv)%idim_type  &
          ,hr_table(nv)%ngrid  &
          ,hr_table(nv)%nvalues

           cstring=hr_table(nv)%string
           npointer= hr_table(nv)%npointer
           idim_type=hr_table(nv)%idim_type
           ngridl=hr_table(nv)%ngrid
           nvalues=hr_table(nv)%nvalues
      END IF

      CALL Broadcast(cstring, master_num, "cstring")
      CALL Broadcast(npointer, master_num, "npointer")
      CALL Broadcast(idim_type, master_num, "idim_type")
      CALL Broadcast(ngridl, master_num, "ngridl")
      CALL Broadcast(nvalues, master_num, "nvalues")

      IF (mchnum/=master_num) THEN
         hr_table(nv)%string=cstring
         hr_table(nv)%npointer=npointer
         hr_table(nv)%idim_type=idim_type
         hr_table(nv)%ngrid=ngridl
         hr_table(nv)%nvalues=nvalues
      END IF

  enddo

  ! Open history data file
  IF (mchnum==master_num) call rams_f_open(inhunt,hnamein(1:len_trim(hnamein)),'UNFORMATTED','OLD','READ',0)

  ! Loop through all variables
  do nvh=1,nvtabl
     ! Read a variable
     nptsh=hr_table(nvh)%nvalues

     IF (mchnum==master_num) read(inhunt) &
               srcRead(hr_table(nvh)%idim_type)%scr

     !  See if this variable is active in the current run
     ngr=hr_table(nvh)%ngrid
     if(ngr > nvgrids) cycle

     do nv = 1,num_var(ngr)
        npts=vtab_r(nv,ngr)%npts
        if(hr_table(nvh)%string == vtab_r(nv,ngr)%name) then
!           if(nptsh /= npts) then
!              print*,'Grid point number mismatch on history field:',  &
!                   vtab_r(nv,ngr)%name,npts,nptsh
!              stop 'History read number points error'
!           endif
IF (mchnum==master_num)  print 33,'History filling grid: ',ngr,nv,vtab_r(nv,ngr)%name,npts
33         format(a25,2i5,3x,a18,i10)


           SELECT CASE (hr_table(nvh)%idim_type)
           CASE (2)
               nzl = 1
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = 1
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)
               call mk_2_buff(srcRead(hr_table(nvh)%idim_type)%scr(1,:,:,1), &
                              vtab_r(nv,ngr)%var_p, &
                              nnxp(ngr), nnyp(ngr), &
                              m2, m3, ia, iz, ja, jz)
            CASE (3)
               nzl = nnzp(1)
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = 1
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)
               call mk_3_buff(srcRead(hr_table(nvh)%idim_type)%scr(:,:,:,1), &
                              vtab_r(nv,ngr)%var_p, &
                              nnzp(ngr),nnxp(ngr), nnyp(ngr), &
                              m1, m2, m3, ia, iz, ja, jz)
            CASE (4)
               nzl = nzg
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = npatch
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)

               call mk_4_buff(srcRead(hr_table(nvh)%idim_type)%scr, &
                              vtab_r(nv,ngr)%var_p, &
                              nzg,nnxp(ngr), nnyp(ngr),npatch, &
                              nzg, m2, m3,npatch, ia, iz, ja, jz)
            CASE (5)
               nzl = nzs
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = npatch
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)

               call mk_4_buff(srcRead(hr_table(nvh)%idim_type)%scr, &
                              vtab_r(nv,ngr)%var_p, &
                              nzs,nnxp(ngr), nnyp(ngr),npatch, &
                              nzs, m2, m3,npatch, ia, iz, ja, jz)
            CASE (6)
               nzl = 1
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = npatch
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)

               call mk_4_buff(srcRead(hr_table(nvh)%idim_type)%scr(1,:,:,:), &
                              vtab_r(nv,ngr)%var_p, &
                              1,nnxp(ngr), nnyp(ngr),npatch, &
                              1, m2, m3,npatch, ia, iz, ja, jz)
            CASE (7)
               nzl = 1
               nxl = nnxp(1)
               nyl = nnyp(1)
               n4 = nwave
               call parf_bcast(srcRead(hr_table(nvh)%idim_type)%scr, &
                           nzl,nxl,nyl,n4,master_num)


               call mk_4_buff(srcRead(hr_table(nvh)%idim_type)%scr(1,:,:,:), &
                              vtab_r(nv,ngr)%var_p, &
                              1,nnxp(ngr), nnyp(ngr),nwave, &
                              1, m2, m3,nwave, ia, iz, ja, jz)
            CASE DEFAULT
               PRINT *, 'Wrong idim_type: ',hr_table(nv)%idim_type,hr_table(nvh)%string
               STOP 'history_start'
            END SELECT

!            IF(trim(vtab_r(nv,ngr)%name)=='THETA') THEN
!               print *,'***',trim(vtab_r(nv,ngr)%name)
!               CALL printAux(vtab_r(nv,ngr)%var_p,m1,m2,m3)
!            END IF
           exit
        endif
     enddo

  enddo
  close(inhunt)
  IF (mchnum==master_num) PRINT *, 'history file read end'; call flush(6)
  deallocate(scr,hr_table)

end subroutine hist_read_start




SUBROUTINE printAux(field,m1,m2,m3)
  use node_mod, only: &
       mzp, mxp, myp,  & ! INTENT(IN)
       izu, jzv,       & ! INTENT(IN)
       mynum

   INTEGER, INTENT(IN) :: m1,m2,m3
   real, intent(in) :: field(m1,m2,m3)
   INTEGER :: i,j,k

   print *, 'Inside printaux',m1,m2,m3
   DO i=1,m2
      DO j=1,m3
         write (80+mynum,FMT='(2(I2.2,1X),33(F12.6,1X))') &
            i,j,(field(k,i,j),k=1,m1)
      END DO
   END DO

END SUBROUTINE printAux


subroutine rearrange_p(n2,n3,n4,n5,a,b)
  implicit none

  integer :: n2,n3,n4,n5
  real :: a(n4,n2,n3,n5),b(n2,n3,n4,n5)

  integer :: i,j,k,ip

  do ip = 1,n5
     do k = 1,n4
        do j = 1,n3
           do i = 1,n2
              b(i,j,k,ip) = a(k,i,j,ip)
           enddo
        enddo
     enddo
  enddo
  return
end subroutine rearrange_p

! --(DMK)----------------------------------------------------------------------


subroutine AnlwrtInitialize(varType,anamel,anamelh,timeold)

  use an_header,only: &
      nvbtab

  use var_tables, only: &
       num_var,         &
       vtab_r

  use mem_grid,   only: &
       ngrids,          &
       time,            &
       iyear1,          &
       imonth1,         &
       idate1,          &
       itime1

  use io_params,  only: &
       ioutput,         &
       avgtim,          &
       afilout,         &
       iclobber

  implicit none

  include "files.h"

  character(len=*), intent(in)  :: varType
  character(len=f_name_length), intent(out) :: anamel(ngrids)
  character(len=f_name_length), intent(out) :: anamelh
  real,             intent(out) :: timeold

  ! local variables

  integer          :: ngr
  integer          :: nv
  logical          :: exans
  character(len=1) :: vnam
  character(len=2) :: cgrid
  character(len=*), parameter :: h="**(AnlwrtInitialize)**"

  timeold=time
  if(varType == 'MEAN'.or.varType == 'BOTH') &
       time=min(time,time-avgtim/2.)

  ! Construct header file name

  if(varType == 'INST') vnam='A'
  if(varType == 'ANAL') vnam='A'
  if(varType == 'LITE') vnam='L'
  if(varType == 'MEAN') vnam='M'
  if(varType == 'BOTH') vnam='B'
  call makefnam(anamelh,afilout,time,iyear1,imonth1,idate1,  &
       itime1*100,vnam,'head','txt')

  ! Construct vfm file name for each grid

  do ngr=1, ngrids

     write(cgrid,'(a1,i1)') 'g',ngr

     call makefnam(anamel(ngr),afilout,time,iyear1,imonth1,idate1,  &
          itime1*100,vnam,cgrid,'vfm')

     inquire(file=anamel(ngr)(1:len_trim(anamel(ngr))),exist=exans)

     if(exans.and.iclobber.eq.0) then
        call fatal_error(h//" trying to open file name "// &
             trim(anamel(ngr))//" but it already exists. run is ended.")
     endif

  end do

  return
end subroutine AnlwrtInitialize



subroutine OpenAnlwrt(anamel)

  implicit none

  include "files.h"

  character(len=f_name_length), intent(in) :: anamel

  ! local variables

  integer :: lenl

  lenl = len_trim(anamel)
  call rams_c_open(anamel(1:lenl)//char(0),'w'//char(0))

  return
end subroutine OpenAnlwrt





subroutine OneFieldAnlwrt (ioaunt, vsize, v_pointer, varn, idim_type, &
     nnzp, nnxp, nnyp, nzs, nzg, npatch, ngr, npointer, nvtot, nvcnt, aw_table)

  use an_header, only: &
       head_table

  implicit none
  include "i8.h"
  integer,           intent(in)    :: ioaunt            ! i/o unit (unused variable; actually a C file)
  integer,           intent(in)    :: vsize             ! size of field to write
  real,              intent(in)    :: v_pointer(vsize)  ! field to write
  character(len=16), intent(in)    :: varn              ! field name
  integer,           intent(in)    :: idim_type         ! field dimensionality (coded)
  integer,           intent(in)    :: nnzp              ! field # verticals (used only if idim_type==3)
  integer,           intent(in)    :: nnxp              ! field # x points (used only if idim_type==3 or 4)
  integer,           intent(in)    :: nnyp              ! field # y points (used only if idim_type==3 or 4)
  integer,           intent(in)    :: nzs               ! field # surf points (used only if idim_type==4)
  integer,           intent(in)    :: nzg               ! field # ground points (used only if idim_type==4)
  integer,           intent(in)    :: npatch            ! field # patches (used only if idim_type==4)
  integer,           intent(in)    :: ngr               ! grid number
  integer(kind=i8),  intent(inout) :: npointer          ! next available file position
  integer,           intent(in)    :: nvtot             ! aw_table size
  integer,           intent(inout) :: nvcnt             ! last used entry on aw_table
  type(head_table),  intent(inout) :: aw_table(nvtot)   ! fields on file data structure

  ! local variables

  real, pointer :: scr(:)
  integer           :: ierr
  character(len=8)  :: c0
  character(len=8)  :: c1
  character(len=*), parameter :: h="**(OneFieldAnlwrt)**"

  logical, parameter :: dumpLocal=.false.
  integer :: tam
  character(len=128) :: line


  ! debug dumping

  if (dumpLocal) then
     write(c0,"(i8)") vsize
     line=h//" dumps field "//trim(varn)//"("//trim(adjustl(c0))
     write(c0,"(i8)") idim_type
     line=trim(line)//"); idim_type="//trim(adjustl(c0))
     write(c0,"(i8)") ngr
     line=trim(line)//" of grid "//trim(adjustl(c0))
     write(c0,"(i8)") npointer
     line=trim(line)//"; file pos "//trim(adjustl(c0))
     write(c0,"(i8)") nvcnt+1
     line=trim(line)//" at aw_table("//trim(adjustl(c0))//")"
     print *, trim(line); call flush(6)
  end if

  ! scratch area

  allocate(scr(vsize),stat=ierr)
  if (ierr /= 0) then
     write(c0,"(i8)") vsize
     write(c1,"(i8)") ierr
     call fatal_error(h//" allocate scr("//trim(adjustl(c0))//&
          ") failed with stat="//trim(adjustl(c1)))
  end if

  ! Rearrange field (if required) and dump into file

  if (idim_type == 3) then

     ! Rearrange 3-d variables to (i,j,k) and dump

     if (dumpLocal) then
        write(c0,"(i8)") nnxp
        line=h//" rearrange 3D field into ("//trim(adjustl(c0))
        write(c0,"(i8)") nnyp
        line=trim(line)//","//trim(adjustl(c0))
        write(c0,"(i8)") nnzp
        write(c1,"(i8)") nnzp*nnxp*nnyp
        line=trim(line)//","//trim(adjustl(c0))//"); rearranged size is "//&
             trim(adjustl(c1))
        print *, trim(line); call flush(6)
     end if

     call rearrange (nnzp, nnxp, nnyp, v_pointer, scr)
     call OneFieldWrite (ioaunt, vsize, scr, varn, idim_type, ngr, &
          npointer, nvtot, nvcnt, aw_table)

  elseif (idim_type == 4) then

     ! Rearrange 4-d leaf%soil variables to (i,j,k,ip) and dump

     if (dumpLocal) then
        write(c0,"(i8)") nnxp
        line=h//" rearrange 4D field into ("//trim(adjustl(c0))
        write(c0,"(i8)") nnyp
        line=trim(line)//","//trim(adjustl(c0))
        write(c0,"(i8)") nzg
        line=trim(line)//","//trim(adjustl(c0))
        write(c0,"(i8)") npatch
        write(c1,"(i8)") nnxp*nnyp*nzg*npatch
        line=trim(line)//","//trim(adjustl(c0))//"); rearranged size is "//&
             trim(adjustl(c1))
        print *, trim(line); call flush(6)
     end if

     call rearrange_p (nnxp, nnyp, nzg, npatch, v_pointer, scr)
     call OneFieldWrite (ioaunt, vsize, scr, varn, idim_type, ngr, &
          npointer, nvtot, nvcnt, aw_table)

  elseif (idim_type == 5) then

     ! Rearrange 4-d leaf%sfcwater variables to (i,j,k,ip) and dump

     if (dumpLocal) then
        write(c0,"(i8)") nnxp
        line=h//" rearrange 4D field into ("//trim(adjustl(c0))
        write(c0,"(i8)") nnyp
        line=trim(line)//","//trim(adjustl(c0))
        write(c0,"(i8)") nzs
        line=trim(line)//","//trim(adjustl(c0))
        write(c0,"(i8)") npatch
        write(c1,"(i8)") nnxp*nnyp*nzs*npatch
        line=trim(line)//","//trim(adjustl(c0))//"); rearranged size is "//&
             trim(adjustl(c1))
        print *, trim(line); call flush(6)
     end if

     call rearrange_p (nnxp, nnyp, nzs, npatch, v_pointer, scr)
     call OneFieldWrite (ioaunt, vsize, scr, varn, idim_type, ngr, &
          npointer, nvtot, nvcnt, aw_table)
  else

     ! dump input field

     call OneFieldWrite (ioaunt, vsize, v_pointer, varn, idim_type, ngr, &
          npointer, nvtot, nvcnt, aw_table)
  end if

  ! deallocate scratch area

  deallocate(scr, stat=ierr)
  if (ierr /= 0) then
     write(c1,"(i8)") ierr
     call fatal_error(h//" deallocate scr failed with stat="// &
          trim(adjustl(c1)))
  end if
end subroutine OneFieldAnlwrt





subroutine OneFieldWrite (ioaunt, vsize, v_pointer, varn, idim_type, ngr, &
     npointer, nvtot, nvcnt, aw_table)

  use an_header, only: &
       head_table

  implicit none
  include "i8.h"
  integer,           intent(in)    :: ioaunt            ! i/o unit (unused variable; actually a C file)
  integer,           intent(in)    :: vsize             ! size of field to write
  real,              intent(in)    :: v_pointer(vsize)  ! field to write
  character(len=16), intent(in)    :: varn              ! field name
  integer,           intent(in)    :: idim_type         ! field dimensionality (coded)
  integer,           intent(in)    :: ngr               ! grid number
  integer(kind=i8),  intent(inout) :: npointer          ! next available file position
  integer,           intent(in)    :: nvtot             ! aw_table size
  integer,           intent(inout) :: nvcnt             ! last used entry on aw_table
  type(head_table),  intent(inout) :: aw_table(nvtot)   ! fields on file data structure

  ! local variables

  integer          :: ierr
  integer(kind=i8) :: vsize_i8
  character(len=8) :: c0
  character(len=8) :: c1
  character(len=*), parameter :: h="**(OneFieldWrite)**"

  logical, parameter :: dumpLocal=.false.
  integer :: tam
  character(len=128) :: line

  real, allocatable :: scr(:)
  real, allocatable :: cscr(:)

  ! debug dumping

  if (dumpLocal) then
     line=h//" stores at aw_table("
     write(c0,"(i8)") nvcnt+1
     line=trim(line)//trim(adjustl(c0))//") fields %string="//trim(varn)
     write(c0,"(i8)") idim_type
     line=trim(line)//"; %idim_type="//trim(adjustl(c0))
     write(c0,"(i8)") ngr
     line=trim(line)//"; %ngrid="//trim(adjustl(c0))
     write(c0,"(i8)") npointer
     line=trim(line)//"; %npointer="//trim(adjustl(c0))
     write(c0,"(i8)") vsize
     line=trim(line)//"; %nvalues="//trim(adjustl(c0))
     print *, trim(line); call flush(6)
  end if

  ! scratch areas

  allocate(scr(vsize), stat=ierr)
  if (ierr /= 0) then
     write(c0,"(i8)") vsize
     write(c1,"(i8)") ierr
     call fatal_error(h//" allocate scr("// &
          trim(adjustl(c0))//") failed with stat="// &
          trim(adjustl(c1)))
  end if
  scr=0
  allocate(cscr(vsize), stat=ierr)
  if (ierr /= 0) then
     write(c0,"(i8)") vsize
     write(c1,"(i8)") ierr
     call fatal_error(h//" allocate cscr("// &
          trim(adjustl(c0))//") failed with stat="// &
          trim(adjustl(c1)))
  end if
  cscr=0.0

  ! store info on field to be written info at next available aw_table entry

  nvcnt=nvcnt+1
  aw_table(nvcnt)%string=varn
  aw_table(nvcnt)%idim_type=idim_type
  aw_table(nvcnt)%ngrid=ngr
  aw_table(nvcnt)%npointer=npointer
  aw_table(nvcnt)%nvalues=vsize

  ! debug dumping

  if (dumpLocal) then
     print *, h//" test scr"; call flush(6)
     scr(1)=SUM(scr(1:vsize))
     print *, h//" test cscr"; call flush(6)
     scr(1)=SUM(cscr(1:vsize))
     print *, h//" test v_pointer"; call flush(6)
     scr(1)=SUM(v_pointer(1:vsize))
     print *, h//" test OK; invokes vforecr"; call flush(6)
  end if

  ! code field and dump coded field on file

  vsize_i8 = int(vsize,i8)
  call vforecr(ioaunt, v_pointer, vsize_i8, 18,  scr, cscr, &
       'LIN', npointer)

  ! deallocate scratch area

  deallocate(scr, stat=ierr)
  if (ierr /= 0) then
     write(c1,"(i8)") ierr
     call fatal_error(h//" deallocate scr failed with stat="//trim(adjustl(c1)))
  end if

  deallocate(cscr, stat=ierr)
  if (ierr /= 0) then
     write(c1,"(i8)") ierr
     call fatal_error(h//" deallocate cscr failed with stat="//trim(adjustl(c1)))
  end if

  if (dumpLocal) then
     print *, h//" done"; call flush(6)
  end if
end subroutine OneFieldWrite



subroutine CloseAnlwrt()

  implicit none

  call rams_c_close()
end subroutine CloseAnlwrt


logical function frqWrt(iflag,varType,time,avgtim,timmax,frqanl,frqlite, &
     frqmean,frqboth,dtlongn)

  implicit none

  integer,       intent(in) :: iflag
  character*(*), intent(in) :: varType
  real,          intent(in) :: time
  real,          intent(in) :: avgtim
  real,          intent(in) :: timmax
  real,          intent(in) :: frqanl
  real,          intent(in) :: frqlite
  real,          intent(in) :: frqmean
  real,          intent(in) :: frqboth
  real,          intent(in) :: dtlongn

  ! local variables

  character(len=*), parameter :: h="**(frqWrt)**"

  frqWrt=.false.

  select case (varType)
  case ("ANAL")
     if(mod(time,frqanl) < dtlongn .or.  &
          time  >=  timmax - .01*dtlongn .or.  &
          iflag == 1)then
        frqWrt=.true.
     endif
  case ("LITE")
     if(frqlite > 0.) then
        if( mod(time,frqlite) < dtlongn .or.  &
             time  >=  timmax - .01*dtlongn ) then
           frqWrt=.true.
        endif
     endif
  case ("MEAN")
     if (frqmean>0.) then
        if(avgtim > 0.0.and.mod(time-avgtim/2.,frqmean) < dtlongn &
             .and.time >= avgtim) frqWrt=.true.
        if(avgtim < 0.0.and.mod(time,frqmean) < dtlongn)  &
             frqWrt=.true.
     endif
  case ("BOTH")
     if (frqboth>0.) then
        if(avgtim > 0.0.and.mod(time-avgtim/2.,frqboth) < dtlongn &
             .and.time >= avgtim) frqWrt=.true.
        if(avgtim < 0.0.and.mod(time,frqboth) < dtlongn)  &
             frqWrt=.true.
     endif
  case default
     call fatal_error(h//" unknown varType=**"//trim(varType)//"**")
  end select

end function frqWrt



! --(DMK)----------------------------------------------------------------------




subroutine CopyLocalChunk(field, LocalChunk, LocalSize)
  integer, intent(in ) :: LocalSize
  real,    intent(in ) :: field(LocalSize)
  real,    intent(out) :: LocalChunk(LocalSize)

  LocalChunk(:) = field(:)
end subroutine CopyLocalChunk

subroutine CopyLocalChunkReverse(field, LocalChunk, LocalSize)
  integer, intent(in ) :: LocalSize
  real,    intent(out ) :: field(LocalSize)
  real,    intent(in) :: LocalChunk(LocalSize)
	  field(:) = LocalChunk(:)
end subroutine CopyLocalChunkReverse


!----------------ALF--------------------------------------------------------


subroutine OutputFields(histFlag, instFlag, liteFlag, meanFlag)

  ! OutputFields: Define the fields to write and select the output
  !               method: parallel HDF5, VFM, parallel MPI-IO,
  !               binary per process

  use io_params, only: &
       ioutput

  use mem_grid, only: &
       ngrids

  use var_tables, only: &
       num_var, &
       vtab_r

  use node_mod, only: mchnum

  implicit none
  logical, intent(in) :: histFlag     ! true iff history output requested
  logical, intent(in) :: instFlag     ! true iff instant output requested
  logical, intent(in) :: liteFlag     ! true iff lite vars output requested
  logical, intent(in) :: meanFlag     ! true iff field average output requested

  integer :: maxNFields, nvMax, ierr

  logical, allocatable :: Willwrite(:,:)

  logical, parameter :: dumpLocal=.false.
  character(len=7)            :: cProc
  character(len=16)           :: varn
  character(len=8)            :: c0, c1, c2
  character(len=*), parameter :: h="**(OutputFields)**"

  logical, external :: checkTimeIO

  write(cProc,"(a5,i2)") " Proc",mchnum
  if (dumpLocal) then
     write(*, "(4(a,l1))") h//cProc//" begins with"//&
          " histFlag=",histFlag, &
          ", instFlag=",instFlag, &
          ", liteFlag=",liteFlag, &
          ", meanFlag=",meanFlag
  end if

  ! nothing to do if no output is selected
  if (.not. checkTimeIO(histFlag, instFlag, liteFlag, meanFlag)) then
     return
  end if

  nvMax = maxval(num_var(1:ngrids))
  if (dumpLocal) then
     write(c0, "(i8)") nvMax
     write(*, "(a)") h//cProc//" nvMax="//trim(adjustl(c0))
  end if

  allocate(Willwrite(nvMax,ngrids), stat=ierr)
  if (ierr /= 0) then
     write(c0,"(i8)") nvMax
     write(c1,"(i8)") ngrids
     write(c2,"(i8)") ierr
     call fatal_error(h//" allocate Willwrite("//trim(adjustl(c0))//&
          ","//trim(adjustl(c1))//") failed with stat="//trim(adjustl(c2)))
  elseif (dumpLocal) then
     write(*, "(a)") h//cProc//" allocate Willwrite "
  end if

  ! required fields for selected outputs:
  ! Willwrite stores if field is required or not
  call fieldWrite(nvMax, ngrids, Willwrite, maxNFields)

  if (ioutput/=0) then
     select case (ioutput)

        case (1)
           call saveHdf5Par(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
                ngrids, willwrite, maxNFields)

        case (2)
           call saveVFM(histFlag, instFlag, liteFlag, meanFlag, nvMax, ngrids, &
                willwrite, maxNFields)

        case (3)
           call saveBinMPIIO(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
                ngrids, willwrite, maxNFields)

        case (4)
           call saveNodeFields(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
                ngrids, willwrite, maxNFields)

     end select

  endif

  ! deallocate Willwrite
  deallocate(Willwrite, stat=ierr)
  if (ierr /= 0) then
     write(c2,"(i8)") ierr
     call fatal_error(h//" deallocate Willwrite failed with stat="//&
          trim(adjustl(c2)))
  end if

end subroutine OutputFields



logical function checkTimeIO(histFlag, instFlag, liteFlag, meanFlag)

  use node_mod, only: mchnum !INTENT(IN)

  implicit none

  logical, intent(in) :: histFlag       ! true iff history output requested
  logical, intent(in) :: instFlag       ! true iff instant output requested
  logical, intent(in) :: liteFlag       ! true iff lite vars output requested
  logical, intent(in) :: meanFlag       ! true iff field average output requested

  character(len=*), parameter :: h="**(checkTimeIO)**"
  character(len=7)            :: cProc
  logical, parameter          :: dumpLocal=.false.

  write(cProc,"(a5,i2)") " Proc",mchnum

  if (.not. (histFlag .or. instFlag .or. liteFlag .or. meanFlag)) then
     if (dumpLocal) write(*, "(a)") h//cProc//" nothing to do"
     checkTimeIO = .false.
  else
     checkTimeIO = .true.
  end if

end function checkTimeIO



subroutine fieldWrite(nvMax, ngrids, Willwrite, maxNFields)

  use node_mod, only: mchnum !INTENT(IN)

  use var_tables, only: &
       num_var, &
       vtab_r, &
       var_tables_r ! TYPE

  implicit none
  integer, intent(in)    :: nvMax
  integer, intent(in)    :: ngrids
  logical, intent(inout) :: Willwrite(nvMax,ngrids)
  integer, intent(out)   :: maxNFields

  integer                     :: ierr
  integer                     :: ng, nv

  character(len=*), parameter :: h="**(fieldWrite)**"
  character(len=8)            :: c0, c1, c2
  character(len=7)            :: cProc
  logical, parameter          :: dumpLocal=.false.

  write(cProc,"(a5,i2)") " Proc",mchnum

  if (dumpLocal) then
     write(*, "(a)") h//cProc//" initiates "
     write(c0, "(i8)") num_var(1)
     write(*, "(a)") h//cProc//" num_var(1)="//trim(adjustl(c0))
  end if

  do ng = 1, ngrids
     do nv = 1, num_var(ng)
        Willwrite(nv,ng) = &
             (vtab_r(nv,ng)%ihist==1) .or. &
             (vtab_r(nv,ng)%ianal==1) .or. &
             (vtab_r(nv,ng)%ilite==1) .or. &
             (vtab_r(nv,ng)%imean==1)
     end do
     do nv = num_var(ng)+1, nvMax
        Willwrite(nv,ng)=.false.
     end do
  end do

  ! how many fields to write for any output
  maxNFields = count(Willwrite)

  if (dumpLocal) then
     write(c0, "(i8)") maxNFields
     write(*, "(a)") h//cProc//" will write "//trim(adjustl(c0))//" fields"
  end if

end subroutine fieldWrite



subroutine saveVFM(histFlag, instFlag, liteFlag, meanFlag, nvMax, ngrids, &
     willwrite, maxNFields)

  ! saveVFM: Master process gathers fields that are domain decomposed
  !          over slaves and builds selected output files.
  !          Master only gathers fields that are selected for output,
  !          one at a time, to reduce master's memory to a minumum.
  !          Once a field is gathered, it is used on all selected
  !          output options.

  use an_header, only: &
       IOFileDS,       &
       CreateDisabledIOFileDS, &
       CreateEnabledIOFileDS, &
       OpenDataFile, &
       CloseDataFile, &
       DumpIOHeadTable, &
       DestroyIOFileDS

  use io_params, only: &
       iclobber, &
       hfilout,  &
       afilout,  &
       ioutput

  use mem_grid, only: &
       time, iyear1, imonth1, idate1, itime1, &
       GlobalSizes, &
       nnzp, nnxp, nnyp, nzg, nzs, npatch

  use mem_aerad, only: &
       nwave

  use ReadBcst, only: &
       LocalSizesAndDisp, &
       PreProcAndGather, &
       RearrangeAndDump

  use var_tables, only: &
       num_var, &
       vtab_r

  use node_mod, only: mchnum, nmachs, master_num, mynum  !INTENT(IN)

  implicit none

  logical, intent(in) :: histFlag
  logical, intent(in) :: instFlag
  logical, intent(in) :: liteFlag
  logical, intent(in) :: meanFlag
  integer, intent(in) :: nvMax
  integer, intent(in) :: ngrids
  logical, intent(in) :: Willwrite(nvMax, ngrids)
  integer, intent(in) :: maxNFields

  type(IOFileDS) :: histFileDS
  type(IOFileDS) :: instFileDS
  type(IOFileDS) :: liteFileDS
  type(IOFileDS) :: meanFileDS

  integer :: ng, nv

  integer :: ierr

  character(len=7)            :: cProc
  character(len=8)            :: c0, c1, c2
  character(len=*), parameter :: h="**(saveVFM)**"
  logical, parameter :: dumpLocal=.false.

  integer, parameter :: idim_type_min=2
  integer, parameter :: idim_type_max=7
  integer :: idim_type
  integer :: maxLocalSize
  integer :: maxSizeFullField
  integer :: maxSizeGathered
  integer :: globalSize(idim_type_min:idim_type_max)
  integer :: sizeFullField(idim_type_min:idim_type_max)
  integer :: sizeGathered(idim_type_min:idim_type_max)
  integer :: il1(nmachs)
  integer :: ir2(nmachs)
  integer :: jb1(nmachs)
  integer :: jt2(nmachs)
  integer :: localSize(nmachs,idim_type_min:idim_type_max)
  integer :: disp(nmachs,idim_type_min:idim_type_max)

  logical :: preproc
  logical :: rearran
  logical :: thisHistFlag   ! true iff history output requested and current field applies
  logical :: thisInstFlag   ! true iff instant output requested and current field applies
  logical :: thisLiteFlag   ! true iff lite vars output requested and current field applies
  logical :: thisMeanFlag   ! true iff field average output requested and current field applies
  character(len=16)           :: varn

  real, allocatable :: LocalChunk(:)
  real, allocatable :: Gathered(:)
  real, allocatable :: FullField(:)
  real, allocatable :: Rear(:)

  write(cProc,"(a5,i2)") " Proc",mchnum

  if (histFlag .and. mchnum == master_num) then
     call CreateEnabledIOFileDS ("HIST", hfilout, "H", iclobber, &
          time, iyear1, imonth1, idate1, itime1, &
          .true., .true., ngrids, maxNFields, histFileDS)
  else
     call CreateDisabledIOFileDS("HIST", histFileDS)
  end if
  if (instFlag .and. mchnum == master_num) then
     call CreateEnabledIOFileDS ("INST", afilout, "A", iclobber, &
          time, iyear1, imonth1, idate1, itime1, &
          .false., .false., ngrids, maxNFields, instFileDS)
  else
     call CreateDisabledIOFileDS("INST", instFileDS)
  end if
  if (liteFlag .and. mchnum == master_num) then
     call CreateEnabledIOFileDS ("LITE", afilout, "L", iclobber, &
          time, iyear1, imonth1, idate1, itime1, &
          .false., .false., ngrids, maxNFields, liteFileDS)
  else
     call CreateDisabledIOFileDS("LITE", liteFileDS)
  end if
  if (meanFlag .and. mchnum == master_num) then
     call CreateEnabledIOFileDS ("MEAN", afilout, "M", iclobber, &
          time, iyear1, imonth1, idate1, itime1, &
          .false., .false., ngrids, maxNFields, meanFileDS)
  else
     call CreateDisabledIOFileDS("MEAN", meanFileDS)
  end if
  if (dumpLocal) then
     write(*, "(a)") h//cProc//" created all IOFileDS"
     call flush(6)
  end if

  do ng = 1, ngrids

     if (dumpLocal) then
        write(c0,"(i8)") ng
        write(*,"(a)") h//cProc//" starts processing grid "//trim(adjustl(c0))
        call flush(6)
     end if

     ! grid dependent field sizes as a function of idim_type

     call GlobalSizes(ng, nmachs, nwave, globalSize)

     ! grid dependent, field independent constants for gather and unpacking
     ! as a function of idim_type

     call LocalSizesAndDisp(ng, il1, ir2, jb1, jt2, localSize, disp)

     ! full field size (no ghost zones) at this process

     if (mchnum == master_num) then
        sizeFullField(:) = globalSize(:)
     else
        sizeFullField(:) = 1
     end if

     ! full field size (with ghost zones) at any process

     sizeGathered(:) = disp(nmachs,:) + localSize(nmachs,:)

     ! maximum sizes over all fields

     maxLocalSize = maxval(LocalSize(mynum,:))

     maxSizeFullField = maxval(sizeFullField)
     maxSizeGathered = maxval(sizeGathered)

     if (dumpLocal) then
        write(c0,"(i8)") maxLocalSize
        write(c1,"(i8)") maxSizeFullField
        write(c2,"(i8)") maxSizeGathered
        write(*,"(a)") h//cProc//" maxSizeFullField="//trim(adjustl(c1))//&
             "; maxLocalSize="//trim(adjustl(c0))//&
             "; maxSizeGathered="//trim(adjustl(c2))
        call flush(6)
     end if

     ! maximum space for pre-processed local field

     allocate(LocalChunk(maxLocalSize), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxLocalSize
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate LocalSize("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxLocalSize
        write(*,"(a)") h//cProc//" allocated LocalSize("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     allocate(Gathered(maxSizeGathered), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxSizeGathered
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate Gathered("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxSizeGathered
        write(*,"(a)") h//cProc//" allocated Gathered("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     ! maximum space for unpacked field

     allocate(FullField(maxSizeFullField), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxSizeFullField
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate FullField("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxSizeFullField
        write(*,"(a)") h//cProc//" allocated FullField("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     ! maximum space for rearranged area

     allocate(Rear(maxSizeFullField), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxSizeFullField
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate Rear("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxSizeFullField
        write(*,"(a)") h//cProc//" allocated Rear("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     ! master opens requested output data files for this grid

     if (mchnum == master_num) then
        if (histFlag) then
           call OpenDataFile(histFileDS, ng)
        end if
        if (instFlag) then
           call OpenDataFile(instFileDS, ng)
        end if
        if (liteFlag) then
           call OpenDataFile(liteFileDS, ng)
        end if
        if (meanFlag) then
           call OpenDataFile(meanFileDS, ng)
        end if
     end if

     do nv = 1, num_var(ng)

        ! field to gather

        if (Willwrite(nv,ng)) then

           ! specific flags for this field

           thisHistFlag = vtab_r(nv,ng)%ihist==1 .and. histFlag
           thisInstFlag = vtab_r(nv,ng)%ianal==1 .and. instFlag
           thisLiteFlag = vtab_r(nv,ng)%ilite==1 .and. liteFlag
           thisMeanFlag = vtab_r(nv,ng)%imean==1 .and. meanFlag

           ! field name and dimensionality

           varn = vtab_r(nv,ng)%name
           idim_type = vtab_r(nv,ng)%idim_type
           if (idim_type < idim_type_min .or. idim_type > idim_type_max) then
              write(c0,"(i8)") idim_type
              call fatal_error(h//" unknown idim_type="//trim(adjustl(c0)))
           end if

           ! if field requires pre-processing

           preProc = (thisInstFlag .or. thisLiteFlag .or. thisMeanFlag) .and. &
                (varn == "PP" .or. varn == "HKM" .or. varn == "VKH")

           ! if field requires rearranging

           rearran = (thisInstFlag .or. thisLiteFlag .or. thisMeanFlag) .and. &
                (idim_type == 3 .or. idim_type == 4 .or. idim_type == 5)

           if (dumpLocal) then
              write(*,"(2(a,l1))") h//cProc//" on var_p field "//varn//&
                   "; preproc=",&
                   preProc, ", rearran=", rearran
              call flush(6)
           end if

           ! case 1: output current field values (for hist, inst and lite output files)

           call CopyLocalChunk(vtab_r(nv,ng)%var_p, LocalChunk, &
                LocalSize(mynum,idim_type))

           ! case 1-A: field with current values does not require pre-processing

           if (  thisHistFlag .or. &
                (thisInstFlag .and. .not. preProc) .or. &
                (thisLiteFlag .and. .not. preProc)        ) then

              ! gather untouched field

              call PreProcAndGather(.false., ng, idim_type, varn, &
                   il1, ir2, jb1, jt2, localSize, disp, &
                   LocalSize(mynum,idim_type), LocalChunk, &
                   sizeGathered(idim_type), Gathered, &
                   sizeFullField(idim_type), FullField)

              ! output untouched field and, if appropriate,
              ! rearrange and output rearranged field

              if (mchnum == master_num) then

                 call RearrangeAndDump (ng, .false., &
                      thisHistFlag, &
                      thisInstFlag .and. (.not. preProc) .and. (.not. rearran),&
                      thisLiteFlag .and. (.not. preProc) .and. (.not. rearran),&
                      .false., &           ! mean takes var_m
                      histFileDS, instFileDS, liteFileDS, meanFileDS, &
                      varn, idim_type, sizeFullField(idim_type), &
                      FullField, Rear)
                 if ( (thisInstFlag .and. (.not. preProc) .and. rearran) .or. &
                      (thisLiteFlag .and. (.not. preProc) .and. rearran) ) then
                    call RearrangeAndDump (ng, rearran,  &
                         .false., &        ! hist is not rearranged
                         thisInstFlag .and. (.not. preProc), &
                         thisLiteFlag .and. (.not. preProc), &
                         .false., &        ! mean takes var_m
                         histFileDS, instFileDS, liteFileDS, meanFileDS, &
                         varn, idim_type, sizeFullField(idim_type), &
                         FullField, Rear)
                 end if
              end if
           end if

           ! case 1-B: field with current values has to be pre-processed

           if ( (thisInstFlag .and. preProc) .or. &
                (thisLiteFlag .and. preProc)        ) then

              ! gather preprocessed field

              call PreProcAndGather(preProc, ng, idim_type, varn, &
                   il1, ir2, jb1, jt2, localSize, disp, &
                   LocalSize(mynum,idim_type), LocalChunk, &
                   sizeGathered(idim_type), Gathered, &
                   sizeFullField(idim_type), FullField)

              ! output rearranged field

              if (mchnum == master_num) then
                 call RearrangeAndDump (ng, rearran, &
                      .false., &           ! hist is not rearranged
                      thisInstFlag, &
                      thisLiteFlag, &
                      .false., &           ! mean takes var_m
                      histFileDS, instFileDS, liteFileDS, meanFileDS, &
                      varn, idim_type, sizeFullField(idim_type), &
                      FullField, Rear)
              end if
           end if

           if (thisMeanFlag) then

              call CopyLocalChunk(vtab_r(nv,ng)%var_m, LocalChunk, &
                   LocalSize(mynum,idim_type))
              varn= vtab_r(nv,ng)%name   ! could be changed on prior calls to PreProcAndGather

              if (dumpLocal) then
                 write(*,"(2(a,l1))") h//cProc//" on var_m field "//varn//&
                      "; preproc=",preProc, ", rearran=", rearran
              end if

              ! gather field, preprocessed or untouched

              call PreProcAndGather(preProc, ng, idim_type, varn, &
                   il1, ir2, jb1, jt2, localSize, disp, &
                   LocalSize(mynum,idim_type), LocalChunk, &
                   sizeGathered(idim_type), Gathered, &
                   sizeFullField(idim_type), FullField)

              ! output field, rearranged or untouched

              if (mchnum == master_num) then
                 call RearrangeAndDump (ng, rearran, &
                      .false., &    ! hist takes var_p
                      .false., &    ! inst takes var_p
                      .false., &    ! lite takes var_p
                      thisMeanFlag, &
                      histFileDS, instFileDS, liteFileDS, meanFileDS, &
                      varn, idim_type, sizeFullField(idim_type), &
                      FullField, Rear)
              end if

           endif

        endif

     enddo

     ! deallocated space

     deallocate(LocalChunk, stat=ierr)
     if (ierr /= 0) then
        write(c1,"(i8)") ierr
        call fatal_error(h//" deallocate LocalSize failed with stat="// &
             trim(adjustl(c1)))
     end if

     deallocate(Gathered, stat=ierr)
     if (ierr /= 0) then
        write(c1,"(i8)") ierr
        call fatal_error(h//" deallocate Gathered failed with stat="// &
             trim(adjustl(c1)))
     end if

     ! maximum space for unpacked field

     deallocate(FullField, stat=ierr)
     if (ierr /= 0) then
        write(c1,"(i8)") ierr
        call fatal_error(h//" deallocate FullField failed with stat="// &
             trim(adjustl(c1)))
     end if

     ! maximum space for rearranged area

     deallocate(Rear, stat=ierr)
     if (ierr /= 0) then
        write(c1,"(i8)") ierr
        call fatal_error(h//" deallocate Rear failed with stat="// &
             trim(adjustl(c1)))
     end if

     ! master closes output file for this grid

     if (mchnum == master_num) then
        if (histFlag) then
           call CloseDataFile(histFileDS)
        end if
        if (instFlag) then
           call CloseDataFile(instFileDS)
        end if
        if (liteFlag) then
           call CloseDataFile(liteFileDS)
        end if
        if (meanFlag) then
           call CloseDataFile(meanFileDS)
        end if
     end if

     ! master dumps one header file for all grids

     if (mchnum == master_num) then
        if (histFlag) then
           call DumpIOHeadTable(histFileDS)
        end if
        if (instFlag) then
           call DumpIOHeadTable(instFileDS)
        end if
        if (liteFlag) then
           call DumpIOHeadTable(liteFileDS)
        end if
        if (meanFlag) then
           call DumpIOHeadTable(meanFileDS)
        end if
     end if

     ! destroy all IOFileDS

     call DestroyIOFileDS(histFileDS)
     call DestroyIOFileDS(instFileDS)
     call DestroyIOFileDS(liteFileDS)
     call DestroyIOFileDS(meanFileDS)

  enddo

end subroutine saveVFM


!====
subroutine saveNodeFields(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
     ngrids, willwrite, maxNFields)

  use node_mod, only: mchnum, nmachs, mynum

  use mem_grid, only: &
       time, iyear1, imonth1, idate1, itime1

  use grid_dims, only: &
       maxgrds

  use var_tables, only: &
       num_var, &
       vtab_r

  use ReadBcst, only: &
       LocalSizesAndDisp

  implicit none
  logical, intent(in) :: histFlag       ! true iff history output requested
  logical, intent(in) :: instFlag       ! true iff instant output requested
  logical, intent(in) :: liteFlag       ! true iff lite vars output requested
  logical, intent(in) :: meanFlag       ! true iff field average output requested
  integer, intent(in) :: nvMax
  integer, intent(in) :: ngrids
  logical, intent(in) :: Willwrite(nvMax, ngrids)
  integer, intent(in) :: maxNFields

  logical :: thisHistFlag   ! true iff history output requested and current field applies
  logical :: thisInstFlag   ! true iff instant output requested and current field applies
  logical :: thisLiteFlag   ! true iff lite vars output requested and current field applies
  logical :: thisMeanFlag   ! true iff field average output requested and current field applies
  integer :: idim_type

  integer, parameter :: idim_type_min=2
  integer, parameter :: idim_type_max=7
  integer :: il1(nmachs)
  integer :: ir2(nmachs)
  integer :: jb1(nmachs)
  integer :: jt2(nmachs)
  integer :: localSize(nmachs,idim_type_min:idim_type_max)
  integer :: disp(nmachs,idim_type_min:idim_type_max)
  integer :: maxLocalSize

  real, allocatable :: LocalChunk(:)

  integer :: ng, nv

  character(len=7)            :: cProc
  character(len=16)           :: varn
  character(len=8)            :: c0, c1
  character(len=*), parameter :: h="**(saveNodeFields)**"
  logical, parameter          :: dumpLocal = .false.
  integer                     :: ierr

  write(cProc,"(a5,i2)") " Proc",mchnum

  do ng = 1, ngrids

     if (dumpLocal) then
        write(c0,"(i8)") ng
        write(*,"(a)") h//cProc//" starts processing grid "//trim(adjustl(c0))
        call flush(6)
     end if

     ! grid dependent, field independent constants for gather and unpacking
     ! as a function of idim_type
     call LocalSizesAndDisp(ng, il1, ir2, jb1, jt2, localSize, disp)

     ! maximum sizes over all fields
     maxLocalSize = maxval(LocalSize(mynum,:))

     ! maximum space for pre-processed local field
     allocate(LocalChunk(maxLocalSize), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxLocalSize
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate LocalSize("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxLocalSize
        write(*,"(a)") h//cProc//" allocated LocalSize("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     call OpenNodeWrite(25, time, iyear1, imonth1, idate1, &
          itime1, maxgrds, ng, mchnum, nmachs)
     ! for all fields on this grid

     if (dumpLocal) then
        write(*, "(4(a,l1))") h//cProc//" File Open for IOUTPUT=3"
     endif

     do nv = 1, num_var(ng)

        ! field to gather
        if (Willwrite(nv,ng)) then

           ! specific flags for this field
           thisHistFlag = vtab_r(nv,ng)%ihist==1 .and. histFlag
           thisInstFlag = vtab_r(nv,ng)%ianal==1 .and. instFlag
           thisLiteFlag = vtab_r(nv,ng)%ilite==1 .and. liteFlag
           thisMeanFlag = vtab_r(nv,ng)%imean==1 .and. meanFlag

           ! dimensionality
           idim_type = vtab_r(nv,ng)%idim_type
           if (idim_type < idim_type_min .or. idim_type > idim_type_max) then
              write(c0,"(i8)") idim_type
              call fatal_error(h//" unknown idim_type="//trim(adjustl(c0)))
           end if

           if (dumpLocal) then
              write(*,"(2(a,l1))") h//cProc//" on var_p field "//&
                   vtab_r(nv,ng)%name//"; thisHistFlag =",&
                   thisHistFlag, ", thisInstFlag=", thisInstFlag
              call flush(6)
           end if

           ! case 1: output current field values (for hist, inst and lite output files)

           if (.not.thisMeanFlag) then
              call CopyLocalChunk(vtab_r(nv,ng)%var_p, LocalChunk, &
                   LocalSize(mynum,idim_type))
              call nodeWrite(25, LocalChunk, LocalSize(mynum,idim_type))
           elseif (thisMeanFlag) then
              call CopyLocalChunk(vtab_r(nv,ng)%var_m, LocalChunk, &
                   LocalSize(mynum,idim_type))
              call nodeWrite(25, LocalChunk, LocalSize(mynum,idim_type))
           end if

        endif
     enddo

     deallocate(LocalChunk, stat=ierr)
     if (ierr /= 0) then
        write(c1,"(i8)") ierr
        call fatal_error(h//" deallocate LocalSize failed with stat="// &
             trim(adjustl(c1)))
     end if

     call CloseNodeWrite(25)

  end do

end subroutine saveNodeFields


!====
subroutine saveHdf5Par(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
     ngrids, willwrite, maxNFields)

  use node_mod, only: mchnum, nmachs, &
       ixb, ixe, iyb, iye, &
       nodemxp, nodemyp, nodeia, nodeiz, nodeja, nodejz, nodeibcon

  use mem_grid, only: &
       time, iyear1, imonth1, idate1, itime1, &
       nnzp, nnxp, nnyp, nzg, nzs, npatch

  use mem_aerad, only: &
       nwave

  use grid_dims, only: &
       maxgrds

  use var_tables, only: &
       num_var, &
       vtab_r

  use io_params, only: &
       afilout

  use hdf5_parallel_engine, only: &
       hdf_open, hdf_close, hdf_output, dims_output, hdf_parallel_output

  implicit none

  include "i8.h"

  logical, intent(in) :: histFlag       ! true iff history output requested
  logical, intent(in) :: instFlag       ! true iff instant output requested
  logical, intent(in) :: liteFlag       ! true iff lite vars output requested
  logical, intent(in) :: meanFlag       ! true iff field average output requested
  integer, intent(in) :: nvMax
  integer, intent(in) :: ngrids
  logical, intent(in) :: Willwrite(nvMax,ngrids)
  integer, intent(in) :: maxNFields

  character(len=7)            :: cProc
  character(len=16)           :: varn
  character(len=8)            :: c0, c1
  character(len=*), parameter :: h="**(saveHdf5Par)**"
  logical, parameter          :: dumpLocal = .false.
  integer                     :: ierr

  integer :: ng, nv, rankplus1
  include "files.h"
  character(len=f_name_length) :: saida ! File name for HDF5 file

  integer, parameter :: idim_type_min=2
  integer, parameter :: idim_type_max=7
  integer(kind=i8)   :: localSize(idim_type_min:idim_type_max)

  if (dumpLocal) then
     write(cProc,"(a5,i2)") " Proc", mchnum
  endif

  rankplus1 = mchnum + 1

  ! for all grids
  do ng = 1, ngrids

     if (dumpLocal) then
        write(c0,"(i8)") ng
        write(*,"(a)") h//cProc//" starts processing grid "//trim(adjustl(c0))
        call flush(6)
     end if

     call DefineNameFileWrite(time, iyear1, imonth1, idate1, itime1, &
          maxgrds, ng, "h5", saida)

     call hdf_open(saida, .true.)

     if (dumpLocal) then
        write(*,"(a)") h//cProc//" open filename: "//trim(adjustl(saida))
        call flush(6)
     end if

     ! maximum space for pre-processed local field
     localSize(2) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)
     localSize(3) = nnzp(ng)*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)
     localSize(4) = nzg*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(5) = nzs*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(6) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(7) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*nwave

     ! for all fields on this grid
     do nv = 1, num_var(ng)

        ! field to gather
        if (Willwrite(nv,ng)) then
           if (vtab_r(nv,ng)%imean/=1) then
!!$              rankplus1 = mchnum + 1
              if (dumpLocal) then
                 write(*,*) h//cProc//" var_p field "//varn//&
                      "; idim_type=", vtab_r(nv,ng)%idim_type, &
                      ", nnzp(ng), nnxp(ng), nnyp(ng), nzg, nzs, npatch, nwave=", &
                      nnzp(ng), nnxp(ng), nnyp(ng), nzg, nzs, npatch, nwave, &
                      "mchnum,rankplus1,ng,ixb,ixe,iyb,iye=", mchnum, &
                      rankplus1, ng, &
                      ixb(rankplus1,ng), ixe(rankplus1,ng), &
                      iyb(rankplus1,ng), iye(rankplus1,ng)
                 call flush(6)
              end if

              call dims_output(nnxp(ng), nnyp(ng), nnzp(ng), &
                   nzg, npatch, nzs, nwave, &
                   nodemxp(rankplus1,ng), nodemyp(rankplus1,ng), &
                   nodeia(rankplus1,ng), nodeiz(rankplus1,ng), &
                   nodeja(rankplus1,ng), nodejz(rankplus1,ng), &
                   nodeibcon(rankplus1,ng), &
                   ixb(rankplus1,ng), iyb(rankplus1,ng), &
                   vtab_r(nv,ng)%idim_type)

              if (vtab_r(nv,ng)%idim_type==2) then

                 call hdf_parallel_output(vtab_r(nv,ng)%name, &
                      vtab_r(nv,ng)%var_p_2d(:,:), &
                      localsize(vtab_r(nv,ng)%idim_type))

              elseif (vtab_r(nv,ng)%idim_type==3 .or. &
                   vtab_r(nv,ng)%idim_type==6 .or. &
                   vtab_r(nv,ng)%idim_type==7) then

                 call hdf_parallel_output(vtab_r(nv,ng)%name, &
                      vtab_r(nv,ng)%var_p_3d(:,:,:), &
                      localsize(vtab_r(nv,ng)%idim_type))

              elseif (vtab_r(nv,ng)%idim_type==4 .or. &
                   vtab_r(nv,ng)%idim_type==5) then

                 call hdf_parallel_output(vtab_r(nv,ng)%name, &
                      vtab_r(nv,ng)%var_p_4d(:,:,:,:), &
                      localsize(vtab_r(nv,ng)%idim_type))

              endif

           endif

        endif

     enddo

     call hdf_close()

  enddo

end subroutine saveHdf5Par


!====
subroutine saveBinMPIIO(histFlag, instFlag, liteFlag, meanFlag, nvMax, &
     ngrids, willwrite, maxNFields)

  use node_mod, only: mchnum, nmachs, &
       ixb, ixe, iyb, iye, &
       nodemxp, nodemyp

  use mem_grid, only: &
       time, iyear1, imonth1, idate1, itime1, &
       nnzp, nnxp, nnyp, nzg, nzs, npatch

  use mem_aerad, only: &
       nwave

  use var_tables, only: &
       num_var, &
       vtab_r

  use grid_dims, only: &
       maxgrds

  use MPI_IO_ENGINE, only: &
       INIT_FILETYPES, &
       OPEN_FILE_WRITE, &
       CLOSE_FILE_WRITE, &
       WRITE_BRAMS_DATA

  implicit none

  include "i8.h"

  logical, intent(in) :: histFlag       ! true iff history output requested
  logical, intent(in) :: instFlag       ! true iff instant output requested
  logical, intent(in) :: liteFlag       ! true iff lite vars output requested
  logical, intent(in) :: meanFlag       ! true iff field average output requested
  integer, intent(in) :: nvMax
  integer, intent(in) :: ngrids
  logical, intent(in) :: Willwrite(nvMax,ngrids)
  integer, intent(in) :: maxNFields

  character(len=7)            :: cProc
  character(len=16)           :: varn
  character(len=8)            :: c0, c1
  character(len=*), parameter :: h="**(saveBinMPIIO)**"
  logical, parameter          :: dumpLocal = .false.
  integer                     :: ierr

  integer :: ng, nv

  ! Necessary to MPI-IO
  integer :: gZoneSize ! Ghost zone size (in points per grid boundary)
  integer :: nz, nx, ny, n4, n5 ! 1 to 5th dimensions to be defined according to "idim_type"
  integer :: fileTypeRead, fileTypeWrite, fileTypeWriteGlobal
  integer :: gSizes(5), subSizesRead(5), subSizesWrite(5), startRead(5), startWrite(5), startWriteGlobal(5)
  integer :: rankplus1
  include "files.h"
  character(len=f_name_length) :: saida ! File name for MPI-IO

  integer, parameter :: idim_type_min=2
  integer, parameter :: idim_type_max=7
  integer(kind=i8)   :: localSize(idim_type_min:idim_type_max)
  integer(kind=i8)   :: maxLocalSize

  real, allocatable :: LocalChunk(:)


  if (dumpLocal) then
     write(cProc,"(a5,i2)") " Proc", mchnum
  endif

  ! Defining default ghost zone size
  gZoneSize = 1

  ! for all grids
  do ng = 1, ngrids

     if (dumpLocal) then
        write(c0,"(i8)") ng
        write(*,"(a)") h//cProc//" starts processing grid "//trim(adjustl(c0))
        call flush(6)
     end if

     ! maximum space for pre-processed local field
     rankplus1 = mchnum + 1
     localSize(2) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)
     localSize(3) = nnzp(ng)*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)
     localSize(4) = nzg*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(5) = nzs*nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(6) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*npatch
     localSize(7) = nodemxp(rankplus1,ng)*nodemyp(rankplus1,ng)*nwave

     ! maximum sizes over all fields
     maxLocalSize = maxval(LocalSize(:))

     allocate(LocalChunk(maxLocalSize), stat=ierr)
     if (ierr /= 0) then
        write(c0,"(i8)") maxLocalSize
        write(c1,"(i8)") ierr
        call fatal_error(h//" allocate LocalSize("// &
             trim(adjustl(c0))//") failed with stat="// &
             trim(adjustl(c1)))
     else if (dumpLocal) then
        write(c0,"(i8)") maxLocalSize
        write(*,"(a)") h//cProc//" allocated LocalSize("//trim(adjustl(c0))//")"
        call flush(6)
     end if

     call DefineNameFileWrite(time, iyear1, imonth1, idate1, itime1, &
          maxgrds, ng, "bin", saida)
     call OPEN_FILE_WRITE(saida)

     ! for all fields on this grid
     do nv = 1, num_var(ng)

        ! field to gather
        if (Willwrite(nv,ng)) then
           if (vtab_r(nv,ng)%imean/=1) then
!!$              rankplus1 = mchnum + 1
              if (dumpLocal) then
                 write(*,*) h//cProc//" MPI-IO on var_p field "//varn//&
                      "; idim_type=", vtab_r(nv,ng)%idim_type, &
                      ", nnzp(ng), nnxp(ng), nnyp(ng), nzg, nzs, npatch, nwave=", &
                      nnzp(ng), nnxp(ng), nnyp(ng), nzg, nzs, npatch, nwave, &
                      "mchnum,rankplus1,ng,ixb,ixe,iyb,iye=", mchnum, &
                      rankplus1, ng, &
                      ixb(rankplus1,ng), ixe(rankplus1,ng), &
                      iyb(rankplus1,ng), iye(rankplus1,ng)
                 call flush(6)
              end if
              if (vtab_r(nv,ng)%idim_type==2) then
                 nz = 1
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = 1
                 n5 = 1
              elseif (vtab_r(nv,ng)%idim_type==3) then
                 nz = nnzp(ng)
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = 1
                 n5 = 1
              elseif (vtab_r(nv,ng)%idim_type==4) then
                 nz = nzg
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = npatch
                 n5 = 1
              elseif (vtab_r(nv,ng)%idim_type==5) then
                 nz = nzs
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = npatch
                 n5 = 1
              elseif (vtab_r(nv,ng)%idim_type==6) then
                 nz = 1
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = npatch
                 n5 = 1
              elseif (vtab_r(nv,ng)%idim_type==7) then
                 nz = 1
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = nwave
                 n5 = 1
              else
                 nz = nnzp(ng)
                 nx = nnxp(ng)
                 ny = nnyp(ng)
                 n4 = 1
                 n5 = 1
              endif
              CALL INIT_FILETYPES(nz, nx, ny, n4, n5, &
                   ixb(rankplus1,ng), ixe(rankplus1,ng), &
                   iyb(rankplus1,ng), iye(rankplus1,ng), gZoneSize, &
                   gSizes, subSizesRead, subSizesWrite, &
                   startRead, startWrite, startWriteGlobal, &
                   fileTypeRead, fileTypeWrite, fileTypeWriteGlobal)
!!$              call CopyLocalChunk(vtab_r(nv,ng)%var_p, LocalChunk, &
!!$                   LocalSize(vtab_r(nv,ng)%idim_type))
!!$              call WRITE_BRAMS_DATA(LocalChunk, fileTypeWrite, &
!!$                   fileTypeWriteGlobal)
              if (vtab_r(nv,ng)%idim_type==2) then

                 call WRITE_BRAMS_DATA(vtab_r(nv,ng)%var_p_2d(:,:), &
                      fileTypeWrite, fileTypeWriteGlobal)

              elseif (vtab_r(nv,ng)%idim_type==3 .or. &
                   vtab_r(nv,ng)%idim_type==6 .or. &
                   vtab_r(nv,ng)%idim_type==7) then

                 call WRITE_BRAMS_DATA(vtab_r(nv,ng)%var_p_3d(:,:,:), &
                      fileTypeWrite, fileTypeWriteGlobal)

              elseif (vtab_r(nv,ng)%idim_type==4 .or. &
                   vtab_r(nv,ng)%idim_type==5) then

                 call WRITE_BRAMS_DATA(vtab_r(nv,ng)%var_p_4d(:,:,:,:), &
                      fileTypeWrite, fileTypeWriteGlobal)

              endif

           elseif (vtab_r(nv,ng)%imean==1) then       ! case 2: output average field values (for mean output files)

           end if

        endif

     enddo

     call CLOSE_FILE_WRITE()

  enddo

end subroutine saveBinMPIIO




!--(DMK)-------------------------------------------------------------------
subroutine OpenNodeWrite(unit, time, iyear1, imonth1, idate1, &
     itime1, maxgrds, ng, mchnum, nmachs)

  use ModDateUtils, only: &
       date_add_to

  use io_params,only: &
      afilout

  implicit none

  include "files.h"

  integer, intent(in) :: unit
  real,    intent(in) :: time
  integer, intent(in) :: iyear1
  integer, intent(in) :: imonth1
  integer, intent(in) :: idate1
  integer, intent(in) :: itime1
  integer, intent(in) :: maxgrds
  integer, intent(in) :: ng
  integer, intent(in) :: mchnum
  integer, intent(in) :: nmachs

  integer            :: ierr
  integer            :: oyear1
  integer            :: omonth1
  integer            :: odate1
  integer            :: otime1
  character(len=f_name_length) :: filename
  character(len=4)   :: yy
  character(len=2)   :: mm
  character(len=2)   :: dd
  character(len=6)   :: hh
  character(len=5)   :: pp
  character(len=5)   :: nn
  character(len=2)   :: gg
  character(len=8)   :: c0
!!$  !character(len=256), parameter :: lfsafilout='/tmp/massaru'
!!$  character(len=256), parameter :: lfsafilout='/tmp'
  character(len=7)            :: cProc
  character(len=*), parameter :: h="**(OpenNodeWrite)**"
  logical, parameter          :: dumpLocal = .false.

  write(cProc,"(a5,i2)") " Proc",mchnum


  call date_add_to(iyear1,imonth1,idate1,itime1,time,'s', &
       oyear1,omonth1,odate1,otime1)

  write (yy,'(i4.4)') oyear1
  write (mm,'(i2.2)') omonth1
  write (dd,'(i2.2)') odate1
  write (hh,'(i6.6)') otime1
  write (pp,'(i5.5)') mchnum
  write (nn,'(i5.5)') nmachs
  write (gg,'(i1)')   ng

!!$  filename=trim(lfsafilout)//"/p"//pp//"-"//nn//"-A"//yy//"-"//mm//"-"//&
!!$       dd//"-"//hh//"-g"//trim(gg)//".bin"
  filename=trim(afilout)//"-p"//pp//"-"//nn//"-A"//yy//"-"//mm//"-"//&
       dd//"-"//hh//"-g"//trim(gg)//".bin"

  if (dumpLocal) then
     write(*, "(4(a,l1))") h//cProc//" filename="//filename
  endif

  open (unit, file=filename(1:len_trim(filename)), form='unformatted', &
       iostat=ierr)

  if (ierr /= 0) then
     write(c0,"(i8)") ierr
     call fatal_error("opening "//trim(filename)//" failed with iostat "//&
          trim(adjustl(c0)))
  end if

end subroutine OpenNodeWrite
!--(DMK)-------------------------------------------------------------------



!--(DMK)-------------------------------------------------------------------
subroutine CloseNodeWrite(unit)

  implicit none

  integer, intent(in) :: unit

  close (unit)

  return
end subroutine CloseNodeWrite
!--(DMK)-------------------------------------------------------------------



!--(DMK)-------------------------------------------------------------------
subroutine nodeWrite(unit, field, LocalSize)

  implicit none

  integer, intent(in) :: unit
  real,    intent(in) :: field(LocalSize)
  integer, intent(in) :: LocalSize

  call writeField(unit, field, LocalSize)

  return
end subroutine nodeWrite
!--(DMK)-------------------------------------------------------------------


subroutine DefineNameFileWrite(time, iyear1, imonth1, idate1, &
     itime1, maxgrds, ng, sufix, filename)

  use ModDateUtils, only: &
       date_add_to

  use io_params, only: &
      afilout

  implicit none

  include "files.h"

  real,    intent(in) :: time
  integer, intent(in) :: iyear1
  integer, intent(in) :: imonth1
  integer, intent(in) :: idate1
  integer, intent(in) :: itime1
  integer, intent(in) :: maxgrds
  integer, intent(in) :: ng
  character(len=*), intent(in) :: sufix
  character(len=f_name_length), intent(out) :: filename

  integer            :: ierr
  integer            :: oyear1
  integer            :: omonth1
  integer            :: odate1
  integer            :: otime1
  character(len=4)   :: yy
  character(len=2)   :: mm
  character(len=2)   :: dd
  character(len=6)   :: hh
  character(len=5)   :: pp
  character(len=5)   :: nn
  character(len=2)   :: gg
  character(len=8)   :: c0

  call date_add_to(iyear1,imonth1,idate1,itime1,time,'s', &
       oyear1,omonth1,odate1,otime1)

  write (yy,'(i4.4)') oyear1
  write (mm,'(i2.2)') omonth1
  write (dd,'(i2.2)') odate1
  write (hh,'(i6.6)') otime1
  write (gg,'(i1)')   ng

  filename=trim(afilout)//"-A"//yy//"-"//mm//"-"//&
       dd//"-"//hh//"-g"//trim(gg)//"."//sufix

end subroutine DefineNameFileWrite



!--(DMK)-------------------------------------------------------------------
subroutine writeField(unit, field, LocalSize)

  implicit none

  integer, intent(in) :: unit
  real,    intent(in) :: field(LocalSize)
  integer, intent(in) :: LocalSize

  write(unit) LocalSize
  write(unit) field(1:LocalSize)

  return
end subroutine writeField



subroutine RearrangeForOutput(nxp, nyp, nzp, nzg, nzs, npatch, &
  idim_type, InField, OutField)
  implicit none

  integer,          intent(in   ) :: nxp
  integer,          intent(in   ) :: nyp
  integer,          intent(in   ) :: nzp
  integer,          intent(in   ) :: nzg
  integer,          intent(in   ) :: nzs
  integer,          intent(in   ) :: npatch
  integer,          intent(in   ) :: idim_type
  real,             intent(in   ) :: InField(*)
  real,             intent(out  ) :: OutField(*)

  character(len=*), parameter :: h="**(RearrangeForOutput)**"

  ! if field to be rearranged, rearrange and dump

  select case(idim_type)

  case(3)

     ! Rearrange 3-d variables from (k,i,j) to (i,j,k)
     call rearrange (nzp, nxp, nyp, InField, OutField)

  case(4)

     ! Rearrange 4-d leaf%soil variables from (k,i,j,ip) to (i,j,k,ip)
     call rearrange_p (nxp, nyp, nzg, npatch, InField, OutField)

  case (5)

     ! Rearrange 4-d leaf%sfcwater variables from (k,i,j,ip) to (i,j,k,ip)
     call rearrange_p (nxp, nyp, nzs, npatch, InField, OutField)

  end select
end subroutine RearrangeForOutput


subroutine PreProcForOutput(ngrid, varnIn, sizeInOut, &
     arrayIn, arrayOut, varnOut)
  use mem_basic, only:  &
       basic_g
  use mem_turb,  only:   &
       turb_g, idiffk, xkhkm

  implicit none

  ! pre process fields PP, HKM and VKH

  integer,          intent(in   ) :: ngrid
  character(len=*), intent(in   ) :: varnIn
  integer,          intent(in   ) :: sizeInOut
  real,             intent(in   ) :: arrayIn(sizeInOut)
  real,             intent(out  ) :: arrayOut(sizeInOut)
  character(len=*), intent(out  ) :: varnOut

  character(len=*), parameter :: h="**(PreProc)**"

  ! pre-process out before gathering

  select case (varnIn)
  case ('PP')

     ! Output total Exner function

     call RAMS_aprep_p (sizeInOut, arrayIn,  &
          basic_g(ngrid)%pi0(1,1,1), arrayOut)
     varnOut='PI'

  case ('HKM')

     ! Convert to HKM to HKH (note that VKH is HKH for Deardorff)

     call RAMS_aprep_hkh (sizeInOut, arrayIn, &
          turb_g(ngrid)%vkh(1,1,1), basic_g(ngrid)%dn0(1,1,1),  &
          arrayOut, idiffk(ngrid), xkhkm(ngrid))
     varnOut='HKH'

  case ('VKH')

     ! Un-density weight VKH

     call RAMS_aprep_vkh (sizeInOut, arrayIn, &
          basic_g(ngrid)%dn0(1,1,1), arrayOut)
     varnOut='VKH'

  case default
     varnOut=varnIn
     arrayOut = arrayIn
  end select
end subroutine PreProcForOutput


subroutine writebin(iun,var,npts)

  implicit none
  include "i8.h"
  integer(kind=i8), intent(in) :: npts
  real, intent(in)             :: var(npts)
  integer, intent(in)          :: iun
  integer                      :: i

  write(iun) (var(i),i=1,npts)

  return
end subroutine writebin

