module cptecBramsIo

  use ModNamelistFile
  use io_params, only: &
       avgtim,  &
       frqmean, &
       frqboth, &
       StoreNamelistFileAtIo_params, &
       createIoData,    & ! Subroutine
       destroyIoData !,   & ! Subroutine

  use mem_grid, only:  &
       StoreNamelistFileAtMem_grid, &
       createMemGrid,  & !Subroutine
       nnxp,           &
       nnyp,           &
       nzg,            & ! INTENT(IN)
       ngrids,         &
       ngrid,          & ! INTENT(OUT)
       nnzp

  use ModParallelEnvironment, only : &
       ParallelEnvironment,    & ! type
       CreateParallelEnvironment !, & ! Subroutine
       !MsgDump, & ! Subroutine
       !DestroyParallelEnvironment   ! Subroutine

  use node_mod, only:  &
       mynum, &
       mchnum, &
       master_num, &
       mxp,  &
       myp,  &
       mzp,  &
       nodemzp, &
       nodemxp, &
       nodemyp, &
       nodei0, &
       nodej0, &
       StoreNamelistFileAtNode_mod, &
       StoreDomainDecompAtNode_mod, &
       StoreParallelEnvironmentAtNode_mod, &
       alloc_bounds,   & ! Subroutine
       nmachs,         & ! INTENT(IN)
       ProcessOrder      ! procedure

  use ref_sounding, only : &
       createRefSounding,  & ! subroutine
       StoreNamelistFileAtRef_sounding

  use ModGridTree, only: &
       GridTree, &
       CreateGridTree !, &

  use var_tables, only: &
    vtab_r, &
    num_var


  use ModOutputUtils, only: GetVarFromMemToOutput

!  use ModPostGrid, only: PostGrid

!  use ModPostOneFieldUtils, only: &
!    Brams2Post_topo



  implicit none

  private

  public :: readBramsHistoryDelegee    ! read brams atmopheric guess fields
  public :: writeBramsHistoryDelegee   ! write brams atmospheric analysis fields
  public :: getVariable ! get brams variable

  public :: initializeEnvironment
  public :: num_var

  integer, parameter :: stdout = 6 ! standard output
  character(len=64),parameter :: myname='cptecBramsIo'
  type(parallelEnvironment), pointer :: oneParallelEnvironment => null()
  type(GridTree), pointer :: AllGrids => null()

  type(namelistFile), pointer :: oneNamelistFile => null()

  interface getVariable
    module procedure getVariable2d
    module procedure getVariable3d
  end interface

contains

  subroutine readBramsHistoryDelegee(fileName)

    implicit none

    character(*), intent(in) :: fileName
    character(len=256) :: name_name !not used even in BRAMS
    character(len=64),parameter :: myname_=trim(myname)//' :: readBramsHistoryDelegee( )'

!#ifdef DEBUG
    print*, "===> Hello from ", trim(myname_)
!#endif

    call history_start(name_name)

  end subroutine readBramsHistoryDelegee


  subroutine writeBramsHistoryDelegee(fileName)

    implicit none
    character(*), intent(in) :: fileName
    character(len=64), parameter :: myname_=trim(myname)//' :: writeBramsHistoryDelegee( )'

!#ifdef DEBUG
    print*, "===> Hello from ", trim(myname_)
!#endif

    call OutputFields(.true., .true., .false., .false.)

  end subroutine writeBramsHistoryDelegee


  subroutine getVariable2d(varName, variable)

    implicit none

    character(*), intent(in) :: varName
    real, intent(inout) :: variable(:,:)

    integer :: gridNum = 1

    call GetVarFromMemToOutput (varName, gridNum, variable)

  end subroutine getVariable2d

  subroutine getVariable3d(varName, variable)

    implicit none

    character(*), intent(in) :: varName
    real, intent(inout) :: variable(:,:,:)

    integer :: gridNum = 1

    call GetVarFromMemToOutput (varName, gridNum, variable)

  end subroutine getVariable3d

  subroutine createAndStoreNameListValues(ramsinFileName)

    implicit none
    character(*), intent(in) :: ramsinFileName

    print*, "===> Reading RAMSIN file: ", ramsinFileName, " ... "
    call CreateNamelistFile(oneNamelistFile)
    oneNamelistFile%fileName=ramsinFileName
    call ReadNamelistFile(oneNamelistFile)
    call TimeUnitsToSeconds(oneNamelistFile)
    ! store namelist info at old places
    call StoreNamelistFileAtIo_Params(oneNamelistFile)
    call StoreNamelistFileAtMem_grid(oneNamelistFile)
    ! build and dump all grids
    call CreateGridTree(oneNamelistFile, oneParallelEnvironment, AllGrids)

  end subroutine createAndStoreNameListValues

  subroutine initializeEnvironment( nmachs_in, mchnum_in, master_num_in, ramsinFileName )

    implicit none

    include "mpif.h"

    ! Arguments:
    integer, intent(in) :: nmachs_in           ! number of processes (0 iff sequential run)
    integer, intent(in) :: master_num_in        ! this process rank (0:nmachs_in-1); 0 on sequential runs
    integer, intent(in) :: mchnum_in            ! this process rank (0:nmachs_in-1); 0 on sequential runs
    character(*), intent(in) :: ramsinFileName

    if (associated(oneNamelistFile)) then
      call createAndStoreNameListValues(ramsinFileName)
    else
      print*, "===> Initializing Environment ..."
      call CreateParallelEnvironment(nmachs_in, mchnum_in, master_num_in, &
           MPI_COMM_WORLD, oneParallelEnvironment)
      call StoreParallelEnvironmentAtNode_mod(oneParallelEnvironment)

      call createAndStoreNameListValues(ramsinFileName)
      ! Allocating "mem_grid" data
      call createMemGrid(ngrids, nnxp, nnyp, nnzp)
      ! Allocating bounds
      call alloc_bounds(ngrids, nmachs)
      call StoreDomainDecompAtNode_mod(AllGrids)
      ! Allocating IO Data
      call createIoData(ngrids)
      ! Allocating Sounding Data
      call createRefSounding(ngrids, nnzp)
        ! create process numbering (basic initialization of module node_mod @mpi/node_mod.f90)
      call ProcessOrder()
      !avgtim=1
      call decomp_node(1)
      !call rams_mem_alloc(2)
      call rams_mem_alloc(1) ! change to 2 when nodemxp and nodemyp have values

      !- ainda nÃ£o entendi como avgtim estÃ¡ sendo calculado senÃ£o em anlavg abaixo
      !por esse motivo passo o mÃ©todo rams_mem_alloc para baixo. (No ModOneProc estÃ¡ acima)

      do ngrid=1,ngrids
        !call newgrid(ngrid)
        !if ((avgtim/=0.) .and. (frqmean/=0. .or. frqboth/=0.))  &
          call anlavg(mzp,mxp,myp,nzg)
        !call cfl(mzp, mxp, myp, nodei0(mynum,ngrid), nodej0(mynum,ngrid))
      end do
    end if

  end subroutine initializeEnvironment

end module
