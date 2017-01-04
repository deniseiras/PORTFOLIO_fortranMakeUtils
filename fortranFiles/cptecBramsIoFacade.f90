module cptecBramsIoFacade

  implicit none

  private

  public :: readBramsHistory    ! read brams atmopheric guess fields
  public :: writeBramsHistory   ! write brams atmospheric analysis fields
  public :: getVariable ! get brams variable
  public :: setVariable ! set variable value to write brams history

  interface getVariable
    module procedure getVariable2d
    module procedure getVariable3d
  end interface

  contains


  subroutine readBramsHistory(fileName)

    use cptecBramsIo, only: readBramsHistoryDelegee
    implicit none

    character(*), intent(in) :: fileName

    call readBramsHistoryDelegee(fileName)

  end subroutine readBramsHistory


  subroutine writeBramsHistory(fileName)

    use cptecBramsIo, only: writeBramsHistoryDelegee

    implicit none

    character(*), intent(in) :: fileName

    call writeBramsHistoryDelegee(fileName)

  end subroutine writeBramsHistory


  subroutine getVariable2d(varName, variable)
    implicit none

    character(*), intent(in) :: varName
    real, allocatable, intent(out) :: variable(:,:)

  end subroutine getVariable2d


  subroutine getVariable3d(varName, variable)
    implicit none

    character(*), intent(in) :: varName
    real, allocatable, intent(out) :: variable(:,:,:)

  end subroutine getVariable3d


  subroutine setVariable(varName, variable)
    implicit none

    character(*), intent(in) :: varName
    real, intent(in) :: variable(:,:)

  end subroutine setVariable


end module cptecBramsIoFacade