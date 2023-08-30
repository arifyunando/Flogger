module Flogger
    implicit none

    type, public :: flogs
        private
            integer :: FLOGS_LEVEL
            character(:), allocatable :: FLOGS_NAME
    contains
        procedure, public :: set_name => set_name_sub
        procedure, public :: set_level => set_level_sub
    end type flogs

    ! restrict access to the actual procedure names
    private :: set_name_sub, set_level_sub

contains

!--- setter and getter
subroutine set_name_sub(this, name)
    implicit none
    class(flogs) :: this
    character(:), allocatable, intent(in) :: name
    this%FLOGS_NAME = name
end subroutine set_name_sub

subroutine set_level_sub(this, level)
    implicit none
    class(flogs) :: this
    integer, intent(in) :: level
    this%FLOGS_LEVEL = level
end subroutine set_level_sub

end module Flogger

