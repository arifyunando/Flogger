module Flogger
    use FloggerAnsiStyling
    use FloggerFormatter
    implicit none

    integer, parameter :: FLOGS_LEVEL_DEBUG   = 0
    integer, parameter :: FLOGS_LEVEL_RELEASE = 1
    integer, parameter :: FLOGS_LEVEL_NOWARN  = 2
    integer, parameter :: FLOGS_LEVEL_SILENT  = 3
    
    ! Class-wide Variable
    integer, public :: FLOGS_LEVEL_GLOBAL = FLOGS_LEVEL_RELEASE

    ! Class-Object
    type, public :: flogType
        character(:), allocatable, private :: FLOGS_NAME 
    contains
        procedure, public :: setName => set_name_sub
        procedure, public :: debug => debug_print
        procedure, public :: info => info_print
        procedure, public :: notice => notice_print
        procedure, public :: warning => warning_print
        procedure, public :: error => error_print
        procedure, public :: fatal => fatal_print
    end type flogType

    ! make procedure name private
    private :: set_name_sub
    private :: info_print

contains

!--- setter and getter
subroutine set_name_sub(this, name)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: name
    this%FLOGS_NAME = name
end subroutine set_name_sub

!--- categories call
subroutine debug_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 1) print*, trim(print_complete(message, this%FLOGS_NAME, 1))
end subroutine debug_print

subroutine info_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 2) print*, trim(print_complete(message, this%FLOGS_NAME, 2))
end subroutine info_print

subroutine notice_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 2) print*, trim(print_complete(message, this%FLOGS_NAME, 3))
end subroutine notice_print

subroutine warning_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 2) print*, trim(print_complete(message, this%FLOGS_NAME, 4))
end subroutine warning_print

subroutine error_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 3) print*, trim(print_complete(message, this%FLOGS_NAME, 5))
end subroutine error_print

subroutine fatal_print(this, message)
    implicit none
    class(flogType) :: this
    character(*), intent(in) :: message
    if (FLOGS_LEVEL_GLOBAL < 4) print*, trim(print_complete(message, this%FLOGS_NAME, 6))
end subroutine fatal_print
end module Flogger

