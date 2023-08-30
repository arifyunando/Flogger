module Flogger
    use FloggerFormatter
    implicit none

    ! Compile Mode ID
    integer, parameter :: FLOGS_SET_DEBUG   = 0
    integer, parameter :: FLOGS_SET_RELEASE = 1
    integer, parameter :: FLOGS_SET_NOWARN  = 2
    integer, parameter :: FLOGS_SET_SILENT  = 3

    ! Level-ID
    integer, parameter, private :: FLOGS_ID_DEBUG    = 1
    integer, parameter, private :: FLOGS_ID_INFO     = 2
    integer, parameter, private :: FLOGS_ID_NOTICE   = 3
    integer, parameter, private :: FLOGS_ID_WARNING  = 4
    integer, parameter, private :: FLOGS_ID_ERROR    = 5
    integer, parameter, private :: FLOGS_ID_FATAL    = 6
    
    ! Class-wide Variable
    integer, private :: FLOGS_LEVEL_GLOBAL = FLOGS_SET_DEBUG
    logical, private :: FLOGS_USE_ENCODING = .true.
    logical, private :: FLOGS_CONSOLE_PRINT = .true.

    ! Class-Object
    type, public :: FloggerUnit
        character(64) :: id 
    contains
        procedure, public :: debug => debug_print
        procedure, public :: info => info_print
        procedure, public :: notice => notice_print
        procedure, public :: warning => warning_print
        procedure, public :: error => error_print
        procedure, public :: fatal => fatal_print
    end type FloggerUnit

    ! make procedure name private
    private :: debug_print, info_print, notice_print, warning_print, error_print, fatal_print

contains

!--- Class-Methods
subroutine SET_FLOGS_OPTIONS(Level, UseEncoding, ConsolePrint, FileOutput)
    integer, optional, intent(in) :: Level
    logical, optional, intent(in) :: UseEncoding
    logical, optional, intent(in) :: ConsolePrint
    logical, optional, intent(in) :: FileOutput

    !--- processes
    if ( present(Level) ) then
        FLOGS_LEVEL_GLOBAL = Level
    end if

    if ( present(UseEncoding) ) then
        FLOGS_USE_ENCODING = UseEncoding
    end if

    if ( present(ConsolePrint) ) then
        FLOGS_CONSOLE_PRINT = ConsolePrint
    end if

    if ( present(FileOutput) ) then
        
    end if
end subroutine SET_FLOGS_OPTIONS

!--- Object-Methods / Object-Procedures
subroutine debug_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp

    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 1 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printConsole(message, this%id, FLOGS_ID_DEBUG)
        else
            tmp = printPlainText(message, this%id, FLOGS_ID_DEBUG)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine debug_print

subroutine info_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp

    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 2 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printConsole(message, this%id, FLOGS_ID_INFO)
        else
            tmp = printPlainText(message, this%id, FLOGS_ID_INFO)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine info_print

subroutine notice_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp

    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 2 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printConsole(message, this%id, FLOGS_ID_NOTICE)
        else
            tmp = printPlainText(message, this%id, FLOGS_ID_NOTICE)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine notice_print

subroutine warning_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp
    
    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 2 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printConsole(message, this%id, FLOGS_ID_WARNING)
        else
            tmp = printPlainText(message, this%id, FLOGS_ID_WARNING)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine warning_print

subroutine error_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp
    
    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 3 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printconsole(message, this%id, FLOGS_ID_ERROR)
        else
            tmp = printplaintext(message, this%id, FLOGS_ID_ERROR)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine error_print

subroutine fatal_print(this, message, output)
    implicit none
    class(FloggerUnit) :: this
    character(:), allocatable, optional, intent(out) :: output
    character(*), intent(in) :: message
    character(512) :: tmp

    !--- processes
    if ( FLOGS_LEVEL_GLOBAL < 4 ) then
        if ( FLOGS_USE_ENCODING ) then
            tmp = printconsole(message, this%id, FLOGS_ID_FATAL)
        else
            tmp = printplaintext(message, this%id, FLOGS_ID_FATAL)
        end if

        if ( FLOGS_CONSOLE_PRINT ) print*, trim(tmp)
        if ( present(output) ) output = trim(tmp)

    end if
end subroutine fatal_print
end module Flogger

