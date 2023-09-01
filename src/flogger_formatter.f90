! "Flogger" is simple and fast logging library for Modern Fortran applications.
! https://github.com/arifyunando/flogger
!
! MIT License
!
! Copyright (c) 2023 Arif Y. Sunanhadikusuma
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.


module FloggerFormatter
    use FloggerAnsiStyling
    implicit none
    private
    ! Date Styles
    character(64):: FLOGS_STYLE_LABEL = FAS_K_CLEAR
    character(64):: FLOGS_STYLE_TEXT  = FAS_K_CLEAR
    character(64):: FLOGS_STYLE_DATETIME                                        &
                        = FAS_K_START // trim(FGB_CYAN) // FAS_K_END
    
    ! Default Category Styles
    character(15) :: STY_DEBUG, STY_INFO, STY_NOTE
    character(15) :: STY_WARN, STY_ERROR, STY_FATAL
    parameter(STY_DEBUG = FAS_K_START // trim(FGD_WHITE)   // FAS_K_END)
    parameter(STY_INFO  = FAS_K_START // trim(FGD_GREEN)   // FAS_K_END)
    parameter(STY_NOTE  = FAS_K_START // trim(FGD_MAGENTA) // FAS_K_END)
    parameter(STY_WARN  = FAS_K_START // trim(FGB_YELLOW)  // FAS_K_END)
    parameter(STY_ERROR = FAS_K_START // trim(FAS_BOLD)    // ';'               &
                                      // trim(FGB_GREEN)   // FAS_K_END)
    parameter(STY_FATAL = FAS_K_START // trim(FAS_BOLD)    // ';'               &
                                      // trim(FGB_RED)     // FAS_K_END)
                                     
    ! Category Definition
    type :: FloggerCategory
        character(7) :: name
        character(15) :: style
    end type FloggerCategory

    type(FloggerCategory) :: flogDebug, flogInfo, flogNotice
    type(FloggerCategory) :: flogWarning, flogError, flogFatal
    parameter(flogDebug   = FloggerCategory('debug  ', STY_DEBUG))
    parameter(flogInfo    = FloggerCategory('info   ', STY_INFO))
    parameter(flogNotice  = FloggerCategory('notice ', STY_NOTE))
    parameter(flogWarning = FloggerCategory('warning', STY_WARN))
    parameter(flogError   = FloggerCategory('ERROR!!', STY_ERROR))
    parameter(flogFatal   = FloggerCategory('FATAL!!', STY_FATAL))

    public :: SET_FLOGGER_STYLE, PRINT_FILE_HEADER
    public :: printConsole, printPlainText
contains

!--- PRIVATE FUNCTIONS / SUBROUTINES

function getDateTime(useEncoding) result(out)
    implicit none

    character(:), allocatable :: out
    logical, optional :: useEncoding

    !--- local variables
    integer :: date_time(8)
    logical :: useEncodingLocal = .true.
    character(100) :: tmp

    !--- processes
    call date_and_time(values=date_time)
    
    if ( present(useEncoding) ) useEncodingLocal = useEncoding
    if ( useEncodingLocal ) then
        write(tmp, 200) trim(FLOGS_STYLE_DATETIME),                             &
                        date_time(1), date_time(2), date_time(3),               &
                        date_time(5), date_time(6), date_time(7), date_time(8), &
                        FAS_K_CLEAR
    else
        write(tmp, 210) date_time(1), date_time(2), date_time(3),               &
                        date_time(5), date_time(6), date_time(7), date_time(8)
    end if

    out = trim(tmp)

    !--- formatters
    200 format (A, '[', I4, '-', I2.2, '-', I2.2, ' ',                          &
                I2.2, ':', I2.2, ':', I2.2, '.', I3.3, ']', A)
    210 format ('[', I4, '-', I2.2, '-', I2.2, ' ',                             & 
                I2.2, ':', I2.2, ':', I2.2, '.', I3.3, ']')
end function


function getNameLabel(label, useEncoding) result(out)
    implicit none
    character(:), allocatable :: out
    character(*), intent(in) :: label
    logical, optional :: useEncoding

    !--- local variables
    character(100) :: tmp
    logical :: useEncodingLocal = .true.

    !--- processes
    if ( present(useEncoding) ) useEncodingLocal = useEncoding
    if ( useEncodingLocal ) then
        write(tmp, 200) trim(FLOGS_STYLE_LABEL), trim(label), FAS_K_CLEAR
    else
        write(tmp, 210) trim(label)
    end if

    out = trim(tmp)

    !--- formatters
    200 format (A, '[', A, ']', A)
    210 format ('[', A, ']')
end function


function getLevelLabel(level, useEncoding) result(out)
    implicit none

    character(:), allocatable :: out
    integer, intent(in) :: level
    logical, optional :: useEncoding

    !--- local variables
    type(FloggerCategory) :: lv(6) = [                                          &
        flogDebug, flogInfo, flogNotice, flogWarning, flogError, flogFatal      &
    ]
    character(100) :: tmp
    logical :: useEncodingLocal = .true.

    !--- processes
    if ( present(useEncoding) ) useEncodingLocal = useEncoding
    if ( useEncodingLocal ) then
        write(tmp, 200) trim(lv(level)%style), trim(lv(level)%name), FAS_K_CLEAR
    else
        write(tmp, 210) trim(lv(level)%name)
    end if

    out = trim(tmp)

    !--- formatters
    200 format ('[', A, A, A, ']')
    210 format ('[', A, ']')
end function

!--- PUBLIC FUNCTIONS / SUBROUTINES

subroutine SET_FLOGGER_STYLE(LabelOptions,  DateOptions, TextOptions)
    implicit none

    character(*), optional, intent(in) :: LabelOptions(:)
    character(*), optional, intent(in) :: DateOptions(:)
    character(*), optional, intent(in) :: TextOptions(:)

    if ( present(LabelOptions) ) &
        FLOGS_STYLE_LABEL = getStyleEncoding(LabelOptions)

    if ( present(DateOptions) ) &
        FLOGS_STYLE_DATETIME = getStyleEncoding(DateOptions)

    if ( present(TextOptions) ) &
        FLOGS_STYLE_TEXT = getStyleEncoding(TextOptions)
end subroutine SET_FLOGGER_STYLE


subroutine PRINT_FILE_HEADER(unit)
    integer, intent(in) :: unit

    write(unit, 200) 
    write(unit, 210) "LOGFILES GENERATED BY FLOGGER"
    write(unit, 220) getDateTime(UseEncoding=.false.)
    write(unit, 200) 
    write(unit, *) 

    !--- formatters
    210 format (34X, A29, 34X)
    220 format (36X, A25, 36X)
    200 format ("=================================================",            &
                "================================================")
end subroutine PRINT_FILE_HEADER


function printConsole(message, label, level) result(out)
    implicit none

    character(:), allocatable :: out
    character(*), intent(in) :: message
    character(*), intent(in) :: label
    integer, intent(in) :: level

    !--- local variables
    character(512) :: tmp

    !--- processes
    write(tmp, 200) getDateTime(useEncoding=.true.),                            &
                    getNameLabel(label, useEncoding=.true.),                    &
                    getLevelLabel(level, useEncoding=.true.),                   &
                    message
    out = trim(tmp)

    !--- formatters
    200 format (A, ' ', A, ' ', A, ' ', A)
end function printConsole


function printPlainText(message, label, level) result(out)
    implicit none

    character(:), allocatable :: out
    character(*), intent(in) :: message
    character(*), intent(in) :: label
    integer, intent(in) :: level

    !--- local variables
    character(512) :: tmp

    !--- processes
    write(tmp, 200) getDateTime(useEncoding=.false.),                           &
                    getNameLabel(label, useEncoding=.false.),                   &
                    getLevelLabel(level, useEncoding=.false.),                  &
                    message
    out = trim(tmp)

    !--- formatters
    200 format (A, ' ', A, ' ', A, ' ', A)
end function printPlainText

end module FloggerFormatter