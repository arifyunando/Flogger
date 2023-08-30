! "Flogger" is simple and fast logging library for Modern Fortran applications.
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

    ! category definition
    type :: FloggerCategory
        character(7) :: fcat_name
        character(15) :: fstyle_option
    end type FloggerCategory

    type(FloggerCategory) :: flogDebug, flogInfo, flogNotice, flogWarning, flogError, flogFatal
    parameter(flogDebug   = FloggerCategory('debug  ', k_start // trim(fg_white       ) // k_end))
    parameter(flogInfo    = FloggerCategory('info   ', k_start // trim(fg_green       ) // k_end))
    parameter(flogNotice  = FloggerCategory('notice ', k_start // trim(fg_magenta     ) // k_end))
    parameter(flogWarning = FloggerCategory('warning', k_start // trim(fg_brightyellow) // k_end))
    parameter(flogError   = FloggerCategory('ERROR!!', k_start // trim(sty_bold       ) // ';' &
                                                               // trim(fg_brightgreen ) // k_end))
    parameter(flogFatal   = FloggerCategory('FATAL!!', k_start // trim(sty_bold       ) // ';' &
                                                               // trim(fg_brightred   ) // k_end))

contains

function getTimeDate(options, useEncoding) result(out)
    implicit none
    character(*), optional, intent(in) :: options(:)
    character(:), allocatable :: out
    logical, optional :: useEncoding

    !--- local variables
    character(:), allocatable :: fmt_begin, fmt_end
    character(100) :: tmp
    character(10) :: b(3)
    integer :: date_time(8)
    
    !--- processes
    fmt_begin = getStyleEncoding(options)
    fmt_end   = getStyleEncoding()
    call date_and_time(b(1), b(2), b(3), date_time)
    
    if ( .not. present(useEncoding) ) useEncoding = .true.
    if ( useEncoding ) then
        write(tmp, 200) fmt_begin, date_time(1), date_time(2), date_time(3),    &
                        date_time(5), date_time(6), date_time(7), date_time(8), fmt_end
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

function getNameLabel(label, options, useEncoding) result(out)
    implicit none 
    character(:), allocatable :: out
    character(*), intent(in) :: label
    character(*), optional, intent(in) :: options(:)
    logical, optional :: useEncoding

    !--- local variables
    character(:), allocatable :: fmt_begin, fmt_end
    character(100) :: tmp

    !--- processes
    fmt_begin = getStyleEncoding(options)
    fmt_end   = getStyleEncoding()
    
    if ( .not. present(useEncoding) ) useEncoding = .true.
    if ( useEncoding ) then
        write(tmp, 200) trim(fmt_begin), trim(label), trim(fmt_end)
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
    character(100) :: tmp
    type(FloggerCategory) :: category(6) = [                                        &
        flogDebug, flogInfo, flogNotice,                                            &
        flogWarning, flogError, flogFatal                                           &
    ]

    !--- processes
    if ( .not. present(useEncoding) ) useEncoding = .true.
    if ( useEncoding ) then
        write(tmp, 200) trim(category(level)%fstyle_option),                        &
                        trim(category(level)%fcat_name), k_clear
    else
        write(tmp, 210) trim(category(level)%fcat_name)
    end if

    out = trim(tmp)

    !--- formatters
    200 format ('[ ', A, A, A,' ]')
    210 format ('[', A, ']')
end function

function printConsole(message, label, level) result(out)
    implicit none
    character(:), allocatable :: out
    character(*), intent(in) :: message
    character(*), intent(in) :: label
    integer, intent(in) :: level

    !--- local variables
    character(512) :: tmp

    !--- processes
    write(tmp, 200) getTimeDate(options=[fg_brightcyan], useEncoding=.true.),       &
                    getNameLabel(label, useEncoding=.true.),                        &
                    getLevelLabel(level, useEncoding=.true.),                       &
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
    write(tmp, 200) getTimeDate(options=[fg_brightcyan], useEncoding=.false.),      &
                    getNameLabel(label, useEncoding=.false.),                       &
                    getLevelLabel(level, useEncoding=.false.),                      &
                    message
    out = trim(tmp)
    
    !--- formatters
    200 format (A, ' ', A, ' ', A, ' ', A)
end function printPlainText
end module FloggerFormatter