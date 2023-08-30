module FloggerFormatter
    use FloggerAnsiStyling
    implicit none

    ! category definition
    type :: flog_categories
        character(7) :: fcat_name
        character(15) :: fstyle_option
    end type flog_categories

    type(flog_categories) :: flogDebug, flogInfo, flogNotice, flogWarning, flogError, flogFatal
    parameter(flogDebug   = flog_categories('debug  ', k_start // trim(fg_white       ) // k_end))
    parameter(flogInfo    = flog_categories('info   ', k_start // trim(fg_green       ) // k_end))
    parameter(flogNotice  = flog_categories('notice ', k_start // trim(fg_magenta     ) // k_end))
    parameter(flogWarning = flog_categories('warning', k_start // trim(fg_brightyellow) // k_end))
    parameter(flogError   = flog_categories('ERROR!!', k_start // trim(sty_bold       ) // ';' &
                                                               // trim(fg_brightgreen ) // k_end))
    parameter(flogFatal   = flog_categories('FATAL!!', k_start // trim(sty_bold       ) // ';' &
                                                               // trim(fg_brightred   ) // k_end))
contains

function getTimeDate(options) result(out)
    implicit none
    character(len=*), optional, intent(in) :: options(:)
    character(:), allocatable :: out

    character(len=:), allocatable :: fmt_begin, fmt_end
    character(100) :: tmp
    character(10) :: b(3)
    integer :: date_time(8)
    
    fmt_begin = getStyleEncoding(options)
    fmt_end   = getStyleEncoding()

    call date_and_time(b(1), b(2), b(3), date_time)
    write(tmp, 200) fmt_begin, date_time(1), date_time(2), date_time(3), &
                    date_time(5), date_time(6), date_time(7), date_time(8), fmt_end
    out = trim(tmp)

    200 format (A, '[', I4, '-', I2.2, '-', I2.2, ' ', & 
                I2.2, ':', I2.2, ':', I2.2, '.', I3.3, ']', A)
end function

function getNameLabel(label, options) result(out)
    implicit none 
    character(*), intent(in) :: label
    character(*), optional, intent(in) :: options(:)
    character(:), allocatable :: out

    character(:), allocatable :: fmt_begin, fmt_end
    character(100) :: tmp

    fmt_begin = getStyleEncoding(options)
    fmt_end   = getStyleEncoding()

    write(tmp, 200) trim(fmt_begin), trim(label), trim(fmt_end)
    out = trim(tmp)

    200 format (A, '[', A,']', A)
end function

function getLevelLabel(level) result(out)
    implicit none 
    integer, intent(in) :: level
    character(:), allocatable :: out

    character(100) :: tmp
    type(flog_categories) :: category(6) = [    &
        flogDebug, flogInfo, flogNotice,        &
        flogWarning, flogError, flogFatal       &
    ]

    write(tmp, 200) trim(category(level)%fstyle_option), trim(category(level)%fcat_name), k_clear
    out = trim(tmp)

    200 format ('[ ', A, A, A,' ]')
end function

function print_complete(message, label, level) result(out)
    implicit none
    character(*), intent(in) :: message
    character(*), intent(in) :: label
    integer, intent(in) :: level
    character(:), allocatable :: out

    character(512) :: tmp
    write(tmp, 200) getTimeDate([fg_brightcyan]), getNameLabel(label), getLevelLabel(level), message
    out = trim(tmp)
    
    200 format (' ', A, ' ', A, ' ', A, ' ', A)
end function print_complete

end module FloggerFormatter