module Flogger
    use AnsiStyling
    implicit none
    
contains

function getTimeDate(style, fg_color, bg_color) result(out)
    implicit none

    character(len=*), optional, intent(in) :: style, fg_color, bg_color
    character(len=:), allocatable :: fmt_begin, fmt_end
    character(40) :: out
    character(10) :: b(3)
    integer :: date_time(8)
    
    fmt_begin = getStyleEncoding(style, fg_color, bg_color)
    fmt_end   = getStyleEncoding()

    call date_and_time(b(1), b(2), b(3), date_time)
    write(out, 200) fmt_begin, date_time(1), date_time(2), date_time(3), &
                    date_time(5), date_time(6), date_time(7), date_time(8), fmt_end
    out = trim(out)

    200 format (A, '[', I4, '-', I2.2, '-', I2.2, ' ', I2.2, ':', I2.2, ':', I2.2, '.', I3.3, ']', A)
end function
    
end module Flogger

