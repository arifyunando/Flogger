module AnsiStyling
    implicit none
  
    !--- escape keys
        character(len=1), parameter :: k_esc      = achar(27)
        character(len=1), parameter :: k_end      = 'm'
        character(len=2), parameter :: k_start    = k_esc // '['
        character(len=*), parameter :: k_clear    = k_start // '0' // k_end

    !--- font style
        character(len=*), parameter :: sty_reset  = '0'
        character(len=*), parameter :: sty_bold   = '1'
        character(len=*), parameter :: sty_italic = '3'
        character(len=*), parameter :: sty_uline  = '4'
    
    !--- text colors
        character(len=*), parameter :: fg_black   = '30'
        character(len=*), parameter :: fg_red     = '31'
        character(len=*), parameter :: fg_green   = '32'
        character(len=*), parameter :: fg_yellow  = '33'
        character(len=*), parameter :: fg_blue    = '34'
        character(len=*), parameter :: fg_magenta = '35'
        character(len=*), parameter :: fg_cyan    = '36'
        character(len=*), parameter :: fg_white   = '37'

    !--- background colors
        character(len=*), parameter :: bg_black   = '40'
        character(len=*), parameter :: bg_red     = '41'
        character(len=*), parameter :: bg_green   = '42'
        character(len=*), parameter :: bg_yellow  = '43'
        character(len=*), parameter :: bg_blue    = '44'
        character(len=*), parameter :: bg_magenta = '45'
        character(len=*), parameter :: bg_cyan    = '46'
        character(len=*), parameter :: bg_white   = '47'

  
contains
  
    function getStyleEncoding(style, fg_color, bg_color) result(out)
        character(len=*), optional, intent(in) :: style, fg_color, bg_color
        character(len=:), allocatable :: out
        integer :: counter = 0

        out = k_start

        if ( present(style) ) then
            if (counter > 0) out = out // ';'
            out = out // style
            counter = counter + 1
        end if

        if ( present(fg_color) ) then
            if (counter > 0) out = out // ';'
            out = out // fg_color
            counter = counter + 1
        end if

        if ( present(bg_color) ) then
            if (counter > 0) out = out // ';'
            out = out // bg_color
            counter = counter + 1
        end if

        if ( counter == 0) out = out // '0'
        out = out // k_end
    end function getStyleEncoding
  
end module AnsiStyling