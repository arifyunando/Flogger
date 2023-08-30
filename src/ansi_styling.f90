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


module FloggerAnsiStyling
    implicit none
  
    !--- escape keys
        character(len=1), parameter :: k_esc      = achar(27)
        character(len=1), parameter :: k_end      = 'm'
        character(len=2), parameter :: k_start    = k_esc // '['
        character(len=*), parameter :: k_clear    = k_start // '0' // k_end

    !--- font style
        character(len=3), parameter :: sty_reset  = '0'
        character(len=3), parameter :: sty_bold   = '1'
        character(len=3), parameter :: sty_italic = '3'
        character(len=3), parameter :: sty_uline  = '4'
    
    !--- text colors
        character(len=3), parameter :: fg_black   = '30'
        character(len=3), parameter :: fg_red     = '31'
        character(len=3), parameter :: fg_green   = '32'
        character(len=3), parameter :: fg_yellow  = '33'
        character(len=3), parameter :: fg_blue    = '34'
        character(len=3), parameter :: fg_magenta = '35'
        character(len=3), parameter :: fg_cyan    = '36'
        character(len=3), parameter :: fg_white   = '37'
        character(len=3), parameter :: fg_brightblack   = '90'
        character(len=3), parameter :: fg_brightred     = '91'
        character(len=3), parameter :: fg_brightgreen   = '92'
        character(len=3), parameter :: fg_brightyellow  = '93'
        character(len=3), parameter :: fg_brightblue    = '94'
        character(len=3), parameter :: fg_brightmagenta = '95'
        character(len=3), parameter :: fg_brightcyan    = '96'
        character(len=3), parameter :: fg_brightwhite   = '97'

    !--- background colors
        character(len=3), parameter :: bg_black   = '40'
        character(len=3), parameter :: bg_red     = '41'
        character(len=3), parameter :: bg_green   = '42'
        character(len=3), parameter :: bg_yellow  = '43'
        character(len=3), parameter :: bg_blue    = '44'
        character(len=3), parameter :: bg_magenta = '45'
        character(len=3), parameter :: bg_cyan    = '46'
        character(len=3), parameter :: bg_white   = '47'
        character(len=3), parameter :: bg_brightblack   = '100'
        character(len=3), parameter :: bg_brightred     = '101'
        character(len=3), parameter :: bg_brightgreen   = '102'
        character(len=3), parameter :: bg_brightyellow  = '103'
        character(len=3), parameter :: bg_brightblue    = '104'
        character(len=3), parameter :: bg_brightmagenta = '105'
        character(len=3), parameter :: bg_brightcyan    = '106'
        character(len=3), parameter :: bg_brightwhite   = '107'

  
contains
  
    function getStyleEncoding(options) result(out)
        implicit none
        character(len=*), optional, intent(in) :: options(:)
        character(len=:), allocatable :: out
        integer :: option_count, i
        
        out = k_start
        option_count = size(options)

        if ( (.not. present(options)) .or. (option_count == 0) ) then
            out = k_clear
            return
        end if

        do i = 1, option_count
            if ( i == 1 ) then
                out = out // trim(options(i))
            else
                out = out // ';' // trim(options(i))
            end if
        end do

        out = out // k_end
        out = trim(out)
    end function getStyleEncoding
  
end module FloggerAnsiStyling