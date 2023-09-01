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

program TEST_FORMATTER
    implicit none
    real :: timestart, timeend

    print*
    print*, "##################################################################"
    print*, "                          TEST FORMATTER                          "
    print*, "##################################################################"
    
    call cpu_time(timestart)
    call test_Combinations()
    call cpu_time(timeend)
    
    print*
    print*, "##################################################################"
    print 100, (timeend - timestart)*1000
    print*, "##################################################################"

    100 format (" ","Finish Testing Formatter, total test time = ", G0, ' ms')
contains

subroutine test_Combinations()
    use FloggerAnsiStyling
    use FloggerFormatter
    implicit none
    integer :: i, j
    
    print*
    print*, "!--- CHANGE FORMATTING"
    call flogger_set_style(DateOptions=[FGD_CYAN])
    do i = 1, 20
        print *, printFormatted("Test Message", "FloggerTest", mod(i, 6) + 1, .true.)
    end do

    print*
    print*, "!--- CHANGE FORMATTING"
    call flogger_set_style(DateOptions=[FGB_CYAN])
    do j = 1, 6
        do i = 1, 5
            print *, printFormatted("Test Message 4", "FloggerTest 1", j, .true.)
        end do
    end do

    print*
    print*, "!--- CHANGE FORMATTING"
    call flogger_set_style(DateOptions=[FGB_MAGENTA], LabelOptions=[FAS_ITALIC])
    do j = 1, 6
        do i = 1, 5
            print *, printFormatted("Test Message 3", "FloggerTest 2", j, .true.)
        end do
    end do
    
    print*
    print*, "!--- CHANGE FORMATTING"
    call flogger_set_style(DateOptions=[FGB_CYAN], LabelOptions=[FAS_RESET])
    do j = 1, 6
        do i = 1, 5
            print *, printFormatted("Test Message 2", "FloggerTest 3", j, .true.)
        end do
    end do
        
    print*
    print*, "!--- CHANGE FORMATTING"
    call flogger_set_style(DateOptions=[FGB_RED, FAS_BOLD, FAS_ITALIC])
    do j = 1, 6
        do i = 1, 5
            print *, printFormatted("Test Message 1", "FloggerTest 4", j, .true.)
        end do
    end do

end subroutine test_Combinations

end program TEST_FORMATTER