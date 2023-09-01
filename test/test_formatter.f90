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
    use FloggerFormatter
    use FloggerAnsiStyling
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
    implicit none
    integer :: i
    
    print*
    print*, "!--- CHANGE FORMATTING"
    call SET_FLOGGER_STYLE(DateOptions=[FGD_CYAN])
    do i = 1, 20
        print *, printConsole("Test Message", "FloggerTest", mod(i, 6) + 1)
    end do

    print*
    print*, "!--- CHANGE FORMATTING"
    call SET_FLOGGER_STYLE(DateOptions=[FGB_CYAN])
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 1)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 2)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 3)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 4)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 5)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 4", "FloggerTest 1", 6)
    end do

    print*
    print*, "!--- CHANGE FORMATTING"
    call SET_FLOGGER_STYLE(DateOptions=[FGB_MAGENTA], LabelOptions=[FAS_ITALIC])
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 1)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 2)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 3)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 4)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 5)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 3", "FloggerTest 2", 6)
    end do
    
    print*
    print*, "!--- CHANGE FORMATTING"
    call SET_FLOGGER_STYLE(DateOptions=[FGB_CYAN], LabelOptions=[FAS_RESET])
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 1)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 2)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 3)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 4)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 5)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 2", "FloggerTest 3", 6)
    end do
        
    print*
    print*, "!--- CHANGE FORMATTING"
    call SET_FLOGGER_STYLE(DateOptions=[FGB_RED, FAS_BOLD, FAS_ITALIC])
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 1)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 2)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 3)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 4)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 5)
    end do
    do i = 1, 5
        print *, printConsole("Test Message 1", "FloggerTest 4", 6)
    end do

end subroutine test_Combinations

end program TEST_FORMATTER