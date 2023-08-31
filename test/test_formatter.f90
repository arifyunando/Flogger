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
    implicit none
    print*
    print*, "##################################################################"
    print*, "                          TEST FORMATTER                          "
    print*, "##################################################################"
    call test_getDateTime()
    call test_getNameLabel()
    call test_getLevelLabel()
    call test_Combinations()

contains

subroutine test_getDateTime()
    implicit none
    integer :: i
    print*
    print*, "!--- TESTING getDateTime"
    do i = 1, 5
        print *, getDateTime()
    end do
end subroutine test_getDateTime

subroutine test_getNameLabel()
    implicit none
    integer :: i
    character(50) :: tmp

    print*
    print*, "!--- TESTING getNameLabel"
    do i = 1, 5
        write(tmp, *) "Testing", i
        print *, getNameLabel(tmp)
    end do
end subroutine test_getNameLabel

subroutine test_getLevelLabel()
    implicit none
    
    integer :: i

    print*
    print*, "!--- TESTING getLevelLabel"
    do i = 1, 6
        print *, getLevelLabel(i)
    end do
end subroutine test_getLevelLabel

subroutine test_Combinations()
    implicit none
    integer :: i
    
    print*
    print*, "!--- TESTING COMBINATIONS"
    call SET_FLOGGER_STYLE(DateOptions=[FGD_CYAN])
    do i = 1, 10
        print 200,  getDateTime(),                                              &
                    getNameLabel('FloggerTest'),                                &
                    getLevelLabel( mod(i, 6) + 1), "Test Message"
    end do

    call SET_FLOGGER_STYLE(DateOptions=[FGB_CYAN])
    do i = 1, 10
        print 200,  getDateTime(),                                              &
                    getNameLabel('FloggerTest'),                                &
                    getLevelLabel(2), "Test Message"
    end do
    
    call SET_FLOGGER_STYLE(DateOptions=[FGB_MAGENTA], LabelOptions=[FAS_ITALIC])
    do i = 1, 10
        print 200,  getDateTime(),                                              &
                    getNameLabel('FloggerBaseClass'),                           &
                    getLevelLabel(4), "Test Message"
    end do
    
    call SET_FLOGGER_STYLE(DateOptions=[FGB_CYAN], LabelOptions=[FAS_RESET])
    do i = 1, 5
        print 200,  getDateTime(),                                              &
                    getNameLabel('FloggerTest'),                                &
                    getLevelLabel(5), "Test Message"
    end do
    
    call SET_FLOGGER_STYLE(DateOptions=[FGB_RED, FAS_BOLD, FAS_ITALIC])
    do i = 1, 5
        print 200,  getDateTime(),                                              &
                    getNameLabel('FloggerTest'),                                &
                    getLevelLabel(6), "Test Message"
    end do

    200 format (' ', A, ' ', A, ' ', A, ' ', A)
end subroutine test_Combinations

end program TEST_FORMATTER