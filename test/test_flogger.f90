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


program TEST_FLOGGER
    use Flogger
    implicit none
    real :: stressTime(5)
    integer :: iteration = 100000
    character(20) :: args

    call get_command_argument(number=1, value=args)

    print*
    print*, "##################################################################"
    print*, "                           TEST FLOGGER                           "
    print*, "##################################################################"
    
    if ( args == "-fstress-test" ) then
        call stress_test_basic(iteration, stresstime(1))
        call stress_test_plain(iteration, stresstime(2))
        call stress_test_empty(iteration, stresstime(3))
        call stress_test_noDTnoLabel(iteration, stresstime(4))
        call stress_test_fortranprint(iteration, stresstime(5))
    
        print*, "***************************************************************"
        print*, " Single Thread, ", iteration, " iterations"
        print*, "***************************************************************"
        print 200, "Basic Test", stressTime(1), iteration/stressTime(1) 
        print 200, "Plain Test", stressTime(2), iteration/stressTime(2)
        print 200, "Empty Test", stressTime(3), iteration/stressTime(3)
        print 200, "NoDTnoLabel Test", stressTime(4), iteration/stressTime(4)
        print 200, "Fortran Print", stressTime(5), iteration/stressTime(5)
        print*, "***************************************************************"
    end if

    call levelClassificationTest()

    200 format (" ", A18, 4X, "Elapsed: ", F6.3, " secs", 4X, F12.3, " /sec")
contains

subroutine middleDebug()
    implicit none
    integer :: i
    type(FloggerUnit) :: flog = FloggerUnit("MiddleDebug")
    do i = 1, 5
        call flog%debug("Middle Message Test")
    end do
end subroutine middleDebug


subroutine middleWarning()
    implicit none
    integer :: i
    type(FloggerUnit) :: flog = FloggerUnit("MiddleWarning")
    do i = 1, 5
        call flog%warning("Middle Message Test")
    end do
end subroutine middleWarning


subroutine middleNotice()
    implicit none
    integer :: i
    type(FloggerUnit) :: flog = FloggerUnit("MiddleNotice")
    do i = 1, 5
        call flog%notice("Middle Message Test")
    end do
end subroutine middleNotice


subroutine levelClassificationTest()
    use FloggerFormatter, only: flogger_set_section
    implicit none
    type(FloggerUnit) :: flogs = FloggerUnit("levelClassificationTest")
    call flogger_set_section(addDateTime=.true., addLabel=.true.)
    call flogger_set_options(FileOutput=.true., useEncoding=.true.)

    print*
    print*, "!--- DEFAULT BEHAVIOR (DEBUG)"
    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 1")
    call flogs%info("Message Test Info 1")
    call flogs%notice("Message Test Notice 1")
    call flogs%warning("Message Test Warning 1")
    call flogs%error("Message Test Error 1")
    call flogs%fatal("Message Test Fatal 1")
    
    print*
    print*, "!--- SET TO RELEASE"
    call flogger_set_options(Level=FLOGS_SET_RELEASE)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 2")
    call flogs%info("Message Test Info 2")
    call flogs%notice("Message Test Notice 2")
    call flogs%warning("Message Test Warning 2")
    call flogs%error("Message Test Error 2")
    call flogs%fatal("Message Test Fatal 2")

    print*
    print*, "!--- SET TO NO WARNING"
    call flogger_set_options(Level=FLOGS_SET_NOWARN)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 3")
    call flogs%info("Message Test Info 3")
    call flogs%notice("Message Test Notice 3")
    call flogs%warning("Message Test Warning 3")
    call flogs%error("Message Test Error 3")
    call flogs%fatal("Message Test Fatal 3")
    call flogger_set_options(FileOutput=.false.)

    print*
    print*, "!--- SET TO SILENT"
    call flogger_set_options(Level=FLOGS_SET_SILENT, useEncoding=.false.)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 4")
    call flogs%info("Message Test Info 4")
    call flogs%notice("Message Test Notice 4")
    call flogs%warning("Message Test Warning 4")
    call flogs%error("Message Test Error 4")
    call flogs%fatal("Message Test Fatal 4")

    print*
    print*, "!--- SET TO RELEASE"
    call flogger_set_options(Level=FLOGS_SET_RELEASE)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 5")
    call flogs%info("Message Test Info 5")
    call flogs%notice("Message Test Notice 5")
    call flogs%warning("Message Test Warning 5")
    call flogs%error("Message Test Error 5")
    call flogs%fatal("Message Test Fatal 5")

end subroutine levelClassificationTest


subroutine stress_test_basic(n_iteration, time)
    implicit none
    integer, intent(in) :: n_iteration
    real, intent(out) ::  time
    integer :: i
    real :: timein, timeout
    character(10) :: tmp

    type(FloggerUnit) :: flog = FloggerUnit("Stress Test 1")
    call flogger_set_options(UseEncoding=.true., ConsolePrint=.true.,           &
                             level=FLOGS_SET_DEBUG)

    call cpu_time(timein)
    do i = 1, n_iteration
        write(tmp, "(I7)") i
        call flog%warning("Basic Stress Test Message" // tmp)
    end do
    call cpu_time(timeout)

    time = timeout - timein
end subroutine stress_test_basic


subroutine stress_test_plain(n_iteration, time)
    implicit none
    integer, intent(in) :: n_iteration
    real, intent(out) ::  time
    integer :: i
    real :: timein, timeout
    character(10) :: tmp

    type(FloggerUnit) :: flog = FloggerUnit("Stress Test 2")
    call flogger_set_options(UseEncoding=.false., ConsolePrint=.true.,          &
                             level=FLOGS_SET_DEBUG)

    call cpu_time(timein)
    do i = 1, n_iteration
        write(tmp, "(I7)") i
        call flog%warning("Plain Stress Test Message" // tmp)
    end do
    call cpu_time(timeout)

    time = timeout - timein
end subroutine stress_test_plain


subroutine stress_test_empty(n_iteration, time)
    implicit none
    integer, intent(in) :: n_iteration
    real, intent(out) ::  time
    integer :: i
    real :: timein, timeout
    character(10) :: tmp

    type(FloggerUnit) :: flog = FloggerUnit("Stress Test 3")
    call flogger_set_options(UseEncoding=.false., ConsolePrint=.false.,         &
                             level=FLOGS_SET_DEBUG)

    call cpu_time(timein)
    do i = 1, n_iteration
        write(tmp, "(I7)") i
        call flog%warning("Empty Stress Test Message" // tmp)
    end do
    call cpu_time(timeout)

    time = timeout - timein
end subroutine stress_test_empty


subroutine stress_test_noDTnoLabel(n_iteration, time)
    use FloggerFormatter, only: flogger_set_section
    implicit none
    integer, intent(in) :: n_iteration
    real, intent(out) ::  time

    integer :: i
    real :: timein, timeout
    character(10) :: tmp

    type(FloggerUnit) :: flogsta = FloggerUnit("Stress Test 4")
    call flogger_set_options(UseEncoding=.false., ConsolePrint=.true.,          &
                             level=FLOGS_SET_DEBUG)
    call flogger_set_section(addDateTime=.false., addLabel=.false.)

    call cpu_time(timein)
    do i = 1, n_iteration
        write(tmp, "(I7)") i
        call flogsta%warning("noDTnoLabel Stress Test Message" // tmp)
    end do
    call cpu_time(timeout)

    time = timeout - timein
end subroutine stress_test_noDTnoLabel


subroutine stress_test_fortranprint(n_iteration, time)
    use FloggerFormatter, only: flogger_set_section
    implicit none
    integer, intent(in) :: n_iteration
    real, intent(out) ::  time
    integer :: i
    real :: timein, timeout

    call flogger_set_options(UseEncoding=.false., ConsolePrint=.true., level=FLOGS_SET_DEBUG)
    call flogger_set_section(addDateTime=.false., addLabel=.false.)
    call cpu_time(timein)
    do i = 1, n_iteration
        print*, i
    end do
    call cpu_time(timeout)

    time = timeout - timein
end subroutine stress_test_fortranprint

end program TEST_FLOGGER