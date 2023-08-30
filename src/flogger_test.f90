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


program floggertest
    use Flogger
    implicit none
    call levelClassificationTest()

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

subroutine formatterTest()

    implicit none
    integer :: i

    ! time and date
    print *, getTimeDate()
    print *, getTimeDate([sty_uline, fg_green])
    print *, getTimeDate([fg_cyan, sty_bold])
    print *, getTimeDate([fg_blue, sty_italic])
    print *, getTimeDate([fg_black, bg_red, sty_bold])
    print *, getTimeDate()

    ! name
    print *, getNameLabel("Testing 1")
    print *, getNameLabel("Testing 2", [sty_uline, fg_green])
    print *, getNameLabel("Testing 3", [fg_cyan, sty_bold])
    print *, getNameLabel("Testing 4", [fg_black, bg_red, sty_bold])
    print *, getNameLabel("Testing 5", [fg_blue, sty_italic])

    ! level
    print*, getLevelLabel(1)
    print*, getLevelLabel(2)
    print*, getLevelLabel(3)
    print*, getLevelLabel(4)
    print*, getLevelLabel(5)
    print*, getLevelLabel(6)
    
    do i = 1, 18
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'),       &
                   getLevelLabel( mod(i, 6) + 1), "Test Message"
    end do
    do i = 1, 10
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'),       &
                   getLevelLabel(2), "Test Message"
    end do
    do i = 1, 10
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerBaseClass'),  &
                   getLevelLabel(4), "Test Message"
    end do
    do i = 1, 5
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'),       &
                   getLevelLabel(5), "Test Message"
    end do
    do i = 1, 5
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'),       &
                   getLevelLabel(6), "Test Message"
    end do

    ! format
    200 format (' ', A, ' ', A, ' ', A, ' ', A)
end subroutine formatterTest

subroutine levelClassificationTest()
    implicit none
    type(FloggerUnit) :: flogs = FloggerUnit("levelClassificationTest")
    call SET_FLOGGER_OPTIONS(FileOutput=.true.)

    print*, "SET TO RELEASE"
    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 1")
    call flogs%info("Message Test Info 1")
    call flogs%notice("Message Test Notice 1")
    call flogs%warning("Message Test Warning 1")
    call flogs%error("Message Test Error 1")
    call flogs%fatal("Message Test Fatal 1")

    
    print*, "SET TO DEBUG"
    call SET_FLOGGER_OPTIONS(Level=FLOGS_SET_DEBUG)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 2")
    call flogs%info("Message Test Info 2")
    call flogs%notice("Message Test Notice 2")
    call flogs%warning("Message Test Warning 2")
    call flogs%error("Message Test Error 2")
    call flogs%fatal("Message Test Fatal 2")


    print*, "SET TO NO WARNING"
    call SET_FLOGGER_OPTIONS(Level=FLOGS_SET_NOWARN)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 3")
    call flogs%info("Message Test Info 3")
    call flogs%notice("Message Test Notice 3")
    call flogs%warning("Message Test Warning 3")
    call flogs%error("Message Test Error 3")
    call flogs%fatal("Message Test Fatal 3")
    call SET_FLOGGER_OPTIONS(FileOutput=.false.)

    print*, "SET TO SILENT"
    call SET_FLOGGER_OPTIONS(Level=FLOGS_SET_SILENT)

    call middleWarning()
    call middleNotice()
    call middleDebug()
    call flogs%debug("Message Test Debug 4")
    call flogs%info("Message Test Info 4")
    call flogs%notice("Message Test Notice 4")
    call flogs%warning("Message Test Warning 4")
    call flogs%error("Message Test Error 4")
    call flogs%fatal("Message Test Fatal 4")


    print*, "SET TO RELEASE"
    call SET_FLOGGER_OPTIONS(Level=FLOGS_SET_RELEASE)

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
end program