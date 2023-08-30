program floggertest
    use FloggerAnsiStyling
    use FloggerFormatter
    use Flogger
    implicit none
    call levelClassificationTest()

contains
subroutine middleDebug()
    implicit none
    integer :: i
    type(flogType) :: flogs2
    call flogs2%setName("middleDebug")
    do i = 1, 5
        call flogs2%debug("Middle Message Test")
    end do
end subroutine middleDebug

subroutine middleWarning()
    implicit none
    integer :: i
    type(flogType) :: flogs2
    call flogs2%setName("middleWarning")
    do i = 1, 5
        call flogs2%warning("Middle Message Test")
    end do
end subroutine middleWarning

subroutine middleNotice()
    implicit none
    integer :: i
    type(flogType) :: flogs2
    call flogs2%setName("middleNotice")
    do i = 1, 5
        call flogs2%notice("Middle Message Test")
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
    type(flogType) :: flogs
    call flogs%setName("levelClassificationTest")

    print*, "RELEASE"
    call flogs%debug("Message Test Debug 1")
    call flogs%info("Message Test Info 1")
    call flogs%notice("Message Test Notice 1")
    call flogs%warning("Message Test Warning 1")
    call flogs%error("Message Test Error 1")
    call flogs%fatal("Message Test Fatal 1")
    call middleWarning()
    call middleNotice()
    call middleDebug()
    
    print*, "DEBUG"
    FLOGS_LEVEL_GLOBAL = FLOGS_LEVEL_DEBUG
    call flogs%debug("Message Test Debug 2")
    call flogs%info("Message Test Info 2")
    call flogs%notice("Message Test Notice 2")
    call flogs%warning("Message Test Warning 2")
    call flogs%error("Message Test Error 2")
    call flogs%fatal("Message Test Fatal 2")
    call middleWarning()
    call middleNotice()
    call middleDebug()

    print*, "NOWARN"
    FLOGS_LEVEL_GLOBAL = FLOGS_LEVEL_NOWARN
    call flogs%debug("Message Test Debug 3")
    call flogs%info("Message Test Info 3")
    call flogs%notice("Message Test Notice 3")
    call flogs%warning("Message Test Warning 3")
    call flogs%error("Message Test Error 3")
    call flogs%fatal("Message Test Fatal 3")
    call middleWarning()
    call middleNotice()
    call middleDebug()

    print*, "SILENT"
    FLOGS_LEVEL_GLOBAL = FLOGS_LEVEL_SILENT
    call flogs%debug("Message Test Debug 4")
    call flogs%info("Message Test Info 4")
    call flogs%notice("Message Test Notice 4")
    call flogs%warning("Message Test Warning 4")
    call flogs%error("Message Test Error 4")
    call flogs%fatal("Message Test Fatal 4")
    call middleWarning()
    call middleNotice()
    call middleDebug()

    print*, "RELEASE"
    FLOGS_LEVEL_GLOBAL = FLOGS_LEVEL_RELEASE
    call flogs%debug("Message Test Debug 5")
    call flogs%info("Message Test Info 5")
    call flogs%notice("Message Test Notice 5")
    call flogs%warning("Message Test Warning 5")
    call flogs%error("Message Test Error 5")
    call flogs%fatal("Message Test Fatal 5")
    call middleWarning()
    call middleNotice()
    call middleDebug()
end subroutine levelClassificationTest
end program