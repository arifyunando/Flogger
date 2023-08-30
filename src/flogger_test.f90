subroutine formatterTest()
    use FloggerAnsiStyling
    use FloggerFormatter
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

    ! combine test
    print 200, getTimeDate(), getNameLabel('Testing 5', [fg_blue, sty_italic])
    print 200, getTimeDate([sty_uline, fg_green]), getNameLabel("Testing 2", [sty_uline, fg_green])
    print 200, getTimeDate([fg_cyan, sty_bold]), getNameLabel("Testing 3", [fg_cyan, sty_bold])
    print 200, getTimeDate([fg_blue, sty_italic]), getNameLabel("Testing 4", [fg_black, bg_red, sty_bold])
    print 200, getTimeDate([fg_black, bg_red, sty_bold]), getNameLabel("Testing 5", [fg_blue, sty_italic])

    ! level
    print*, getLevelLabel(1)
    print*, getLevelLabel(2)
    print*, getLevelLabel(3)
    print*, getLevelLabel(4)
    print*, getLevelLabel(5)
    print*, getLevelLabel(6)
    
    do i = 1, 18
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'), &
                   getLevelLabel( mod(i, 6) + 1), "Test Message"
    end do
    do i = 1, 10
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'), &
                   getLevelLabel(2), "Test Message"
    end do
    do i = 1, 10
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerBaseClass'), &
                   getLevelLabel(4), "Test Message"
    end do
    do i = 1, 5
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'), &
                   getLevelLabel(5), "Test Message"
    end do
    do i = 1, 5
        print 200, getTimeDate([fg_brightcyan]), getNameLabel('FloggerTest'), &
                   getLevelLabel(6), "Test Message"
    end do

    ! format
    200 format (' ', A, ' ', A, ' ', A, ' ', A)
end subroutine formatterTest

program floggertest
    implicit none
    call formatterTest()
end program