program foo
    use Flogger
    use AnsiStyling
    implicit none

    print*, getTimeDate(sty_bold, fg_cyan)

end program
