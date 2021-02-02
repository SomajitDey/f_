!@Somajit Dey <somajit@users.sourceforge.net> February 2021
!
! Copyright (C) 2020-2021 Somajit Dey
! Department of Physics, University of Calcutta
! Email: somajit@users.sourceforge.net
! 
! This file is part of f_
! 
! f_ is a free software library: you can redistribute it and/or modify it
! under the terms of the GNU Lesser General Public License as published by the
! Free Software Foundation, either version 2.1 of the License, or (at your
! option) any later version.
!   
! f_ is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!   
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!************************************************************************

! BRIEF:
! This module contains some useful routines for interacting with a terminal.
! Although ncurses is the de-facto standard for this, we try to avoid dependency
! on libncurses library in the spirit of KISS. The price for this is heavy use of
! execute_command_line. Hence this module is suitable only for lightweight programs
! that do not require a lot of RAM. Hopefully, most UI front-ends will simply be
! drivers of the more heavy-weight back-ends. Hence, calling execute_command_line
! should not be an issue.
!************************************************************************

! SIGSEGV RUNTIME ERROR:
! Any such error possibly arises from type mismatch when porting from 32-bit to 
! 64-bit. E.g. equating INT32 with C_LONG variable type. C_LONG can take both 32-bit
! and 64-bit form depending on the implementation.
!************************************************************************

! WARNING:
! Below we use the bash built-in command, read. Hence, the specific use of bash.
! Note that execute_command_line invokes sh, not bash.
! Also some terminals that do not support tput may cause trouble.
!************************************************************************

! REFERENCE:
! Command: read --help
! For tput: http://linuxcommand.org/lc3_adv_tput.php
!************************************************************************

module f_tty

use f_utils
use f_syscall

implicit none
private

integer, parameter :: red=1, green=2, yellow=3, blue=4, magenta=5, cyan=6

!~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Keyboard input without pressing Enter key
public :: f_keypress, f_getch, f_getyesno

!Display style
public :: f_bold, f_outstanding, f_underline, f_italic, f_blink

!Display color
public :: f_red, f_green, f_yellow, f_blue, f_magenta, f_cyan

!Get #Lines(Rows) and #Columns in terminal window
public :: f_termheight, f_termwidth

!Clear screen
public :: f_clrscr

!Prompt or Cursor
public :: f_hideprompt, f_showprompt
!~~~~~~~~~~~~~~~~~~~~~~~~~~~END CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contains

subroutine f_keypress()
call execute_command_line('bash -c "read -sn 1"')
end subroutine f_keypress

function f_getch(nchars,echo,timeout)
integer, intent(in), optional :: nchars, timeout
logical, intent(in), optional :: echo
character(:), allocatable :: f_getch, cmd, nflag, sflag, tflag
integer :: pipe, istat
character(len=500) :: buffer
!Setting defaults first:
nflag=' -n 1'
sflag=' -s'
tflag=''
if(present(nchars))then
    if(nchars>0)then
        nflag=' -n '//f_int_to_char(nchars)
    else
        nflag=''
    endif
endif
if(present(echo))then
    if(echo)sflag=''
endif
if(present(timeout))then
    if(timeout>=0)tflag=' -t '//f_int_to_char(timeout)
endif
cmd='bash -c "read'//nflag//sflag//tflag//'&& echo \$REPLY"'
call f_popen(cmd,pipe)
read(pipe,'(A)',iostat=istat)buffer
if(istat/=0)buffer=''
f_getch=trim(adjustl(buffer))
call f_pclose(pipe)
end function f_getch

subroutine f_getyesno(assertive)
logical, intent(out) :: assertive
do
    select case(f_getch())
    case('y', 'Y')
        assertive=.true.
        exit
    case('n', 'N')
        assertive=.false.
        exit
    case default
        call f_italic('Press either ', .true.)
        call f_green('y', .true.)
        call f_italic(' or ', .true.)
        call f_red('n')
        cycle
    end select
enddo
end subroutine f_getyesno

subroutine f_bold(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput bold && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput bold && echo "'//string//'" && tput sgr0')
end subroutine f_bold

subroutine f_outstanding(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput smso && echo -n "'//string//'" && tput rmso')
        return
    endif
endif
call execute_command_line('tput smso && echo "'//string//'" && tput rmso')
end subroutine f_outstanding

subroutine f_underline(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput smul && echo -n "'//string//'" && tput rmul')
        return
    endif
endif
call execute_command_line('tput smul && echo "'//string//'" && tput rmul')
end subroutine f_underline

subroutine f_italic(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput sitm && echo -n "'//string//'" && tput ritm')
        return
    endif
endif
call execute_command_line('tput sitm && echo "'//string//'" && tput ritm')
end subroutine f_italic

subroutine f_blink(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput blink && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput blink && echo "'//string//'" && tput sgr0')
end subroutine f_blink

subroutine f_red(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(red)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(red)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_red

subroutine f_green(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(green)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(green)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_green

subroutine f_yellow(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(yellow)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(yellow)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_yellow

subroutine f_blue(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(blue)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(blue)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_blue

subroutine f_magenta(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(magenta)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(magenta)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_magenta

subroutine f_cyan(string, noadvance)
character(len=*), intent(in) :: string
logical, intent(in), optional :: noadvance
if(present(noadvance))then
    if(noadvance)then
        call execute_command_line('tput setaf '//f_int_to_char(cyan)// &
                               ' && echo -n "'//string//'" && tput sgr0')
        return
    endif
endif
call execute_command_line('tput setaf '//f_int_to_char(cyan)// &
                                  ' && echo "'//string//'" && tput sgr0')
end subroutine f_cyan

integer function f_termheight()
integer :: pipe
call f_popen('tput lines',pipe)
read(pipe,*)f_termheight
call f_pclose(pipe)
end function f_termheight

integer function f_termwidth()
integer :: pipe
call f_popen('tput cols',pipe)
read(pipe,*)f_termwidth
call f_pclose(pipe)
end function f_termwidth

subroutine f_clrscr(nlines)
integer, intent(in), optional :: nlines
integer :: icount
if(present(nlines))then
    if(nlines<0)then
        do icount=1,-nlines
            call execute_command_line('tput cud1 && tput el')
        enddo
    elseif(nlines==0)then
        call execute_command_line('tput el && tput el1')
    else
        call execute_command_line('tput cuu '//f_int_to_char(nlines)//' && tput ed')
    endif
else
    call execute_command_line('tput clear')
endif
end subroutine f_clrscr

subroutine f_hideprompt()
call execute_command_line('tput civis')
end subroutine f_hideprompt

subroutine f_showprompt()
call execute_command_line('tput cnorm')
end subroutine f_showprompt

end module f_tty
