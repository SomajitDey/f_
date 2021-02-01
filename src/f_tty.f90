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
! This module may not work on shells other than bash. Also some terminals that do
! not support tput will not be compatible.
!************************************************************************

! REFERENCE:
! Command: read --help
! For tput: http://linuxcommand.org/lc3_adv_tput.php
!************************************************************************

module f_tty

implicit none
private

integer, parameter :: red=1, green=2, yellow=3, blue=4, magenta=5, cyan=6

!~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Input single character without pressing Enter
public :: f_getch

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

character function f_getch(echo) !Works for bash only
logical, intent(in), optional :: echo
if(present(echo))then
    if(echo)call execute_command_line('read -n 1 f_getch_private')
else
    call execute_command_line('read -sn 1 f_getch_private')
endif
call get_environment_variable('f_getch_private',f_getch)
end function f_getch

subroutine f_bold(string,noadvance)
character(len=*), intent(in) :: string
call execute_command_line('tput bold && echo '//string//' && tput sgr0')
end subroutine f_bold

subroutine f_outstanding(string)
character(len=*), intent(in) :: string
call execute_command_line('tput smso && echo '//string//' && tput rmso')
end subroutine f_outstanding

subroutine f_underline(string)
character(len=*), intent(in) :: string
call execute_command_line('tput smul && echo '//string//' && tput rmul')
end subroutine f_underline

subroutine f_italic(string)
character(len=*), intent(in) :: string
call execute_command_line('tput sitm && echo '//string//' && tput ritm')
end subroutine f_italic

subroutine f_blink(string)
character(len=*), intent(in) :: string
call execute_command_line('tput blink && echo '//string//' && tput sgr0')
end subroutine f_blink

subroutine f_clrscr(nlines)
integer, intent(in), optional :: nlines
character(len=4) :: nrows
if(present(nlines))then
    write(nrows,'(I0)')nlines
    if(nlines<0)then
        call execute_command_line('tput reset')
    else
        call execute_command_line('tput cuu '//trim(nrows)//' && tput ed')
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
