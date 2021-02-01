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

module f_tty

implicit none
private

!~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
public :: f_getch, f_bold, f_outstanding, f_clrscr
!~~~~~~~~~~~~~~~~~~~~~~~~~~~END CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contains

character function f_getch(echo) !Works for bash only
logical, optional :: echo
if(present(echo))then
    if(echo)call execute_command_line('read -n 1 f_getch_private')
else
    call execute_command_line('read -sn 1 f_getch_private')
endif
call get_environment_variable('f_getch_private',f_getch)
end function f_getch

subroutine f_bold(string)
character(len=*) :: string
call execute_command_line('tput bold && echo '//string//' && tput sgr0')
end subroutine f_bold

subroutine f_outstanding(string)
character(len=*) :: string
call execute_command_line('tput smso && echo '//string//' && tput rmso')
end subroutine f_outstanding

subroutine f_clrscr(height)
integer, optional :: height
character(len=4) :: nlines
if(present(height))then
    write(nlines,'(I0)')height
    if(height<0)then
        call execute_command_line('tput clear')
    else
        call execute_command_line('tput cuu '//trim(nlines)//' && tput ed')
    endif
else
    call execute_command_line('tput reset')
endif
end subroutine f_clrscr

end module f_tty
