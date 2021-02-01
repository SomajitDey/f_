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
! This module contains some portable utility routines in modern fortran.
! Generic interfaces may often be required to accomodate for the different
! types a procedure argument might have. E.g. crossproduct may take both
! real(4) and real(8) arguments.
!************************************************************************

! SIGSEGV RUNTIME ERROR:
! Any such error possibly arises from type mismatch when porting from 32-bit to 
! 64-bit. E.g. equating INT32 with C_LONG variable type. C_LONG can take both 32-bit
! and 64-bit form depending on the implementation.
!************************************************************************

module f_utils

implicit none
private

!~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Integer-Character interconversion
public :: f_int_to_char, f_char_to_int
!~~~~~~~~~~~~~~~~~~~~~~~~~~~END CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

contains

pure function int_to_string(num)
integer, intent(in) :: num
character(len=256) :: int_to_string
write(int_to_string,'(I0)')num
end function int_to_string

function f_int_to_char(num)
integer, intent(in) :: num
character(len_trim(int_to_string(num))) :: f_int_to_char
f_int_to_char=trim(int_to_string(num))
end function f_int_to_char

function f_char_to_int(string)
character(len=*), intent(in) :: string
integer :: f_char_to_int
read(string,*)f_char_to_int
end function f_char_to_int

end module f_utils
