!@Somajit Dey <somajit@users.sourceforge.net> January 2021
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
! This module contains wrappers for some user-land C system calls. This 
! module is meant to be used for making your Fortran code portable. Some
! compilers (e.g. gfortran, ifort) provide their own (i.e. non-standard) 
! extensions for these system calls.
!
! These module procedures and interfaces basically bind to functions in 
! the glibc library that ships with GNU/Linux.
!************************************************************************

! SIGSEGV RUNTIME ERROR:
! Any such error possibly arises from type mismatch when porting from 32-bit to 
! 64-bit. E.g. equating INT32 with C_LONG variable type. C_LONG can take both 32-bit
! and 64-bit form depending on the implementation.
!************************************************************************

module f_syscall

use iso_c_binding
use iso_fortran_env, only: int64

implicit none
private

!~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Signal
public :: f_signal, f_kill, f_alarm

!Sleep
public :: f_nanosleep, f_sleep

!Process and Host
public :: f_getpid, f_getcwd, f_gethostname

!Exit with given exitcode
public :: f_exit

!Directory
public :: f_mkdir, f_rmdir, f_chdir

!File or Path (some apply to directory as well, everything in linux is a file)
public :: f_rename, f_link, f_symlink, f_unlink, f_chmod

!Number of seconds since the Epoch, 1970-01-01 00:00:00 +0000 (UTC)
public :: f_time
!~~~~~~~~~~~~~~~~~~~~~~~~~~~END CONTENTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type, bind(c) :: timespec
    integer(c_long) :: tv_sec, tv_nsec
end type

abstract interface
   subroutine handler(signum)
   integer :: signum
   end subroutine handler
end interface

interface
   function f_getpid() bind(c, name='getpid')
   import :: c_int
   integer(c_int) :: f_getpid
   end function f_getpid
end interface   

type handler_pointer
    procedure(handler), pointer, nopass :: ptr
end type handler_pointer
integer, parameter :: no_of_signals=64  !Obtained with command: kill -l
type(handler_pointer) :: handler_ptr_array(no_of_signals)

contains

!Drop the 2nd arg below to install a do-nothing handler. Note that any sleep or 
!idle-wait would be interrupted though when the signal signum is caught.
!Use f_exit as the handler to abort on signal signum with exitcode=signum
subroutine f_signal(signum, handler_routine)
integer, intent(in) :: signum
procedure(handler), optional:: handler_routine
type(c_funptr) :: iret
type(c_funptr) :: c_handler

interface
   function c_signal(signal, sighandler) bind(c, name='signal')
   import :: c_int, c_funptr 
   integer(c_int), value, intent(in) :: signal
   type(c_funptr), value, intent(in) :: sighandler
   type(c_funptr) :: c_signal
   end function c_signal
end interface

if(present(handler_routine))then
    handler_ptr_array(signum)%ptr => handler_routine
else
    handler_ptr_array(signum)%ptr => null(handler_ptr_array(signum)%ptr)
endif
c_handler=c_funloc(f_handler)
iret=c_signal(signum, c_handler)
end subroutine f_signal

subroutine f_handler(signum) bind(c)
integer(c_int), intent(in), value :: signum
if(associated(handler_ptr_array(signum)%ptr))call handler_ptr_array(signum)%ptr(signum)
end subroutine f_handler

subroutine f_kill(pid, signal, exitstat)
integer, intent(in) :: pid, signal
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_kill(pid, sig) bind(c, name='kill') result(ret)
   import :: c_int
   integer(c_int), value :: pid, sig
   integer(c_int) :: ret
   end function c_kill
end interface   

iret=c_kill(pid, signal)
if(present(exitstat))exitstat=iret
end subroutine f_kill

subroutine f_alarm(seconds, remaining)
integer, intent(in) :: seconds
integer, intent(out), optional :: remaining
integer :: iret

interface
   function c_alarm(sec) bind(c, name='alarm') result(ret)
   import :: c_int
   integer(c_int), value :: sec
   integer(c_int) :: ret
   end function c_alarm
end interface

iret=c_alarm(seconds)
if(present(remaining))remaining=iret
end subroutine f_alarm

subroutine f_nanosleep(s, ns, rem_s, rem_ns)
integer, intent(in) :: s, ns
integer, intent(out) :: rem_s, rem_ns
type(timespec) :: f_req, f_rem
interface
   subroutine c_nanosleep(req, rem) bind(c, name='nanosleep')
   import :: timespec
   type(timespec) :: req, rem   
   end subroutine c_nanosleep   
end interface

f_req%tv_sec=int(s, kind=c_long)
f_req%tv_nsec=int(ns, kind=c_long)
call c_nanosleep(f_req, f_rem)
rem_s=int(f_rem%tv_sec, kind(rem_s))
rem_ns=int(f_rem%tv_nsec, kind(rem_ns))
end subroutine f_nanosleep

subroutine f_sleep(seconds, remaining)
integer, intent(in) :: seconds
integer, intent(out), optional :: remaining
integer :: iret
interface
   function c_sleep(sec) bind(c, name='sleep') result(ret)
   import :: c_int
   integer(c_int), value :: sec
   integer(c_int) :: ret
   end function c_sleep
end interface   

iret=c_sleep(seconds)
if(present(remaining))remaining=iret
end subroutine f_sleep

subroutine f_getcwd(str)
character(len=*) :: str
integer :: endChar
interface
   subroutine c_getcwd(buff, n) bind(c, name='getcwd')
   import :: c_char, c_size_t
   character(c_char), intent(out) :: buff(*)
   integer(c_size_t), value, intent(in) :: n
   end subroutine c_getcwd
end interface

str=repeat(' ',len(str))
call c_getcwd(str, len(str, kind=c_size_t))
endChar=len_trim(str)
str(endChar:endChar)=' ' !Because C returns a null-terminated string, we remove c_null_char with a blank character
end subroutine f_getcwd

subroutine f_gethostname(str)
character(len=*) :: str
integer :: endChar
interface
   subroutine c_gethostname(buff, n) bind(c, name='gethostname')
   import :: c_char, c_size_t
   character(c_char), intent(out) :: buff(*)
   integer(c_size_t), value, intent(in) :: n
   end subroutine c_gethostname
end interface

str=repeat(' ',len(str))
call c_gethostname(str, len(str, kind=c_size_t))
endChar=len_trim(str)
str(endChar:endChar)=' ' !Because C returns a null-terminated string, we remove c_null_char with a blank character
end subroutine f_gethostname

subroutine f_exit(exitstat) !f_exit must be of type procedure(handler)
integer :: exitstat
interface
    subroutine c_exit(exitstat) bind(c, name='exit')
    import :: c_int
    integer(c_int),value::exitstat
    end subroutine c_exit
end interface
call c_exit(exitstat)
end subroutine f_exit

subroutine f_mkdir(dirname, mode, exitstat)
character(len=*), intent(in) :: dirname, mode
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_mkdir(dirname, mode) bind(c, name='mkdir') result(ret)
   import :: c_char, c_int, c_long 
   character(c_char), dimension(*), intent(in) :: dirname
   integer(c_long), value :: mode
   integer(c_int) :: ret
   end function c_mkdir
   
   function c_strtol(string, nullchar, base) bind(c, name='strtol') result(ret)
   import :: c_long, c_char, c_int
   character(c_char), dimension(*) :: string, nullchar
   integer(c_int), value :: base
   integer(c_long) :: ret
   end function c_strtol
end interface
character(len=len_trim(dirname)+1) :: c_dirname
character(len=len_trim(mode)+1) :: c_string
integer(c_long) :: c_mode
character(len=2)::nullchar

nullchar='0'//c_null_char
c_string=trim(mode)//c_null_char
c_mode=c_strtol(c_string, nullchar, 8_c_int)
c_dirname=trim(dirname)//c_null_char
iret=c_mkdir(c_dirname, c_mode)
if(present(exitstat))exitstat=iret
end subroutine f_mkdir

subroutine f_rmdir(dirname, exitstat)
character(len=*), intent(in) :: dirname
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_rmdir(dirname) bind(c, name='rmdir') result(ret)
   import :: c_char, c_int 
   character(c_char), dimension(*), intent(in) :: dirname
   integer(c_int) :: ret
   end function c_rmdir
end interface
character(len=len_trim(dirname)+1) :: c_dirname

c_dirname=trim(dirname)//c_null_char
iret=c_rmdir(c_dirname)
if(present(exitstat))exitstat=iret
end subroutine f_rmdir

subroutine f_chdir(dirname, exitstat)
character(len=*), intent(in) :: dirname
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_chdir(dirname) bind(c, name='chdir') result(ret)
   import :: c_char, c_int 
   character(c_char), dimension(*), intent(in) :: dirname
   integer(c_int) :: ret
   end function c_chdir
end interface
character(len=len_trim(dirname)+1) :: c_dirname

c_dirname=trim(dirname)//c_null_char
iret=c_chdir(c_dirname)
if(present(exitstat))exitstat=iret
end subroutine f_chdir

subroutine f_rename(oldname, newname, exitstat)
character(len=*), intent(in) :: oldname, newname
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_rename(oldf, newf) bind(c, name='rename') result(ret)
   import :: c_char, c_int 
   character(c_char), dimension(*), intent(in) :: oldf, newf
   integer(c_int) :: ret
   end function c_rename
end interface
character(len=len_trim(oldname)+1) :: c_oldname
character(len=len_trim(newname)+1) :: c_newname

c_oldname=trim(oldname)//c_null_char
c_newname=trim(newname)//c_null_char
iret=c_rename(c_oldname, c_newname)
if(present(exitstat))exitstat=iret
end subroutine f_rename

subroutine f_link(path, link, exitstat)
character(len=*), intent(in) :: path, link
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_link(path1, path2) bind(c, name='link') result(ret)
   import :: c_char, c_int 
   character(c_char), dimension(*), intent(in) :: path1, path2
   integer(c_int) :: ret
   end function c_link
end interface
character(len=len_trim(path)+1) :: c_path1
character(len=len_trim(link)+1) :: c_path2

c_path1=trim(path)//c_null_char
c_path2=trim(link)//c_null_char
iret=c_link(c_path1, c_path2)
if(present(exitstat))exitstat=iret
end subroutine f_link

subroutine f_symlink(path, link, exitstat)
character(len=*), intent(in) :: path, link
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_symlink(path1, path2) bind(c, name='symlink') result(ret)
   import :: c_char, c_int 
   character(c_char), dimension(*), intent(in) :: path1, path2
   integer(c_int) :: ret
   end function c_symlink
end interface
character(len=len_trim(path)+1) :: c_path1
character(len=len_trim(link)+1) :: c_path2

c_path1=trim(path)//c_null_char
c_path2=trim(link)//c_null_char
iret=c_symlink(c_path1, c_path2)
if(present(exitstat))exitstat=iret
end subroutine f_symlink

subroutine f_unlink(path, exitstat)
character(len=*), intent(in) :: path
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_unlink(pathname) bind(c, name='unlink') result(ret)
   import :: c_char, c_int
   character(c_char), dimension(*) :: pathname
   integer(c_int) :: ret
   end function c_unlink
end interface
character(len=len_trim(path)+1) :: c_path

c_path=trim(path)//c_null_char
iret=c_unlink(c_path)
if(present(exitstat))exitstat=iret
end subroutine f_unlink

subroutine f_chmod(filename, mode, exitstat)
character(len=*), intent(in) :: filename, mode
integer, intent(out), optional :: exitstat
integer :: iret
interface
   function c_chmod(filename, mode) bind(c, name='chmod') result(ret)
   import :: c_char, c_int, c_long 
   character(c_char), dimension(*), intent(in) :: filename
   integer(c_long), value :: mode
   integer(c_int) :: ret
   end function c_chmod
   
   function c_strtol(string, nullchar, base) bind(c, name='strtol') result(ret)
   import :: c_long, c_char, c_int
   character(c_char), dimension(*) :: string, nullchar
   integer(c_int), value :: base
   integer(c_long) :: ret
   end function c_strtol
end interface
character(len=len_trim(filename)+1) :: c_filename
character(len=len_trim(mode)+1) :: c_string
integer(c_long) :: c_mode
character(len=2)::nullchar

nullchar='0'//c_null_char
c_string=trim(mode)//c_null_char
c_mode=c_strtol(c_string, nullchar, 8_c_int)
c_filename=trim(filename)//c_null_char
iret=c_chmod(c_filename, c_mode)
if(present(exitstat))exitstat=iret
end subroutine f_chmod

function f_time(long)
integer :: f_time
integer(int64), optional :: long
integer(c_long) ::f_time_c_long 
interface
   subroutine c_time(tloc) bind(c, name='time')
   import :: c_long
   integer(c_long) :: tloc
   end subroutine c_time
end interface
call c_time(f_time_c_long)
f_time=int(f_time_c_long, kind(f_time))
if(present(long))long=int(f_time_c_long, kind(long))
end function f_time

end module f_syscall
