use f_tty
implicit none
logical :: nod
character(len=256) :: username
call f_italic('Wanna get some characters without pressing Enter? y/n')
call f_getyesno(nod)
if(nod)then
call f_green('Type 3 letter word here: ', .true.)
call f_blue('You typed: '//f_getch(3, .true.))
call f_cyan('Great! Now press any key: ', noadvance=.false.)
call f_blue('You typed: '//f_getch())
call f_magenta('Awesome! Now enter your name wihin 10s: ')
call f_blue('Did you enjoy '//f_getch(-5, .false., 10)//'?')
call f_getyesno(nod)
if(nod)then
    call f_yellow('Thank you!')
else
    call f_yellow('Oops')
endif
endif
end