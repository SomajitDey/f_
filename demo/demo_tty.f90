use f_
implicit none
write(*,*)f_termheight(), f_termwidth()
call f_red('I am red')
call f_green('I am green')
call f_yellow('I am yellow')
call f_blue('I am blue')
call f_magenta('I am magenta')
call f_cyan('I am cyan')
call f_hideprompt()
call f_underline('Press any key')
call f_keypress()
call f_clrscr(1)
call f_italic('Thank you')
call f_bold('The key you pressed is:')
call f_blink('I dont know!!')
call f_outstanding('Press any key to clear screen')
call f_showprompt()
call f_keypress()
call f_clrscr(11)
end
