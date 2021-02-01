use f_
implicit none
write(*,*)f_termheight(), f_termwidth()
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
call f_clrscr(5)
end
