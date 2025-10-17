module m_cline
  interface cline
     module procedure cline_logical
     module procedure cline_integer
     module procedure cline_string
  end interface
  private :: writeline
contains
   subroutine writeline(lgreen,fixedstring,fixedcstring)
   use m_ansi_colors
   implicit none
   character(len=*), intent(in) :: fixedstring
   character(len=*), intent(in) :: fixedcstring
   logical, intent(in) :: lgreen

   character(len=20) :: fmt
   integer :: pos

   write(*,'(tr4,a1,tr3,a50,a3)',advance='no') '#',fixedstring,' : '
   if (lgreen) then
       write(*,'(3a)',advance='no')'<'//color(trim(fixedcstring),c_green )//'>'
   else
       write(*,'(3a)',advance='no')'<'//color(trim(fixedcstring),c_yellow)//'>'
   endif
   pos = 22 - len_trim(fixedcstring)
   write(fmt,'("(T",I0,",A10)")') pos
   write(*,fmt) '#        #'
end subroutine

subroutine cline_logical(logi,string)
   implicit none
   logical,          intent(in) :: logi
   character(len=*), intent(in) :: string
   character(len=50) :: fixedstring
   character(len=20) :: fixedcstring
   logical :: lgreen

   lgreen=.false.
   if (logi) lgreen=.true.

   ! convert logical to string
   fixedcstring = ' '                       ! clear
   write(fixedcstring,'(l1)') logi

   ! left-pad: write trimmed string into s50, trailing blanks remain
   fixedstring = ' '                       ! clear
   fixedstring = adjustl(trim(string))     ! left adjust, trailing blanks remain

   call writeline(lgreen,fixedstring,fixedcstring)

end subroutine

subroutine cline_integer(n,string)
   use m_ansi_colors
   implicit none
   integer,       intent(inout) :: n
   character(len=*), intent(in) :: string
   character(len=20)  :: fixedcstring
   character(len=50) :: fixedstring
   logical :: lgreen

   if (n < 0) then
      n=abs(n)
      print *,color('    >>>negative number replaced with its absolute value',c_red)
   endif

   lgreen=.false.
   if (n > 0) lgreen=.true.

   ! left-pad: write trimmed string into s50, trailing blanks remain
   fixedstring = ' '                       ! clear
   fixedstring = adjustl(trim(string))     ! left adjust, trailing blanks remain

   fixedcstring = ' '                       ! clear
   write(fixedcstring,'(i4)')n
   fixedcstring=adjustl(trim(fixedcstring))

   call writeline(lgreen,fixedstring,fixedcstring)

end subroutine

subroutine cline_string(cstring,string)
   implicit none
   character(len=*), intent(in) :: string
   character(len=*), intent(in) :: cstring
   character(len=50) :: fixedstring
   character(len=20) :: fixedcstring
   logical :: lgreen
   lgreen=.false.

   if (trim(fixedcstring) /= 'default') lgreen=.true.

   ! left-pad: write trimmed string into fixedstring, trailing blanks remain
   fixedstring = ' '                        ! clear
   fixedstring = adjustl(trim(string))      ! left adjust, trailing blanks remain

   fixedcstring = ' '                       ! clear
   fixedcstring = adjustl(trim(cstring))    ! left adjust, trailing blanks remain

   call writeline(lgreen,fixedstring,fixedcstring)

end subroutine
end module
