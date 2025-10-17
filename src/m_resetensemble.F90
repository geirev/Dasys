module m_resetensemble
contains
subroutine resetensemble(nrens,it)
   use m_localdefs
   use m_params
   implicit none
   integer, intent(in)  :: nrens
   integer, intent(in)  :: it

   character(len=4) ciens
   character(len=2) cit
   character(len=100) cmd

   integer iens

   character(len=100) outfile

   do iens=1,nrens
      write(ciens,'(i4.4)')iens
      write(cit,'(i2.2)')it
      outfile=trim(experiment)//'/mem'//ciens//'/it'//cit//'/run.out'
      cmd='rm -f '//trim(outfile)
      call system(cmd)
   enddo

end subroutine
end module

