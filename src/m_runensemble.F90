module m_runensemble
contains
subroutine runensemble(nrens,params,ndim,it,ensemble)
   use mod_dimensions
   use m_readinfile, only : experiment, runcommand
   use m_read_uvw
   use m_params
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   real, intent(in) :: ensemble(ndim,nrens)
   type(uncertain_parameters), intent(in) :: params(ndim)
   integer, intent(in) :: it

   character(len=4) ciens
   character(len=2) cit

   logical exd,exf
   integer iens,i

   character(len=100) :: directory
   character(len=512) :: cmd
   character(len=12)  :: cparvalue


   do iens=1,nrens
      print *,'iens=',iens
      write(ciens,'(i4.4)')iens
      write(cit,'(i2.2)')it

      directory=trim(experiment)//'/mem'//ciens//'/it'//cit
      call system('mkdir -p '//trim(directory))

      inquire(file=trim(directory),exist=exd)
      inquire(file='infile.X',exist=exf)
      if (exd .and. exf) then
         call system('cp infile.X infile.tmp')
         do i=1,ndim
            write(cparvalue,'(f12.6)')ensemble(i,iens)
            cmd = 'sed -i ''s/#'//trim(params(i)%parname)//'/'//trim(cparvalue)//'/'' infile.tmp'
            print *,'exe:',trim(cmd)
            call system(trim(cmd))
         enddo
         cmd = 'cp infile.tmp '//trim(directory)//'/infile.in'
         call system(trim(cmd))
         cmd = 'cp measurement_loc.in '//trim(directory)//'/measurement_loc.in'
         call system(trim(cmd))
         cmd = 'cd '//trim(directory)//'; '//trim(runcommand)//' > run.out'
         call system(trim(cmd))
         if (it > 1) then
            cmd = 'cp '//trim(directory)//'/../it01/seed.dat '//trim(directory)
            call system(trim(cmd))
            cmd = 'cp '//trim(directory)//'/../it01/seed.orig '//trim(directory)
            call system(trim(cmd))
         endif
      else
         print *,'runens: Problem with exd or exf'
      endif
   enddo
   print *,'runens: ensemble simulation done!'
   print *

end subroutine
end module
