module m_runensemble
contains
subroutine runensemble(A,nrens,params,ndim,parnr,parnrtime,pardt,it)
   use mod_dimensions
   use m_readinfile, only : experiment, runcommand, itdirs
   use m_read_uvw
   use m_params
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   integer, intent(in) :: parnr
   integer, intent(in) :: parnrtime
   integer, intent(in) :: pardt
   real, target, intent(in) :: A(ndim,nrens)
   real, pointer :: A3(:,:,:)
   type(uncertain_parameters), intent(in) :: params(ndim)
   integer, intent(in) :: it

   character(len=4) ciens
   character(len=2) cit

   logical exd,exf
   integer iens,i

   character(len=100) :: directory
   character(len=512) :: cmd

   if (ndim /= parnrtime*parnr) error stop "runensemble: ndim /= parnrtime*parnr"
   A3(1:parnrtime, 1:parnr, 1:nrens) => A

   do iens=1,nrens
      write(ciens,'(i4.4)')iens
      write(cit,'(i2.2)')it

! Generating directory for running realization
      if (itdirs) then
         directory=trim(experiment)//'/mem'//ciens//'/it'//cit
      else
         directory=trim(experiment)//'/mem'//ciens
      endif
      call system('mkdir -p '//trim(directory))
      inquire(file=trim(directory),exist=exd)

! Copy infile.in to simulation directory
      inquire(file='infile.in',exist=exf)
      cmd = 'cp infile.in '//trim(directory)//'/infile.in'
      call system(trim(cmd))

! Generating the parameter file from ensemble file and copy to run directory
      open(10,file='uvel_time.tmp')
         do i=1,parnrtime
            write(10,'(100f13.5)')real(i-1)*pardt,A3(i,1:parnr,iens)
         enddo
      close(10)
      call system('cp uvel_time.tmp '//trim(directory)//'/uvel_time.dat')

! Copy the measurement_loc.in to run directory
      cmd = 'cp measurement_loc.in '//trim(directory)//'/measurement_loc.in'
      call system(trim(cmd))

      if ((it > 1).and.(itdirs)) then
         cmd = 'cp '//trim(directory)//'/../it01/seed_0000.dat '//trim(directory)
         call system(trim(cmd))
         cmd = 'cp '//trim(directory)//'/../it01/seed_0000.orig '//trim(directory)
         call system(trim(cmd))
      endif

      write(*,'(a,i0,tr1,a)')'iteration ',it,trim(directory)
      cmd = 'cd '//trim(directory)//' > /dev/null ; '//trim(runcommand)//' > run.out'
      call system(trim(cmd))

   enddo

   print *,'runens: ensemble simulation done!'
   print *

end subroutine
end module
