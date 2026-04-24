module m_runensemble
contains
subroutine runensemble(A,nrens,ndim,it)
   use mod_dimensions
   use m_readinfile, only : experiment, runcommand, iwin, nmda
   use m_read_uvw
   use m_parameters, only : parnr,parnrtime,pardt
   use m_input_files_def, only : measurment_locations_file
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   real, target, intent(in) :: A(ndim,nrens)
   real, pointer :: A3(:,:,:)
   integer, intent(in) :: it

   real time
   character(len=4) ciens

   integer iens,i

   character(len=100) :: fname
   character(len=100) :: directory
   character(len=512) :: cmd

   A3(0:parnrtime, 1:parnr, 1:nrens) => A

   do iens=1,nrens
      write(ciens,'(i4.4)')iens

! Generating directory for running realization
      directory=trim(experiment)//'/mem'//ciens
      call system('mkdir -p '//trim(directory))

! Copy infile.in to simulation directory
      cmd = 'cp infile.in '//trim(directory)//'/infile.in'
      call system(trim(cmd))

! Generating the parameter file from ensemble file and write to run directory
      open(10,file=trim(directory)//'/uvel_time.dat')
         do i=0,parnrtime
            time = real((iwin-1)*parnrtime + i) * real(pardt)
            write(10,'(100f13.5)')time,A3(i,1:parnr,iens)
         enddo
      close(10)

      if (it == 1) then
         fname=trim(directory)//'/tecstate_1.dat'
         if (iwin == 1) then
            open(10,file=trim(fname))
               write(10,'(a)')'TITLE = "Prior state variables"'
               write(10,'(a)')'VARIABLES = "time", "vel", "dir"'
               write(10,'(a)')'ZONE T="Prior", I=XXX, DATAPACKING=POINT'
            close(10)
         endif
         write(cmd,'(a,a)')'cat '//trim(directory)//'/uvel_time.dat >> ',trim(fname)
         call system(trim(cmd))
      elseif (it == nmda+1) then
         fname=trim(directory)//'/tecstate_2.dat'
         if (iwin == 1) then
            open(10,file=trim(fname))
               write(10,'(a)')'TITLE = "Posterior state variables"'
               write(10,'(a)')'VARIABLES = "time", "vel", "dir"'
               write(10,'(a)')'ZONE T="Posterior", I=XXX, DATAPACKING=POINT'
            close(10)
         endif
         write(cmd,'(a,a)')'cat '//trim(directory)//'/uvel_time.dat >> ',trim(fname)
         call system(trim(cmd))
      endif


! Copy the measurement_loc.in to run directory
      cmd = 'cp '//trim(measurment_locations_file)//' '//trim(directory)//'/measurement_loc.in'
      call system(trim(cmd))

      write(*,'(a,i0,tr1,a)')'iteration ',it,trim(directory)
      cmd = 'cd '//trim(directory)//' > /dev/null ; '//trim(runcommand)//' > run.out'
      call system(trim(cmd))

   enddo

   print *,'runens: ensemble simulation done!'
   print *

end subroutine
end module
