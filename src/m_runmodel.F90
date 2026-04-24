module m_runmodel
contains
subroutine runmodel()
   use mod_dimensions
   use m_readinfile, only : experiment,runcommand,iwin
   !use m_parameters, only : parnrtime
   use m_read_uvw
   use m_input_files_def, only : measurment_locations_file
   implicit none
   integer :: iens=0
   character(len=4) ciens
   logical exd
   character(len=100) directory
   character(len=100) fname
   character(len=512) :: cmd

! setting up and running the model
   write(ciens,'(i4.4)')iens
   directory=trim(experiment)//'/mem'//ciens
   print '(a)', 'mkdir '//trim(directory)
   call system('mkdir -p '//trim(directory))
   inquire(file=trim(directory),exist=exd)
   if (.not.exd) then
      print *,'runmodel: Problem with exd; run directory does not exist:',trim(directory)
      stop
   endif

   cmd = 'cp '//trim(measurment_locations_file)//' '//trim(directory)//'/measurement_loc.in'
   call system(trim(cmd))
   call system('cp '//trim(experiment)//'/uvel_time.ref '//trim(directory)//'/uvel_time.dat')
   call system('cp infile.in '//trim(directory)//'/infile.in')
   call system('rm -f '//trim(directory)//'/uvw0?????.uf')
   call system('cd '//trim(directory)//'; '//trim(runcommand))

   fname=trim(experiment)//'/tecstate_0.dat'
   if (iwin == 1) then
      open(10,file=trim(fname))
         write(10,'(a)')'TITLE = "State variables"'
         write(10,'(a)')'VARIABLES = "time", "vel", "dir"'
         !write(10,'(a,i0,a)')'ZONE T="Reference", I=',parnrtime,', DATAPACKING=POINT'
         write(10,'(a)')'ZONE T="Reference", I=XXX, DATAPACKING=POINT'
      close(10)
   endif
   write(cmd,'(a,a)')'cat '//trim(experiment)//'/uvel_time.ref >> ',trim(fname)
   call system(trim(cmd))
end subroutine
end module
