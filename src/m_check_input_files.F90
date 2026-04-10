module m_check_input_files
contains
subroutine check_input_files()
   use mod_dimensions
   use m_input_files_def
   implicit none
   logical ex
   character(len=100) fname

   fname(:)=' '
   fname=trim(parameter_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(a)')' 10                   ! Number of parameter values in time'
         write(10,'(a)')' 60                   ! Delta t between parameter values (s)'
         write(10,'(a)')'#vel  8.0  1.0 20 60'
         write(10,'(a)')'#dir  0.0 10.0 20 60'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for uncertain vel and dir'
      print '(3a)','Please edit and restart dasys'
      stop
   endif

   fname(:)=' '
   fname=trim(measurment_locations_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(3(i4,tr2),a)') nx/2, ny/2, nz/2, 'u'
         write(10,'(3(i4,tr2),a)') nx/2, ny/2, nz/2, 'v'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for measurement locations'
      print '(3a)','Please edit and restart dasys'
      stop
   endif

end subroutine
end module
