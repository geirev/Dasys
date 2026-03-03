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
         write(10,'(a)')'#vel  8.0  1.0'
         write(10,'(a)')'#dir  0.0 10.0'
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
         write(10,'(2(i4,tr2),a)') nx/2, ny/2, 'u'
         write(10,'(2(i4,tr2),a)') nx/2, ny/2, 'v'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for measurement locations'
      print '(3a)','Please edit and restart dasys'
      stop
   endif

   fname(:)=' '
   fname=trim(infile_ens)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      print '(3a)','You must create an ',trim(infile_ens),' with uncertain parameters, e.g.:'
      print '(a)','#vel #dir        ! uini, udir    : Inflow wind velocity [m/s], direction in degrees (-45:45)'
      stop
   endif

   fname(:)=' '
   fname=trim(infile_ref)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      print '(3a)','You must create an ',trim(infile_ref),' for the reference simulaton:'
      stop
   endif

end subroutine
end module
