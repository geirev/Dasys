module m_check_input_files
contains
subroutine check_input_files()
   use mod_dimensions
   use m_input_files_def
   implicit none
   logical ex
   character(len=100) fname

   fname(:)=' '
   fname=trim(model_ensemble_size_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(a)')'100'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for 100 realizations'
   endif

   fname(:)=' '
   fname=trim(model_run_command_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(a)')'boltzmann'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for boltzmann run command'
   endif

   fname(:)=' '
   fname=trim(experiment_name_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(a)')'EX_tmp'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for EX_tmp case'
   endif

   fname(:)=' '
   fname=trim(parameter_file)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      open(10,file=trim(fname),status='new')
         write(10,'(a)')'#vel  8.0  1.0'
         write(10,'(a)')'#dir  0.0 10.0'
      close(10)
      print '(3a)','Created default: "',trim(fname),'" for uncertain vel and dir'
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
   endif

   fname(:)=' '
   fname=trim(infile_ens)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      print '(a)','You must create an infile.X with uncertain parameters, corresponding to dasys_uncertain_parameters.tmp e.g.:'
      print '(a)','#vel #dir        ! uini, udir    : Inflow wind velocity [m/s], direction in degrees (-45:45)'
   endif

   fname(:)=' '
   fname=trim(infile_ref)
   inquire(file=trim(fname),exist=ex)
   if (.not.ex) then
      print '(a)','You must create an infile.ref for the reference simulaton:'
   endif

end subroutine
end module
