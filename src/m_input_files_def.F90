module m_input_files_def
   character(len=100) :: model_run_command_file='dasys_model_run_command.tmp'
   character(len=100) :: model_ensemble_size_file='dasys_ensemble_size.tmp'
   character(len=100) :: experiment_name_file='dasys_experiment_name.tmp'
   character(len=100) :: parameter_file='dasys_uncertain_parameters.tmp'
   character(len=100) :: measurment_locations_file='measurement_loc.in'
   character(len=100) :: infile_ens='infile.X'
   character(len=100) :: infile_ref='infile.ref'
end module
