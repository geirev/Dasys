module m_count_measurements
contains
integer function count_measurements()
   use m_input_files_def
   use mod_dimensions
   implicit none
   logical ex
   integer iunit,m,i

   inquire(file=trim(measurment_locations_file),exist=ex)
   if (.not.ex) then
      open(10,file=trim(measurment_locations_file),status='new')
         write(10,'(3(i4,tr2),a)') nx/2, ny/2, nz/2, 'u'
         write(10,'(3(i4,tr2),a)') nx/2, ny/2, nz/2, 'v'
      close(10)
      print '(3a)','Created default: "',trim(measurment_locations_file),'" for measurement locations'
      print '(3a)','Please edit and restart dasys'
      stop
   endif

   open(newunit=iunit, file=trim(measurment_locations_file), status='old', action='read')
      m=0
      do
         read(iunit,*,end=100)i
         m=m+1
      enddo
      100 count_measurements=m
   close(iunit)
end function
end module

