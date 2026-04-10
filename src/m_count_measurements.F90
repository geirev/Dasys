module m_count_measurements

contains

integer function count_measurements(fname)
   implicit none
   character(len=*), intent(in) :: fname
   logical ex
   integer iunit,m,i

   inquire(file=trim(fname), exist=ex)
   if (ex) then
      open(newunit=iunit, file=trim(fname), status='old', action='read')
         m=0
         do
            read(iunit,*,end=100)i
            m=m+1
         enddo
         100 count_measurements=m
      close(iunit)
   else
      print '(3a)', 'The file: ',trim(fname),' does not exist'
   endif
end function
end module

