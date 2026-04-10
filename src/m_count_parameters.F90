module m_count_parameters

contains

subroutine count_parameters(fname,parnr,parnrtime,pardt,ndim)
    implicit none
    character(len=*), intent(in) :: fname
    integer,         intent(out) :: parnr
    integer,         intent(out) :: parnrtime
    integer,         intent(out) :: pardt
    integer,         intent(out) :: ndim
    logical ex
    integer iunit
    character(len=1) ch

    parnr=0
    inquire(file=trim(fname), exist=ex)
    if (ex) then
       open(newunit=iunit, file=trim(fname), status='old', action='read')
       read(iunit,*)parnrtime
       read(iunit,*)pardt
       do
          read(iunit,'(a1)',end=999,err=999)ch
          if (ch == '#') parnr= parnr + 1
       enddo
       999 close(iunit)
       print '(a,i0)','The number of parameters types is:         ',parnr
       print '(a,i0)','The number of parameter values in time is: ',parnrtime
       print '(a,i0)','The deta t between parameter values in is: ',pardt
       ndim=parnr*parnrtime
       print '(a,i0)','The total number of parameter values is:   ',ndim
    else
       print '(3a)', 'The file: ',trim(fname),' does not exist'
    endif

end subroutine
end module
