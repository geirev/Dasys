module m_mkinfile
contains
subroutine mkinfile(fname)
   character(len=*), intent(in) :: fname
   open(unit=10, file=trim(fname), status='unknown', action='write')
      write(10,'(a)')' exp_name                   ! Experiment name'
      write(10,'(a)')' boltzmann                  ! Run command'
      write(10,'(a)')' 100                        ! Ensemble size'
      write(10,'(a)')' 4                          ! ESMDA steps'
      write(10,'(a)')' 0.01                       ! relative observation error'
   close(10)
end subroutine
end module
