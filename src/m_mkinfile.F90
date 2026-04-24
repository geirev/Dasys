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
      write(10,'(a)')' 1000 5000 1000             ! Measurement times: first last dtobs (steps)'
      write(10,'(a)')' 5000                       ! Sampling: Assimilation window length (steps)'
      write(10,'(a)')' 1                          ! Assimilation window number'
   close(10)
end subroutine
end module
