module mod_measurements
type measurement
   character(len=1) c
   integer i
   integer j
   integer k
   real d
end type
type(measurement), allocatable, public :: obs(:)
real,              allocatable, public :: yensemble(:,:)
end module
