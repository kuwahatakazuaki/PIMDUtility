program main
implicit none
integer :: i, j
real(8) :: pi = atan(1.0d0)*4.0d0

open(30,file="bond.out")
do i = 1, 100000
  write(30,*) i, sin(pi*dble(i)/10)  + 0.1 *sin(pi*dble(i)/1000)
end do
close(30)



end program main

