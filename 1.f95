program explicit
implicit none
real :: lambda, k, rho, c , alpha , dx, dt ,time ,x

integer , parameter  :: m = 10 !No of Nodes in x-Direction
integer , parameter  :: l = 5 !No of Nodes in Time -direction
real , dimension (0:m,0:l) :: T

integer :: i ,n 
 
k = 54
rho = 7800
c = 490
alpha = k/ (rho*c)

print *,"Enter the length of the rod "
read *, x 
print *,"Enter the time in sec need to calculate the Temperature "
read*,  time

dx = x/m
dt = time/l
lambda = 1/((dx*dx)/(alpha*dt))

T(0,0:l) = 100
T(1:m-1,0) = 20
T(m,0:l) = 25
 

do n = 0,l-1
    do i = 1,m-1
      T(i,n+1) = T(i,n) + (lambda* (T(i+1,n) - (2*T(i,n)) + T(i-1,n)))
    
 end do
end do

 
open (12,file = 'myoutput.txt')
do n = 0,l
  do i = 0,m
   write (12,*) T(i,n)
    end do
 end do

 print*, "finished"
do n = 0,l
  do i = 0,m
   print*,  T(i,n)
    end do
end do



end program explicit

  