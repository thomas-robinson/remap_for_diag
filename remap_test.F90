module test_rankremap_mod

  interface update_x
    module procedure :: update_x_2d
    module procedure :: update_x_3d
  end interface

 contains
   subroutine update_x_2d (x, y, pow)
      REAL,  dimension(:,:), intent(in), target, contiguous :: y
      REAL,  dimension(:,:), intent(inout),target, contiguous :: x
      integer, intent(in) :: pow
!! Moved from your original main program
   real, pointer, dimension(:,:,:) :: ax_3d => null()          ! but ptrs to data will be 3D, with
   real, pointer, dimension(:,:,:) :: ay_3d => null()          !   the 3rd dim of size one.
   ax_3d(1:size(x,1), 1:size(x,2), 1:1 ) => x
   ay_3d(1:size(y,1), 1:size(y,2), 1:1 ) => y
   call update_x(ax_3d, ay_3d, 2) !!Update with 3D array ptrs  
!!
   end subroutine update_x_2d
   !!Update the 3D array x with x=x +y**pow
   !! Note test will pass in pointers with underlying data of 2D.
   subroutine update_x_3d (x, y, pow)
      REAL,  dimension(:,:,:), intent(in) :: y
      REAL,  dimension(:,:,:), intent(inout) :: x
      integer, intent(in) :: pow
      x = power_add (x,y,pow)
   end subroutine update_x_3d

   !! raise y by power, add it to x and return sum
   elemental function power_add(x,y,power)
      real,intent(in) :: x,y
      integer, intent(in) :: power
      real power_add
      power_add = x + y ** power
   end function power_add

end module test_rankremap_mod


!! This progrtam is one test of a function with a dummy
!! argument of rank 3 can be called with pointers to array of
!! rank 3 that were initialized with an actual data of rank 2.

program test_rankremap
use test_rankremap_mod
   integer :: NX, NY
   real, allocatable, target, dimension(:,:) :: ax_2d, ay_2d  !underlying data wil be 2D


   NX = 4
   NY = 7
   allocate(ax_2d( NX, NY))
   allocate(ay_2d( NX, NY))

   ax_2d = 1
   call init_y (ay_2d, NX, NY)

   !! *** This is the critial mapping ***


   call update_x(ax_2d, ay_2d, 2) !!Update with actual args 2D array .
   call check_x (ax_2d, ay_2d, NX, NY, 2) !!Check resultant 2D arrays

CONTAINS

   !! Set y elems to some unique value.
   subroutine init_y (y, NX, NY)
      real, dimension(:,:), intent(inout) :: y
      integer , intent(in) :: NX, NY
      integer i,j
      do j = 1 , NY
         do i = 1, NX
            y(i,j) = (j-1) * NX + i
         end do
      end do
   end subroutine init_y

   !!Verify 2D array X is what it should be.
   subroutine check_x ( x , y, NX , NY, pow)
      real, dimension(:,:), intent(in) :: x, y !!Passed as 2D array
      integer , intent(in) :: NX, NY
      integer, intent(in) :: pow
      integer :: fail_count = 0
      integer i,j
      do j = 1 , NY
         do i = 1, NX
            !!Formula below is same as power_add, and assuming
            !! x_initial(i) == 1
            if( x(i,j) /= 1 + (y(i,j) ** pow)) then
               fail_count = fail_count + 1
            endif
         end do
      end do
      if(fail_count > 0)then
         print *, " *** FAILURE *** fail_count=", fail_count
      else
         print *, "OK in check_x"
      endif
   end subroutine check_x
end program test_rankremap
