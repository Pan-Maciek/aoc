
program two

  implicit none
  integer(8), dimension(0:8) :: fish_count
  integer, dimension(300) :: fish ! TODO: read from file
  integer :: rc, i
  integer(8) :: day, born_today, sum

  open(3, file="input.in", status="old", iostat=rc)
  if (rc /= 0) stop 'Error: open failed'

  fish_count = 0
  sum = 0

  read(3, *) fish ! TODO: read from file

  do i = 1, size(fish)
    fish_count(fish(i)) = fish_count(fish(i)) + 1
  end do

  do i = 0, 255
    day = modulo(i, 7)
    born_today = fish_count(day)
    fish_count(day) = fish_count(day) + fish_count(7)
    fish_count(7) = fish_count(8)
    fish_count(8) = born_today
  end do

  do i = 0, 8
    sum = sum + fish_count(i)
  end do

  write(*,*) sum


end program two