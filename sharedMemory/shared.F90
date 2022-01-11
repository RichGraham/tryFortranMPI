program sharedmemtest
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
  use mpi
  implicit none
  integer, parameter :: dp = selected_real_kind(14,200)
  integer :: win,hostcomm,hostrank
  INTEGER(KIND=MPI_ADDRESS_KIND) :: windowsize
  INTEGER :: disp_unit,my_rank,ierr,total,i,j
  TYPE(C_PTR) :: baseptr
  real(dp), POINTER :: matrix_elementsy(:,:)
  integer,allocatable :: shapeArray(:)

  ! Standard MPI set-up
  call MPI_INIT( ierr )
  call MPI_COMM_RANK(MPI_COMM_WORLD,MY_RANK,IERR)  !GET THE RANK OF ONE PROCESS
  call MPI_COMM_SIZE(MPI_COMM_WORLD,Total,IERR)  !GET THE TOTAL PROCESSES OF THE COMM

  ! Shared memory set-up
  CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, hostcomm,ierr)
  CALL MPI_Comm_rank(hostcomm, hostrank,ierr)

  ! Allocate array that specifies shape of matrix procs will edit
  allocate(shapeArray(2))
  shapeArray=(/ 4,2 /)

  ! Specify size of window where shared array is located
  if (hostrank == 0) then ! Window will be shared between procs so only need to specify it's size on root
    windowsize = int(10**4,MPI_ADDRESS_KIND)*8_MPI_ADDRESS_KIND !*8 for double ! Put the actual data size here
  else
    windowsize = 0_MPI_ADDRESS_KIND
  end if

  ! Allocate memory in window made above to each process
  disp_unit = 1
  CALL MPI_Win_allocate_shared(windowsize, disp_unit, MPI_INFO_NULL, hostcomm, baseptr, win, ierr)    

  ! Obtain the location of the shared memory segment for the procs where windowsize=0
  if (hostrank /= 0) then
     CALL MPI_Win_shared_query(win, 0, windowsize, disp_unit, baseptr, ierr)     
  end if

  ! baseptr can now be associated with a Fortran pointer                                                                                                 
  ! and thus used to access the shared data
  CALL C_F_POINTER(baseptr, matrix_elementsy,shapeArray)

  !=============================sample code=============================

  ! Have each process fill in part of the shared array.
  ! Use hostrank not my_rank to fill the array as the former starts
  ! from 0 for each group of procs that are on the same node.
  ! This means hostrank will work for cases where the procs are
  ! spread across various nodes. For example, if there were four
  ! procs (0,1,2,3) across two nodes, group one would be 0,1 and
  ! group two, 2,3. Using the proc ranks would mean that the array
  ! of group two would never be filled with zeros as there's no
  ! proc with rank 0 therein. However, proc 2 in this group is
  ! assigned hostrank 0 in its group when the shared mem is set up.
  if (hostrank == 0) then
     matrix_elementsy=0.0_dp
  endif
  CALL MPI_WIN_FENCE(0, win, ierr) ! MPI_win_fence is akin to a barrier call,
                                   ! only rather than preventing procs reading
                                   ! the script beyond the barrier, MPI_win_fence
                                   ! prevents one proc accessing the part of the
                                   ! shared window belonging to another proc until
                                   ! the latter has finished its work on its
                                   ! part of the shared window
  matrix_elementsy(hostrank+1,1)=(hostrank*2)+1
  matrix_elementsy(hostrank+1,2)=(hostrank*2)+2
  CALL MPI_WIN_FENCE(0, win, ierr)
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)

  ! Print the full shared array as it appears on each process; it should be
  ! identical
  do i=1,Total
    if (my_rank == i-1) then
      print *, ' '
      print *, my_rank
      do j=1,shapeArray(1)
        print *, matrix_elementsy(j,:)
      end do
    end if
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  end do

  !=============================end sample code=============================

  call MPI_Win_free(win,ierr)     
  call MPI_FINALIZE(IERR)

  end program
