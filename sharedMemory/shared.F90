program sharedmemtest
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
  use mpi
  implicit none
  integer, parameter :: dp = selected_real_kind(14,200)
  integer :: win,hostcomm,hostrank
  INTEGER(KIND=MPI_ADDRESS_KIND) :: windowsize
  INTEGER :: disp_unit,my_rank,ierr,total,i
  TYPE(C_PTR) :: baseptr
  real(dp), POINTER :: matrix_elementsy(:,:)
  integer,allocatable :: shapeArray(:)

  call MPI_INIT( ierr )

  call MPI_COMM_RANK(MPI_COMM_WORLD,MY_RANK,IERR)  !GET THE RANK OF ONE PROCESS                                                                                                                                                                                                
  call MPI_COMM_SIZE(MPI_COMM_WORLD,Total,IERR)  !GET THE TOTAL PROCESSES OF THE COMM                                                                                                                                                                                          
  CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, hostcomm,ierr)
  CALL MPI_Comm_rank(hostcomm, hostrank,ierr)

  ! Allocate array that specifies shape of matrix both procs will edit
  allocate(shapeArray(2))
  shapeArray=(/ 2,2 /)

  ! Specify size of window where shared array is located
  if (hostrank == 0) then ! switching if statement on doesn't change output
    windowsize = int(10**4,MPI_ADDRESS_KIND)*8_MPI_ADDRESS_KIND !*8 for double ! Put the actual data size here
  else
    windowsize = 0_MPI_ADDRESS_KIND ! Window will be shared between procs so only need to specify it's size on one
  end if

  ! Allocate memory in window made above to each process
  disp_unit = 1
  CALL MPI_Win_allocate_shared(windowsize, disp_unit, MPI_INFO_NULL, hostcomm, baseptr, win, ierr)    

  ! Obtain the location of the memory segment for the procs where
  ! windowsize=0
  if (hostrank /= 0) then
     CALL MPI_Win_shared_query(win, 0, windowsize, disp_unit, baseptr, ierr)     
  end if

  ! baseptr can now be associated with a Fortran pointer                                                                                                 
  ! and thus used to access the shared data
  CALL C_F_POINTER(baseptr, matrix_elementsy,shapeArray)

  !=============================sample code=============================

  ! Have each process fill in part of the shared array
  CALL MPI_WIN_FENCE(0, win, ierr) ! Wrap all operations on the window with this
                                   ! call. A process that has finished all such
                                   ! operations will be held back from accessing
                                   ! the part of the window covered by another
                                   ! process until the latter process has
                                   ! finished its work in the window too.
  matrix_elementsy=0.0_dp
  if (hostrank == 0) then ! Use hostrank not my_rank here as the former starts
                          ! from 0 for each subgroup so this is more general
                          ! (e.g. if there were four procs in two subgroups, 1,2
                          ! and 3,4, then using my_rank would mean the first row
                          ! would only be correct for the first subgroup)
     matrix_elementsy(1,1)=1.0_dp
     matrix_elementsy(1,2)=2.0_dp
  else
     matrix_elementsy(2,1)=3.0_dp
     matrix_elementsy(2,2)=4.0_dp
  end if
  CALL MPI_WIN_FENCE(0, win, ierr)
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)

  ! Print the full shared array as it appears on each process; it should be
  ! identical
  do i=1,Total
    if (my_rank == i-1) then
    print *, ' '
    print *, my_rank
    print *, matrix_elementsy(1,:)
    print *, matrix_elementsy(2,:)
    end if
    CALL MPI_WIN_FENCE(0, win, ierr)
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  end do

  !=============================end sample code=============================

  call MPI_BARRIER(MPI_COMM_WORLD,ierr) 
  call MPI_Win_free(win,ierr)     
  call MPI_FINALIZE(IERR)

  end program
