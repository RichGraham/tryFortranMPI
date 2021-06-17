module sum_func
  use mpi
  use mpi_variables
  implicit none

contains

  
  integer function sumFunc(global_size)


    integer, intent(in):: global_size
    integer:: array_size,i
    integer:: addToEachElement = 10
    integer:: sum=0

    integer, dimension(global_size) :: global      ! only root has this
    integer, dimension(global_size) :: results      ! only root has this
    integer, allocatable,dimension (:):: local       ! everyone has this
    

    !print *, 'Hello World from process (function): ', process_Rank, 'of ', size_Of_Cluster
    
    array_size = global_size /  size_Of_Cluster
    allocate( local( array_size ))

    if (process_Rank == root) then
       global = [ (i, i=1,global_size) ]
       !print *,'Total elements:',global_size
       !print *,'Number of processes:',size_Of_Cluster
       !print *,'Elements per process:',array_size
    endif

    call MPI_Scatter(global, array_size, MPI_INTEGER, &    ! send everyone 2 ints from global
         local,  array_size, MPI_INTEGER, &    ! each proc recieves 2 into
         root,                   &    ! sending process is root,
         MPI_COMM_WORLD, ierror)        ! all procs in COMM_WORLD participate
    
  !print *,local
  do i=1, array_size
     local(i) = local(i) + addToEachElement
  end do
  !print *,local
  
  call MPI_Gather(local, array_size, MPI_INTEGER, &    ! send everyone 2 ints from global
       results,  array_size, MPI_INTEGER, &    ! each proc recieves 2 into
       root,                   &    ! sending process is root,
       MPI_COMM_WORLD, ierror) 


  
  if (process_Rank == root) then
     !print*,results
     sum=0
     do i=1, global_size
        sum = sum + results(i)
     enddo
     sumFunc = sum
  endif
    
  end function sumFunc


end module sum_func
