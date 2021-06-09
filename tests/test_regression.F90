module test_regression
  use mpi
  use mpi_variables
  use sum_func
  use funit
  implicit none
  double precision:: regression_tolerance=1e-5

contains
  !@test
  subroutine initialise()
    !implicit none

    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)


#line 19 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
  call assertEqual(root, 0 ,'Initialisation', &
 & location=SourceLocation( &
 & 'test_regression.pf', &
 & 19) )
  if (anyExceptions()) return
#line 20 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"

  end subroutine initialise
  !===========================================================================================



  !@test
  subroutine test_sum()
    implicit none

    integer answer
    integer:: nTerms=8*9*10


    call MPI_Barrier(  MPI_COMM_WORLD, ierror)



    answer = sumFunc( nTerms)


    call MPI_Barrier(  MPI_COMM_WORLD, ierror)


    if( process_Rank == 0 ) then
#line 45 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
  call assertEqual(answer, 10*nTerms + (nTerms+1)*nTerms/2 ,'Initialisation', &
 & location=SourceLocation( &
 & 'test_regression.pf', &
 & 45) )
  if (anyExceptions()) return
#line 46 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
    end if

  end subroutine test_sum




  !===========================================================================================
  !@test
  subroutine test_finalize()
    implicit none
    call MPI_FINALIZE(ierror)
  end subroutine test_finalize



end module test_regression

module Wraptest_regression
   use FUnit
   use test_regression
   implicit none
   private

contains


end module Wraptest_regression

function test_regression_suite() result(suite)
   use FUnit
   use test_regression
   use Wraptest_regression
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_regression_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('initialise', initialise))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_sum', test_sum))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_finalize', test_finalize))
   call suite%addTest(t)


end function test_regression_suite

