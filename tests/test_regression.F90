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
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

#line 17 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
  call assertEqual(root, 0 ,'Initialisation', &
 & location=SourceLocation( &
 & 'test_regression.pf', &
 & 17) )
  if (anyExceptions()) return
#line 18 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
  end subroutine initialise
  !===========================================================================================



  !@test
  subroutine test_sum()
    integer answer,i
    integer:: nTerms=2*3*4 !!Divisible by 2,3,4,6,8,12,16, etc

    do i=1,10
       answer = sumFunc( nTerms)

       if( process_Rank == 0 ) then
#line 32 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
  call assertEqual(answer, 10*nTerms + (nTerms+1)*nTerms/2 ,'Initialisation', &
 & location=SourceLocation( &
 & 'test_regression.pf', &
 & 32) )
  if (anyExceptions()) return
#line 33 "/Users/pmzrsg/source/tryFortranMPI/tests/test_regression.pf"
       end if

       nTerms = nTerms * 2
    enddo


  end subroutine test_sum





  !===========================================================================================
  !@test
  subroutine test_finalize()
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

