

! A worker-controller model for finding all primes within a given interval.
!Each worker checked primality of a single number
!  This is optimised a bit - it tests only the numbers in small_primes first, then from max_small_prime it tests every odd number, returning at the first factor it finds
!The master dispatches the numbers to test.

!In the Non-blocking version, the master also does some cheap work to trim the list of candidates using a sieve method. This significantly reduces the number of tests the workers have to do

!THIS CODE IS NOT OPTIMAL - it is an example. For instance, we should ignore even numbers from the start, and do not.

MODULE prime_helpers

  IMPLICIT NONE

  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15), INT8 = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: ISPRIME = 2, ISCOMP = 1, ISUNCHECKED = 0

  INTEGER(KIND=INT64), PARAMETER :: small_primes_len = 20, max_small_prime = 71
  INTEGER(KIND=INT64), DIMENSION(20), PARAMETER :: small_primes = &
    (/2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71/)

  CONTAINS

  FUNCTION check_prime(num)

    INTEGER(KIND=INT64) :: num, index, end_ind
    INTEGER(KIND=INT8) :: check_prime

    end_ind = CEILING(SQRT(REAL(num, KIND(1.0d0))))

    check_prime = ISPRIME

    !First check against the small primes
    DO index = 1, small_primes_len
      IF (small_primes(index) > end_ind) RETURN
      IF (MOD(num,small_primes(index)) == 0) THEN
        check_prime = ISCOMP
        RETURN
      END IF
    END DO
    
    !Test higher numbers, skipping all the evens
    DO index = max_small_prime+2, end_ind, 2
      IF (MOD(num,index) == 0) THEN
        check_prime = ISCOMP
        RETURN
      END IF
    END DO

    RETURN

  END FUNCTION check_prime

END MODULE prime_helpers


MODULE controller

  USE mpi
  USE prime_helpers
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE dispatcher(lower, upper, len, flag_array)


    INTEGER(KIND=INT64) :: lower, upper, len
    INTEGER(KIND=INT8), DIMENSION(:) :: flag_array

    INTEGER :: nproc, worker_num, inflight, ierr

    INTEGER, DIMENSION(MPI_Status_size) :: stat
    INTEGER :: request

    INTEGER(KIND=INT64) next_package_start, next_precheck_index
    LOGICAL skip_precheck

    INTEGER(KIND=INT64), PARAMETER :: stop_precheck = 11

    INTEGER(KIND=INT8) :: result

    INTEGER(KIND=INT64) :: offset, dummy_dat
    INTEGER(KIND=INT64), DIMENSION(:), ALLOCATABLE :: current_packages, packages_processed

    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: start_times, end_times, cum_times

    CHARACTER :: inp

    CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)

    ! Allocate all the arrays per worker ...
    
    ! Keep track of what each worker is working on
    ALLOCATE(current_packages(nproc))
    current_packages = 0

    !These track how long each package takes, and cumulative totals per worker
    ALLOCATE(start_times(nproc), end_times(nproc), cum_times(nproc))
    cum_times = 0
    ALLOCATE(packages_processed(nproc))
    packages_processed = 0

    ! Index of next lowest number to check. When this hits len we are done
    next_package_start = 1
    inflight = 0

    !Master has its own work to do
    next_precheck_index = 1
    skip_precheck = .FALSE.

    DO WHILE(.TRUE.)

      !Recieve and unpack the results
      
      !Tag is used for operational info - is this a ready/done signal, or a result?
      
      !Wait to receive any data. On the first pass tag will be 0 which is
      !Just the workers saying that they are ready

!      CALL MPI_Recv(result, 1, MPI_INTEGER1, MPI_ANY_SOURCE, MPI_ANY_TAG, &
!          MPI_COMM_WORLD, stat, ierr)

      CALL MPI_Irecv(result, 1, MPI_INTEGER1, MPI_ANY_SOURCE, MPI_ANY_TAG, &
          MPI_COMM_WORLD, request, ierr)

      IF(.NOT. skip_precheck) THEN
        CALL precheck_flags(flag_array, len, lower, &
          small_primes(next_precheck_index))
        next_precheck_index = next_precheck_index + 1
        IF(next_precheck_index > stop_precheck) skip_precheck = .TRUE.
      END IF

      CALL MPI_Wait(request, stat, ierr)

      !We have to get this information from the status variable because
      !we received the message with MPI_ANY_SOURCE and MPI_ANY_TAG
      IF (stat(MPI_TAG) > 0) THEN
        !Capture end time and add to this workers stats
        end_times(stat(MPI_SOURCE)) = MPI_Wtime()
        cum_times(stat(MPI_SOURCE)) = cum_times(stat(MPI_SOURCE)) + &
            (end_times(stat(MPI_SOURCE)) - start_times(stat(MPI_SOURCE)))
        packages_processed(stat(MPI_SOURCE)) = &
            packages_processed(stat(MPI_SOURCE)) + 1
        !Package recieved and understood
        inflight = inflight - 1

        !Now set the correct flag for this package
        offset = current_packages(stat(MPI_SOURCE)) - lower + 1
        flag_array(offset) = result
      END IF

      !If you've still got work to give out then find the next candidate and dispatch
      IF (next_package_start <= len) THEN

        !Now find the next candidate and send it to the worker that just reported
        !that it had finished.
        !The tag is "1" indicating this is real work

        DO WHILE(.TRUE.)
          !Skip over numbers already tested
          IF( flag_array(next_package_start) == ISUNCHECKED) EXIT
          next_package_start = next_package_start + 1
          IF(next_package_start > len) EXIT
        END DO

        current_packages(stat(MPI_SOURCE)) = lower + next_package_start - 1
        !PRINT*, "Dispatching ", lower+next_package_start - 1
        next_package_start = next_package_start + 1

        inflight = inflight + 1
        !Record the start time
        start_times(stat(MPI_SOURCE)) = MPI_Wtime()
        !Send the work package with the tag of the work index to the source
        !of the previously received message

        CALL MPI_Send(current_packages(stat(MPI_SOURCE)), 1, MPI_INTEGER8, &
            stat(MPI_SOURCE), 1, MPI_COMM_WORLD, ierr)
      ELSE
        !No more work to do so shut down the worker. Here, this is just
        !Sending a message with a zero tag
        dummy_dat = -1
        CALL MPI_Send(dummy_dat, 1, MPI_INTEGER8, stat(MPI_SOURCE), 0, &
            MPI_COMM_WORLD, ierr)
      END IF

      IF(inflight == 0) EXIT

    END DO

    DO worker_num = 1, nproc-1
      PRINT*, "Worker ", worker_num, " checked ", &
          packages_processed(worker_num), "candidates in", &
          cum_times(worker_num)
    END DO

  END SUBROUTINE

  SUBROUTINE precheck_flags(flags, len, lower, stride)

    !Knock out all the multiples of stride in flags

    INTEGER(KIND=INT8), DIMENSION(:) :: flags
    INTEGER(KIND=INT64), INTENT(IN) :: len, lower, stride
    INTEGER(KIND=INT64) :: st, i

    st = stride - MOD(lower, stride) + 1

    DO i = st, len, stride
      flags(i) = ISCOMP
    END DO

  END SUBROUTINE

END MODULE controller

MODULE worker

  USE mpi
  USE prime_helpers
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE worker_fn

    INTEGER, DIMENSION(MPI_Status_size) :: stat
    INTEGER :: ierr
    INTEGER(KIND=INT64) :: package
    INTEGER(KIND=INT8) :: dat, result

    !First just call home to say "I'm here and I'm waiting"
    !So send -1 message and 0 tag to rank 0
    dat = -1
    CALL MPI_Send(dat, 1, MPI_INTEGER1, 0, 0, MPI_COMM_WORLD, ierr)

    DO WHILE(.TRUE.)
      !Receive the work package from rank 0
      CALL MPI_Recv(package, 1, MPI_INTEGER8, 0, MPI_ANY_TAG, &
          MPI_COMM_WORLD, stat, ierr)
      !Need to have a termination condition, here tag = 0
      IF (stat(MPI_TAG) == 0) RETURN

      !Do the work
      result = check_prime(package)
      !Send the result back using the same tag
      CALL MPI_Send(result, 1, MPI_INTEGER1, 0 , stat(MPI_TAG), MPI_COMM_WORLD, ierr)

    END DO

  END SUBROUTINE

END MODULE worker


PROGRAM wc

  USE mpi
  USE worker
  USE controller

  IMPLICIT NONE

  INTEGER :: rank, nproc, i, total, ierr
  INTEGER(KIND=INT64) :: lower_bound, upper_bound, len

  INTEGER(KIND=INT8), DIMENSION(:), ALLOCATABLE :: flags

  CHARACTER(len=32) :: arg


  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  IF (nproc == 1) THEN
    PRINT*, "This example requires more than 1 processor"
    CALL MPI_Abort(MPI_COMM_WORLD, 0, ierr)
  END IF

  IF (COMMAND_ARGUMENT_COUNT() /= 2) THEN
    PRINT*, "Please supply the ends of range to check"
    CALL MPI_Abort(MPI_COMM_WORLD, 0, ierr)
  END IF

  CALL get_command_argument(1, arg)
  READ(arg, "(i32)") lower_bound
  CALL get_command_argument(2, arg)
  READ(arg, "(i32)") upper_bound

  len = upper_bound - lower_bound + 1

  IF (len <= 0) THEN
    PRINT*, "Lower bound must be < upper bound"
    CALL MPI_Abort(MPI_COMM_WORLD, 0, ierr)
  END IF

  !Array of flags corresponding to numbers to test
  ! Flag is ISUNCHECKED for unchecked, ISCOMP for composite and ISPRIME for prime
  !See prime_helpers for these constants definitions
  ALLOCATE(flags(len))
  flags = ISUNCHECKED


!  IF(rank == 0) PRINT*, "Is prime ", check_prime(32452843_INT64)
!  IF(rank == 0) PRINT*, "Is prime ", check_prime(32452842_INT64)
!   CALL MPI_Abort(MPI_COMM_WORLD, 0, ierr)

  IF (rank == 0) THEN

    CALL dispatcher(lower_bound, upper_bound, len, flags)
    total = 0

    DO i = 1, len
      IF(flags(i) == ISPRIME) THEN
        !PRINT*, lower_bound+i-1
        total = total + 1
      END IF
    END DO
    PRINT*, "Found ", total, "primes"

  ELSE
    CALL worker_fn()
  END IF


  CALL MPI_Finalize(ierr)
  
END PROGRAM