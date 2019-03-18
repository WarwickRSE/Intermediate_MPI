PROGRAM type

  USE mpi
  IMPLICIT NONE

  !Only want to send a,b and c, not inter. Could send all of them
  !but demonstrate that you don't have to
  TYPE mytype
    !THIS IS VERY IMPORTANT! The Fortran standard allows reordering of elements
    !in types unless you either specify SEQUENCE or BIND(C) attributes
    SEQUENCE
    INTEGER :: a
    CHARACTER(LEN=1) :: inter
    INTEGER, DIMENSION(5) :: b
    REAL(SELECTED_REAL_KIND(6, 37)), DIMENSION(6) :: c
  END TYPE mytype

!This is the alternative using BIND(C)
!  TYPE, BIND(C) :: mytype
!    INTEGER :: a
!    CHARACTER(LEN=1) :: inter
!    INTEGER, DIMENSION(5) :: b
!    REAL(SELECTED_REAL_KIND(6, 37)), DIMENSION(6) :: c
!  END TYPE mytype

  INTEGER :: rank
  INTEGER :: ierr, i
  INTEGER(MPI_ADDRESS_KIND) :: base
  INTEGER(MPI_ADDRESS_KIND), DIMENSION(3) :: displacements
  INTEGER, DIMENSION(3) :: lengths, types
  INTEGER :: struct_type
  TYPE(mytype) :: data

  CALL MPI_Init(ierr)

  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Get_address(data, base, ierr)
  CALL MPI_Get_address(data%a, displacements(1), ierr)
  CALL MPI_Get_address(data%b, displacements(2), ierr)
  CALL MPI_Get_address(data%c, displacements(3), ierr)
  !MPI_Get_address gets the absolute address from MPI_BOTTOM so you have to
  !subtract the location of the base of your type from the displacements of
  !the elements
  displacements(1) = MPI_Aint_diff(displacements(1), base)
  displacements(2) = MPI_Aint_diff(displacements(2), base)
  displacements(3) = MPI_Aint_diff(displacements(3), base)

  IF (rank == 0) THEN
    PRINT "(A, I4, I4, I4)", "Byte offsets are ", displacements
  END IF

  types = [MPI_INTEGER, MPI_INTEGER, MPI_REAL]
  !Lengths are in multiples of the associated type
  lengths = [1, 5, 6]
  
  !Create the type
  CALL MPI_Type_create_struct(3, lengths, displacements, types, struct_type, &
      ierr)
  !Register the type
  CALL MPI_Type_commit(struct_type, ierr)

  !On rank 0 set some dummy values in my type
  !Don't do this on other ranks so their version
  !Will be in a different and random state
  IF (rank == 0) THEN
    data%a = 100
    DO i = 1, 5
      data%b(i) = i
    END DO
    DO i = 1, 6
      data%c(i) = 100.0/REAL(i, SELECTED_REAL_KIND(6, 37))
    END DO
  END IF

  !Create a character starting at A and increasing for each rank. Cycles at 26
  data%inter = ACHAR(ICHAR('A') + MOD(rank,26))

  IF (rank == 0) THEN
    PRINT *, 'To read the output from this example, open the files fort.1**'
    PRINT *, 'The output from rank 0 will be in fort.100'
    PRINT *, 'The output from rank 1 will be in fort.101 etc.'
  END IF

  CALL MPI_Bcast(data, 1, struct_type, 0, MPI_COMM_WORLD, ierr)

  WRITE(100+rank,*) 'A     : ', data%a
  WRITE(100+rank,*) 'Inter : ', data%inter
  WRITE(100+rank,*) 'NOTE THAT INTER IS DIFFERENT!'
  WRITE(100+rank,*) 'B     : ', data%b
  WRITE(100+rank,*) 'C     : ', data%c

  CALL MPI_Type_free(struct_type, ierr)
  CALL MPI_Finalize(ierr)

END PROGRAM type
