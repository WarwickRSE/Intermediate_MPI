#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char** argv)
{

  int rank, recv_rank, nproc, left, right;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  MPI_Sendrecv(&rank, 1, MPI_INT, right, TAG, &recv_rank, 1, MPI_INT,
      left, TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  printf("Rank %3d got message from rank %3d of %3d\n", rank, left, recv_rank);

  MPI_Finalize();

}
