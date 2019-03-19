#include <stdio.h>
#include <mpi.h>

#define TAG 100
#define NELEMENTS 100000

int main(int argc, char ** argv)
{

  int rank, nproc, left, right, i;
  int send_val[NELEMENTS], recv_val[NELEMENTS];
  MPI_Request requests[2];
  MPI_Status statuses[2];


  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  for (i=0;i<NELEMENTS;++i){send_val[i]=rank;}

  MPI_Isend(send_val, NELEMENTS, MPI_INT, right, TAG, MPI_COMM_WORLD, 
      &requests[0]);
  for (i=0;i<NELEMENTS;++i){send_val[i]=-10;}
  MPI_Irecv(recv_val, NELEMENTS, MPI_INT, left, TAG, MPI_COMM_WORLD,
      &requests[1]);
  MPI_Waitall(2, requests, statuses);

  printf("Rank %3d got message from rank %3d of %3d\n", rank, left, 
      recv_val[0]);

  MPI_Finalize();

}
