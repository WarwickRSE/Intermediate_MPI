#include <stdio.h>
#include <mpi.h>
#include <string.h>


//My struct. I only want to send a, b and c, NOT inter
//I could send inter too, but to demonstrate that I don't have to
//I won't here
typedef struct {
    int a;
    char inter;
    int b[5];
    float c[6];
} mystruct;

int main(int argc, char** argv)
{

  int rank, i, off;
  int lengths[3];
  MPI_Aint base;
  MPI_Aint displacements[3];
  MPI_Datatype types[3];
  MPI_Datatype struct_type;
  mystruct data;

  char buffer[400];

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //In C++ always have to be careful when not working with standard layout types
  //because there are fewer guarantees about how memory is layed out
  // See https://en.cppreference.com/w/cpp/named_req/StandardLayoutType
  // and https://en.cppreference.com/w/cpp/types/is_standard_layout
  MPI_Get_address(&data, &base);
  //Have to subtract the base address as well
  MPI_Get_address(&data.a, &displacements[0]);
  MPI_Get_address(&data.b, &displacements[1]);
  MPI_Get_address(&data.c, &displacements[2]);
  displacements[0] = MPI_Aint_diff(displacements[0], base);
  displacements[1] = MPI_Aint_diff(displacements[1], base);
  displacements[2] = MPI_Aint_diff(displacements[2], base);

  types[0] = MPI_INT; types[1] = MPI_INT; types[2] = MPI_FLOAT;
  //Lengths are in multiples of the associated type
  lengths[0] = 1; lengths[1] = 5; lengths[2] = 6;

  if (rank == 0) {
    printf("Byte offsets are %li %li %li\n", displacements[0], 
        displacements[1], displacements[2]);
  }

  //On rank 0 set some dummy values in my struct
  //Don't do this on other ranks so their version
  //Will be in a different and random state
  if (rank == 0){
    data.a = 100;
    for (i = 0; i<5; ++i){
      data.b[i] = i;
    }
    for (i = 0; i<6; ++i){
      data.c[i] = 100.0/(float)i;
    }
  }

  data.inter = rank;

  //Create and commit the type
  MPI_Type_create_struct(3, lengths, displacements, types, &struct_type);
  MPI_Type_commit(&struct_type);

  //Use MPI_Bcast to send the value from rank 0 to all other ranks
  MPI_Bcast(&data, 1, struct_type, 0, MPI_COMM_WORLD);

  //Output to screen. Write all output to a single string first and then print
  //to maximise the chance that it all gets printed in one chunk
  memset(buffer, 0, 400);
  off=0;
  off+=sprintf(buffer,"============\n");
  off+=sprintf(buffer+off,"rank: %i\n", rank);
  off+=sprintf(buffer+off,"a   : %i\n", data.a);
  off+=sprintf(buffer+off,"inter   : %i\n", data.inter);
  off+=sprintf(buffer+off,"NOTE THAT INTER IS DIFFERENT!\n");
  off+=sprintf(buffer+off,"b   : ");
  for (i = 0; i<5; ++i){
    off+=sprintf(buffer+off,"%i ", data.b[i]);
  }
  off+=sprintf(buffer+off,"\nc   :");
  for (i = 0; i<6; ++i){
    off+=sprintf(buffer+off,"%.6f ", data.c[i]);
  }
  off+=sprintf(buffer+off,"\n");
  printf("%s", buffer);

  //Free the type and Finalize MPI
  MPI_Type_free(&struct_type);
  MPI_Finalize();
}
