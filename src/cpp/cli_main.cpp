#include <iostream>

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs(int input_file_len, char *input_file);

/******************************************************************************/
//
// Main program
//
/******************************************************************************/
int main ( int argc, char *argv[] )
{
  char input_file[] = "inputs.txt";

  read_namelist_inputs(sizeof(input_file), input_file);

  return 0;
}
