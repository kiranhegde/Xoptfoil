#include <iostream>
#include <string.h>
#include "xoptfoil_interface.h"

/******************************************************************************/
//
// Main program
//
/******************************************************************************/
int main ( int argc, char *argv[] )
{
  char input_file[80], errmsg[80];
  int errval;

  // Get input file name from command line input

  if (argc < 2) { strcpy(input_file, "inputs.txt"); }
  else { strcpy(input_file, argv[1]); }

  // Read namelist inputs

  read_namelist_inputs(input_file, sizeof(input_file), &errval, errmsg);
  if (errval != 0) { std::cout << "Error: " << errmsg << std::endl; }

  return 0;
}
