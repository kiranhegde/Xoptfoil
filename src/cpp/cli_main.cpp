#include <iostream>
#include <string.h>
#include "xoptfoil_interface.h"

/******************************************************************************/
//
// Prints error message (char array) to stdout
//
/******************************************************************************/
void print_error ( char *array, int len )
{
  int i;

  std::cout << "Error: ";
  for ( i = 0; i < len-1; i++ ) { std::cout << array[i]; }
  std::cout << array[len-1] << std::endl;
}

/******************************************************************************/
//
// Main program
//
/******************************************************************************/
int main ( int argc, char *argv[] )
{
  char input_file[80], errmsg[80];
  int errval, i;

  // Print program info

  std::cout << std::endl;
  std::cout << " This is Xoptfoil: airfoil optimization with Xfoil" 
            << std::endl;
  std::cout << " Version 2.0" << std::endl;

  // Initialize char arrays

  for ( i = 0; i < 80; i++ ) 
  { 
    input_file[i] = ' ';
    errmsg[i] = ' ';
  }

  // Get input file name from command line input

  if (argc < 2) { strcpy(input_file, "inputs.txt"); }
  else { strcpy(input_file, argv[1]); }

  // Read namelist inputs

  read_namelist_inputs(input_file, &errval, errmsg);
  if (errval != 0) { print_error(errmsg, 80); return 1; }

  // Initialize

  initialize(&errval, errmsg);
  if (errval != 0) { print_error(errmsg, 80); return 1; }

  // Deallocate memory

  cleanup();

  return 0;
}
