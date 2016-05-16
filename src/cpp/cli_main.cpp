#include <iostream>
#include <string.h>
#include "constants.h"
#include "xoptfoil_interface.h"

/******************************************************************************/
//
// Prints error message (char array) to stdout
//
/******************************************************************************/
void print_error ( char *array, int len )
{
  int i;

  std::cout << " Error: ";
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
  char input_file[80], airfoil_file[80], errmsg[80], seed_airfoil[10]; 
  char search_type[16], global_search[17], local_search[7], naca_digits[4];
  int nfunctions_top, nfunctions_bot, restart_write_freq, errval, i;
  int flap_flag[MAX_OP_POINTS];
  bool restart;

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

  read_namelist_inputs(input_file, search_type, global_search, local_search,
                       seed_airfoil, airfoil_file, naca_digits, &nfunctions_top,
                       &nfunctions_bot, &restart, &restart_write_freq,
                       flap_flag, &errval, errmsg);
  if (errval != 0) { print_error(errmsg, 80); return 1; }

  // Initialize

  initialize(seed_airfoil, airfoil_file, naca_digits, &nfunctions_top, 
             &nfunctions_bot, &errval, errmsg);
  if (errval != 0) { print_error(errmsg, 80); return 1; }

  // Deallocate memory

  cleanup();

  return 0;
}
