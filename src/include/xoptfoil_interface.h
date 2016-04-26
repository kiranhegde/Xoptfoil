#pragma once

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs( char *cinput_file, int *cerrval, 
                                     char *cerrmsg );
extern"C" void initialize( int *cerrval, char *cerrmsg );
extern"C" void cleanup ();
