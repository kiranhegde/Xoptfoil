#pragma once

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs( char *cinput_file, int len_input,
                                     int *errval, char *cerrmsg );
