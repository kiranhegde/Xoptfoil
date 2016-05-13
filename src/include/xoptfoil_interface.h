#pragma once

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs( char *cinput_file, int *nfunctions_top,
                                     int *nfunctions_bot, int *errval, 
                                     char *cerrmsg );
extern"C" void initialize( int *nfunctions_top, int *nfunctions_bot,
                           int *errval, char *cerrmsg );
extern"C" void optimizer_setup ( int *errval, char *errmsg );
extern"C" void iterate ( int *errval, char *errmsg );
extern"C" void cleanup ();
