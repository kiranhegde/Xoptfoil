#pragma once

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs( 
                    char *input_file, char *search_type, char *global_search,
                    char *local_search, char *seed_airfoil, char *airfoil_file,
                    char *naca_digits, int *nfunctions_top, int *nfunctions_bot,
                    bool *restart, int *restart_write_freq, int *errval, 
                    char *cerrmsg );

extern"C" void initialize( 
                      char *seed_airfoil, char *airfoil_file, char *naca_digits,
                      int *nfunctions_top, int *nfunctions_bot, int *errval, 
                      char *cerrmsg );

extern"C" void optimizer_setup ( int *errval, char *errmsg );

extern"C" void iterate ( int *errval, char *errmsg );

extern"C" void cleanup ();
