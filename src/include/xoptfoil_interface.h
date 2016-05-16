#pragma once

/******************************************************************************/
//
// Declarations for Fortran interface functions
//
/******************************************************************************/
extern"C" void read_namelist_inputs( char *, int *, char *, char *, char *, 
                                     char *, char *, char *, int *, int *, 
                                     bool *, int *, int [], char *, int *, 
                                     char *);

extern"C" void initialize( char *, char *, char *, int *, int *, char *, int *,
                           char *);

extern"C" void iterate ( int *, char *);

extern"C" void cleanup ();
