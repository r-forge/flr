/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     adalloc.h
 Revision: $Id: adalloc.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: Allocation of arrays of doubles in several dimensions 
 
 Copyright (c) 2003
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
 
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20040423 kowarz: adapted to configure - make - make install
          20000310 olvo:   removed superflous semicola
          19990622 olvo:   myfree routines & special identity 
                           allocations (2n-1-vectors) 
                           (MOSTLY INLINED)
          19981130 olvo:   newly created.
 
----------------------------------------------------------------------------*/
#if !defined (ADOLC_ADALLOC_H)
#define ADOLC_ADALLOC_H 1

#include "common.h"

/****************************************************************************/
/*                                                         Now the C THINGS */
BEGIN_C_DECLS

/*--------------------------------------------------------------------------*/
/*                                              MEMORY MANAGEMENT UTILITIES */
ADOLC_DLL_EXPORT double    *myalloc1(int);
ADOLC_DLL_EXPORT double   **myalloc2(int, int);
ADOLC_DLL_EXPORT double  ***myalloc3(int, int, int);

ADOLC_DLL_EXPORT void myfree1(double   *);
ADOLC_DLL_EXPORT void myfree2(double  **);
ADOLC_DLL_EXPORT void myfree3(double ***);

/*--------------------------------------------------------------------------*/
/*                                          SPECIAL IDENTITY REPRESENTATION */
ADOLC_DLL_EXPORT double   **myallocI2(int);
ADOLC_DLL_EXPORT void myfreeI2(int, double**);

END_C_DECLS

/****************************************************************************/
/*                                                       Now the C++ THINGS */
#if defined(__cplusplus)

/*--------------------------------------------------------------------------*/
/*                                              MEMORY MANAGEMENT UTILITIES */
inline double   * myalloc(int n) {
    return myalloc1(n);
}
inline double  ** myalloc(int m, int n) {
    return myalloc2(m,n);
}
inline double *** myalloc(int m, int n, int p) {
    return myalloc3(m,n,p);
}

inline void myfree(double   *A) {
    myfree1(A);
}
inline void myfree(double  **A) {
    myfree2(A);
}
inline void myfree(double ***A) {
    myfree3(A);
}

#endif

/****************************************************************************/
#endif
