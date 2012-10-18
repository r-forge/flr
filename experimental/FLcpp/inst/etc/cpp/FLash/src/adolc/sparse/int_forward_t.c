/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     int_forward_t.c
 Revision: $Id: int_forward_t.c 134 2009-03-03 14:25:24Z imosqueira $
 Contents: int_forward_tigh
           ( first-order-vector forward mode for bit patterns,
             tight version = basepoint check,  more precize )
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.  
----------------------------------------------------------------------------*/

#define _INT_FOR_TIGHT_ 1
#include "int_for.c"
#undef _INT_FOR_TIGHT_
