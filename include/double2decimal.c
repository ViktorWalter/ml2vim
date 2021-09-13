/* double2decimal.c    
 *
 *  Copyright (C) 1998 Timo Felbinger 
 *  email: Timo.Felbinger@quantum.physik.uni-potsdam.de
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program (in a file called LICENSE); if not, write to 
 *  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
 *  Boston, MA 02111-1307, USA, or look at http://www.gnu.org.
 *
 *
 *   double2decimal: converts a double number into decimal notation
 *
 *   the result is returned as a mantissa and decimal exponent.
 *   for regular (neither NAN nor INF) numbers, the mantissa
 *   will be a NUL-terminated string of exactly FP_MAX_PRECISION
 *   digits. the first digit is not a '0' unless the converted
 *   value itself was 0.0. the decimal point of the mantissa is
 *   to the left of the leftmost digit. no sign is included in the
 *   string; rather, a separate flag will indicate negative numbers.
 *
 *   the conversion will always be performed up to the maximum number
 *   of significant digits in a double. the task of rounding, if
 *   desired, is left to the caller.
 *
 *   for non-regular numbers, the string will contain either "INF"
 *   or "NAN";  +INF and -INF are indicated by the sign flag.
 *   NANs have no meaningful sign.
 */

#ifndef __DOUBLE2DECIMAL_C
#define __DOUBLE2DECIMAL_C

#include "math.h"

#define _DEXPLEN        11     //  same as djgppv2
#define _HIDDENBIT      1      //  same as djgppv2
#define _FEXPLEN        8      //  same as djgppv2 
#define BITS(type)      (BITSPERBYTE * (int)sizeof(type)) 
#define DSIGNIF (BITS(double) - _DEXPLEN + _HIDDENBIT - 1) 

/*  maximum number of significant digits (15 for IEEE 64-bit numbers): */
#define FP_MAX_PRECISION  ( (DSIGNIF * 3) / 10 )

/*  we demand the caller to provide a buffer for the result,
 *  to make this function thread-safe.
 */
struct double2decimal_buffer {
  char dummy[ FP_MAX_PRECISION + 1 ];
};

void double2decimal(
  double value    /*  value to be converted  */
, struct double2decimal_buffer *the_buffer  /*  buffer for the created string (mantissa) */
, int *pdp        /*  position of the decimal point (i.e., the decimal exponent) */
                  /*  (0 means: left of the leftmost digit in the buffer) */
, int *pneg       /*  flag: number is negative (maybe -INF) */
, int *pregular   /*  flag: number is neither NAN nor INF */
) {
  int i;
  double ipart;
  char *const buf = (char *)the_buffer;
  char *p, *pe;

  static int initialized = 0;
  static double bigval, inv_bigval;
  if( ! initialized ) {
    double v = 10.0;  /* for thread safety, we use a auto variable for intermediate result */
    for( i = 1; i < FP_MAX_PRECISION; i++ ) {
      v *= 10.0;
    }
    bigval = v;
    inv_bigval = 1.0 / v;
    initialized = 1;
  }

  if( isnan( value ) ) {
    *pregular = 0;
    strcpy( buf, "NAN" );
    *pneg = 0;    /* not really meaningful, but we avoid printing -NAN */
    return;
  }
  if( ( i = isinf( value ) ) ) {
    *pregular = 0;
    *pneg = ( i < 0 );  /*  -INF and +INF are really different */
    strcpy( buf, "INF" );
    return;
  }
  *pregular = 1;

  if( value < 0 ) 
    *pneg = 1, value = -value;
  else
    *pneg = 0;

  /*  we get the number into the range [ inv_bigval .. 1 [ and remember
   *  the decimal exponent, by multiplying with very large or small factors:
   */
  *pdp = 0;
  p = buf;
  pe = buf + FP_MAX_PRECISION;
  if( value > 0 ) {
    while( value < inv_bigval ) {
      value *= bigval;
      *pdp -= FP_MAX_PRECISION;
    }
    while( value >= 1 ) {
      value *= inv_bigval;
      *pdp += FP_MAX_PRECISION;
    }
  } else {    /*  shortcut for value == 0.0  */
    for( ; p < pe ; *(p++) = '0' )
      ;
    *pe = NUL;
    return;
  }

  /*  we now have a number in [ inv_bigval .. 1 [ ; we normalize it
   *  so it is in the range [ 1 .. 10 [:
   */
  for( ;; ) {
    value = modf( value * 10, &ipart );
    i = (int) ipart;
    if( i ) 
      break;
    else
      -- *pdp;
  }

  /*  we now have the correct decimal exponent and can compute the 
   *  mantissa (we compute one additional digit for rounding):
   */
  while( p <= pe ) {
    *(p++) = i + '0';
    value = modf( value * 10, &ipart );
    i = (int) ipart;
  }

  /*  do the rounding:  */
  p = pe;
  *pe += 5;
  while( *p > '9' ) {
    *p = '0';
    if( p > buf )
      ++ * --p; 
    else                   /*  we have rounded up /all/ digits, so we now have only '0's; so... */
      *p = '1', (*pdp)++;  /*  ...we make it a `1000000....'  */
  }

  /*  the last digit (which had been used only for rounding) becomes the terminating NUL: */
  *pe = NUL;
}

#endif

