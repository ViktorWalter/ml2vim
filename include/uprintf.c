/*  uprintf.c
 *
 *  Copyright (C) 1998 Timo Felbinger
 *  email: Timo.Felbinger@quantum.physik.uni-potsdam.de
 *
 *  This code based on the file util_snprintf.c from the apache 1.2.5
 *  sources; see the original copyright statement and disclaimer below.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version, and the conditions listed below in the Apache 
 *  Group's copyright statement.
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
 */

#ifndef __UPRINTF_C
#define __UPRINTF_C

/*  ... we include the copyright statement and disclaimer only once ;-)  */

/* ====================================================================
 * Copyright (c) 1995-1997 The Apache Group.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission.
 *
 * 5. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Group and was originally based
 * on public domain software written at the National Center for
 * Supercomputing Applications, University of Illinois, Urbana-Champaign.
 * For more information on the Apache Group and the Apache HTTP server
 * project, please see <http://www.apache.org/>.
 *
 * This code is based on, and used with the permission of, the
 * SIO stdio-replacement strx_* functions by Panos Tsirigotis
 * <panos@alumni.cs.colorado.edu> for xinetd.
 */


/*  uprintf:  universal printf:
 *      a flexible ?printf() replacement, allowing:
 *       - printing via a user-provided putc()-like callback function,
 *      using this callback mechanism, several standard 
 *      output methods are implemented:
 *       - printing to a dynamically growing buffer
 *         (no overflows and no (well, almost no) arbitrary size limit)
 *       - printing to a stream (like fprintf)
 *       - printing to a fixed-size buffer (like snprintf)
 *      the callback mechanism allows simple implementation of other 
 *      ?printf() variants.
 *
 *  some new format specifications have been defined:
 *   - r, to print indentations
 *   - U, I, O, X, which act like their lower case variants but
 *     allow printing of integers of user-specified size
 *   - C, which prints printable characters like c, a double
 *     backslash for the backslash, and octal escape sequences 
 *     otherwise. chars should have 8 bits when you use this.
 *   - an alternative format for c has been defined, which
 *     allows to print sequences of identical characters by specifying
 *     a width (C does currently not allow this alternative form).
 *   - an alternative format for s has been defined, which allows
 *     to print strings containing NUL characters; this requires a
 *     width specification, which will be used as the string length.
 *   - a new flag \ has been defined to print octal escape sequences for
 *     nonprintable characters. this flag will read a char * argument
 *     from the argument list, which contains characters to be escaped
 *     besides all nonprintable ones and the backslash. if you specify two 
 *     of these flags, this string will contain characters _not_ to be 
 *     escaped besides alphanumerics and underscore.
 *     This flag is mainly intended to be used with s; using it with other
 *     formats is possible but not all characters in the output will be
 *     escaped (for example, it will _not_ work for the alternate c format,
 *     and you cannot escape padding and minus signs in numbers).
 *
 *  no more that MAXINT characters will be printed in a single call
 *  of uprintf, including the terminating NUL, if there is one.
 *  it is an error to try to exceed this limit.
 *  thus, type int can safely be used to count characters and measure
 *  buffer sizes.
 */


#include "double2decimal.c"

/*  default width for floating point numbers:  */
#define UPRINTF_FLOAT_DIGITS        6

/*  maximum precision in numerical conversions:  */
#define UPRINTF_MAX_PRECISION  64

/*  buffer must be large enough for all conversions; it must be
 *  at least FP_MAX_PRECISION + UPRINTF_MAX_PRECISION + n,
 *  where n is a small integer
 */
#define UPRINTF_BUFSIZE       128

/*  read an unsigned int from format string and advance the pointer:  */
unsigned int uprintf_str2dec( const char ** pstr ) {
  unsigned int num = 0;
  char c;
  while( isdigit( c = **pstr ) ) {
    num =  num * 10 + c - '0';
    (*pstr)++;
  }
  return num;
}

/*  Convert an unsigned integer to its decimal format, return pointer to string.
 *  the string is created starting from the /end/ of a caller-provided buffer,
 *  and will /not/ be NUL-terminated. the sign has to be taken care of by the
 *  caller.
 *
 *  we define the argument to be BIG_INT, which should be defined to be the largest 
 *  integer which is available.
 */
char * uprintf_conv_10(
  BIG_UNSIGNED magnitude /*  (modulus of) the number to convert   */
, char *buf              /*  buffer to store the result  */
, char *buf_end          /*  point  beyond last byte of buffer  */
, int *plen              /*  return value: length of string we created (will be 1 or greater)  */
) {
  char *p = buf_end;

  /* We use a do-while loop so that we write at least 1 digit */
  do {
    BIG_UNSIGNED new_magnitude = magnitude / 10;
    if( p <= buf ) {
      /*  we return "****...**" if the number does not fit into the buffer  */
      *plen = buf_end - buf;
      while( buf_end > buf ) *--buf_end = '*';
      return buf;
    }
    *--p = magnitude - new_magnitude * 10 + '0';
    magnitude = new_magnitude;
  } while( magnitude );

  *plen = buf_end - p;
  return p;
}


/*
 *  Convert num to a base X number where X is a power of 2. nbits determines X.
 *  For example, if nbits is 3, we do base 8 conversion
 *  Return value:
 *       a pointer to a string containing the number
 *
 *  The caller provides a buffer for the string: that is the buf_end argument
 *  which is a pointer to the END of the buffer + 1 (i.e. if the buffer
 *  is declared as buf[ 100 ], buf_end should be &buf[ 100 ])
 *
 *  again, we define num to be the largest integer type which is available.
 */
static char * uprintf_conv_p2(
  BIG_UNSIGNED num
, int nbits   /*  radix is 2 ^ nbits, where 1 <= nbits <= 4  */
, char format
, char *buf
, char *buf_end
, int  *plen
) {
  int mask = (1 << nbits) - 1;
  char *p = buf_end;
  static const char lower_digits[] = "0123456789abcdef";
  static const char upper_digits[] = "0123456789ABCDEF";
  const char *digits = (format == 'X') ? upper_digits : lower_digits;

  do {
    if( p <= buf ) {
      *plen = buf_end - buf;
      while( buf_end > buf ) *--buf_end = '*';
      return buf;
    }
    *--p = digits[ num & mask ];
    num >>= nbits;
  } while( num );

  *plen = buf_end - p;
  return p;
}


/*  formatter for the feEgG formats
 *  basically, these formats mean:
 *     - f:  print all digits before and `precision' digits after the .
 *     - eE: print one significant digit before and `precision' digits after the .
 *     - gG: print `precision' significant digits, in either f or e format.
 */ 
char * uprintf_convert_fp(
  char *numbuf   /*  the mantissa, already in decimal notation  */
, int decexp     /*  the decimal exponent corresponding to the mantissa  */
, int precision  /*  the precision specification  */
, char fmt       /*  a character from "feEgG"  */
, int alternate_form  /*  flag: print the decimal point even if nothing follows  */
, char *buf
, char *buf_end 
) {
  char *s = NULL;
  char *decpoint;
  int eexp = decexp - 1;    /*  instead of 0.123e-4, we want  1.23e-5 in e-format  */
  int negexp;
  int digit;

  /*  first, if format if g or G, decide for either e or f:  */
  if( tolower( fmt ) == 'g' ) {
    if( ( eexp < -4 ) || ( eexp >= precision ) ) {
      fmt = 'e' + fmt - 'g';
      /*  adjust the precision:
       *    - for gG:  significant digits, at least one
       *    - for eE:  digits after the decimal point (ie, significant digits - 1)
       */
      if( precision > 0 ) 
        precision--;   
      else
        precision = 0;
    } else {
      fmt = 'f';   /* there is no F format  */
      precision = precision - decexp;   /*  digits to print after the decimal point  */
      if( precision < 0 ) precision = 0;
    }
  }

  /*  some numbers are impossible to represent in f format without
   *  printing "house numbers" (digits with no significance), so we
   *  may have to switch to e:
   */
  if( fmt == 'f' ) 
    if( decexp > FP_MAX_PRECISION ) {
      fmt = 'e'; 
      precision = FP_MAX_PRECISION - 1;  
      /*                     sic ^ ^ ^ (for e: print /all/ significant digits!)  */
    }

  if( fmt == 'f' ) {
    char *p1, *p2;

    /*  round the mantissa, if we are not going to need all digits:  */
    char * roundpos = numbuf + precision + decexp;
    if( (roundpos >= numbuf) && (roundpos < numbuf + FP_MAX_PRECISION) ) {
      *roundpos += 5;
      while( *roundpos > '9' ) {
        *roundpos = '0';
        if( roundpos > numbuf )
          ++ * -- roundpos;
        else
          *roundpos = '1', decexp++, eexp++;
      }
    }

    decpoint = numbuf + decexp;
    if( precision > 0 ) {
      s = buf_end - precision - 1;
      *s = '.';
      p1 = s + 1;
      p2 = decpoint;
      while( p1 < buf_end ) {
        if( p2 >= numbuf + FP_MAX_PRECISION ) {
          *p1 = ' ';     /*  we avoid printing "house numbers"  */
        } else if( p2 < numbuf ) {
          *p1 = '0';
        } else {
          *p1 = *p2;
        }
        p1++, p2++;
      }
    } else {
      if( alternate_form ) {
        s = buf_end - 1;
        *s = '.';
      } else {
        s = buf_end;
      }
    }
    if( decpoint <= numbuf ) {
      * --s = '0';
    } else {
      p2 = decpoint - 1;
      while( p2 >= numbuf ) {
        /*  p2 < numbuf + FP_MAX_PRECISION is now guaranteed  */
        * --s = *(p2--);
      }
    }

  }

  if( tolower( fmt ) == 'e' ) {
    char *p1;

    char * roundpos = numbuf + precision + 1;
    if( (roundpos >= numbuf) && (roundpos < numbuf + FP_MAX_PRECISION) ) {
      *roundpos += 5;
      while( *roundpos > '9' ) {
        *roundpos = '0';
        if( roundpos > numbuf )
          ++ * -- roundpos;
        else
          *roundpos = '1', decexp++, eexp++;
      }
    }
    s = buf_end;
    if( eexp < 0 )
      negexp = 1, eexp = -eexp;
    else
      negexp = 0;

    digit = eexp % 10;
    * --s = '0' + digit;
    eexp /= 10;
    digit = eexp % 10;
    * --s = '0' + digit;
    eexp /= 10;
    while( eexp ) {
      digit = eexp % 10;
      * --s = '0' + digit;
      eexp /= 10;
    }
    * --s = ( negexp ? '-' : '+' );
    * --s = fmt;

    if( precision == 0 ) {
      if( alternate_form ) {
        * --s = '.';
      }
    } else {
      for( p1 = numbuf + precision; p1 > numbuf; p1-- ) 
        * --s = *p1;
      * --s = '.';
    }
    * --s = *numbuf;
  }

  return s;
}

/*
 *   vuprintf:
 *
 *   the central function:  analyses the format string and prints via callback
 */
int vuprintf(
  int (*callback)( char, void * )  /*  the callback function to output single chars  */
, void * cookie                    /*  the cookie to be passed to the callback function  */
, const char * fmt
, va_list ap
) {
  int cc = 0;   /*  counter for total number of characters printed  */
  int i;
  unsigned char ch;

  char *s = NULL;      /*  string resulting from one %-element  */
  int s_len;

  /*  some error messages that may be included in the output:  */
  static const char *const s_null = "(null)";
  static const char *const s_illegal_format = "<<<illegal format specifier";
  static const char *const s_illegal_length = "<<<illegal length argument";

  int min_width = 0;
  int precision = 0;
  enum { LEFT, RIGHT } adjust;
  char pad_char, prefix_char;

  /*  some of the following initializations are just to avoid compiler warnings:  */
  double fpnum = 0.0;
  BIG_INT inum = 0;
  BIG_UNSIGNED imag = 0;
  size_t isize;

  struct double2decimal_buffer numbuf;
  const int bufsize = UPRINTF_BUFSIZE;
  char buf[bufsize];
  char *buf_end = buf + bufsize;
  int decexp;
  int is_regular;

  int long_level;        /*  0: int (double),  1: long int,  2: long long int  */
  int alternate_form;
  int print_sign;
  int print_blank;
  int adjust_precision;
  int adjust_width;
  int escape_level;
  int is_negative;
  int is_unsigned;
  char *escape_chars = "";
  int escape_it;

  int fall_through;
  int break_out;

  while( *fmt ) {

    if (*fmt != '%') {

      if( cc++, ( (*callback)( *(fmt++), cookie ) == EOF ) ) 
        return -cc;

    } else {

      /*  Default variable settings:  */
      adjust = RIGHT;
      escape_level = alternate_form = print_sign = print_blank = 0;
      pad_char = ' ';
      prefix_char = NUL;

      fmt++;

      /* recognize flags: -, #, BLANK, +   */
      for( ; ; fmt++ ) {
        if (*fmt == '-')
          adjust = LEFT;
        else if (*fmt == '+')
          print_sign = 1;
        else if (*fmt == '#')
          alternate_form = 1;
        else if (*fmt == ' ')
          print_blank = 1;
        else if (*fmt == '0')
          pad_char = '0';
        else if (*fmt == '\\') {
          if( ! escape_level ) 
            if( ! ( escape_chars = va_arg( ap, char * ) ) ) 
              escape_chars = "";
          if( escape_level < 2 )
            escape_level++;
        } else
          break;
      }

      /*  check if a width was specified  */
      if( isdigit(*fmt) ) {
        min_width = uprintf_str2dec( &fmt ); 
        adjust_width = 1;
      } else if (*fmt == '*') {
        min_width = va_arg( ap, int );
        fmt++;
        adjust_width = 1;
        if (min_width < 0) {
          adjust = LEFT;
          min_width = -min_width;
        }
      } else {
        adjust_width = 0;
      }

      /*  check if a precision was specified  */
      if( *fmt == '.' ) {
        adjust_precision = 1;
        fmt++;
        if( isdigit(*fmt) ) {
          precision = uprintf_str2dec( &fmt );
        } else if( *fmt == '*' ) {
          precision = va_arg( ap, int );
          fmt++;
          if (precision < 0) {
            precision = 0;
          }
        } else {
          precision = 0;
        }
      } else {
        adjust_precision = 0;
      }

      /*  check for size modifiers; we allow GNU-style ll, L or q for 'long long' */
      long_level = 0;
      do {
        if( (*fmt == 'L') || (*fmt == 'q') ) {
          fmt++;
          long_level = 2;
          continue;
        }
        if( (*fmt == 'l') && (long_level < 2) ) {
          fmt++;
          long_level++;
          continue;
        } 
        break;
      } until( TheCowsComeHome );

      /*  *fmt should now be a conversion specifier; we evaluate it:  */

      fall_through = break_out = is_unsigned = 0;

      switch (*fmt) {

        case 'U':
          /*
           *  format U will print unsigned integers of user specified size:
           *  the argument list should contain an object of type size_t,
           *  specifying the size of the integer, followed by the integer itself.
           */

          if( ! fall_through ) {

            /*  I don't know a portable way to retrieve a pointer to an
             *  argument of a variadic function, rather than the argument
             *  itself (was this forgotten when defining stdarg?), so we
             *  use a kludge which allows only a limited number of integer
             *  sizes:
             */
            switch( isize = va_arg( ap, size_t ) * BITS_PER_CHAR ) {
#             if HAVE_INT8
              case 8:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED8 );
                break;
#              endif
#              if HAVE_INT16
              case 16:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED16 );
                break;
#              endif
#              if HAVE_INT32
              case 32:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED32 );
                break;
#              endif
#              if HAVE_INT64
              case 64:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED64 );
                break;
#              endif
              default:
                s = (char *)s_illegal_length;
                s_len = strlen(s);
                break_out = 1;
                break;
            }
            if( break_out ) break;
            fall_through = 1;
            is_unsigned = 1;
            is_negative = 0;
          }

        case 'I': 

          if( ! fall_through ) {
            switch( isize = va_arg( ap, size_t ) * BITS_PER_CHAR ) {
#              if HAVE_INT8
              case 8:
                inum = (BIG_INT) va_arg( ap, INT8 );
                break;
#              endif
#              if HAVE_INT16
              case 16:
                inum = (BIG_INT) va_arg( ap, INT16 );
                break;
#              endif
#              if HAVE_INT32
              case 32:
                inum = (BIG_INT) va_arg( ap, INT32 );
                break;
#              endif
#              if HAVE_INT64
              case 64:
                inum = (BIG_INT) va_arg( ap, INT64 );
                break;
#              endif
              default:
                s = (char *)s_illegal_length;
                s_len = strlen(s);
                break_out = 1;
                break;
            }

            if( break_out ) break;
            if( (is_negative = ( inum < 0 )) ) {
              BIG_INT t = inum + 1;
              imag = ( (BIG_UNSIGNED)(-t) ) + 1;
            } else {
              imag = (BIG_UNSIGNED) inum;
            }

            fall_through = 1;
          }

        case 'u':

          if( ! fall_through ) {
            switch( long_level ) {
              case 0 :
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned int );
                break;
              case 1 :
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long );
                break;
              case 2 :
              default:  /* this default should never apply, but anyway...  */
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long long );
                break;
            }
            is_unsigned = 1;
            is_negative = 0;
            fall_through = 1;
          }

        case 'd':
        case 'i':

          if( ! fall_through ) {
            switch( long_level ) {
              case 0 :
                inum = va_arg( ap, int );
                break;
              case 1 :
                inum = va_arg( ap, long );
                break;
              case 2 :
              default:
                inum = va_arg( ap, long long );
                break;
            }
            if( (is_negative = ( inum < 0 )) ) {
              BIG_INT t = inum + 1;
              imag = ( (BIG_UNSIGNED)(-t) ) + 1;
            } else {
              imag = (BIG_UNSIGNED) inum;
            }
            fall_through = 1;
          }

          s = uprintf_conv_10( imag , buf , buf_end , &s_len );
          if( adjust_precision ) {
            if( precision > UPRINTF_MAX_PRECISION )
              precision = UPRINTF_MAX_PRECISION; 
            while ( s_len < precision ) 
              *--s = '0' , s_len++ ; 
          }

          if( ! is_unsigned ) {
            if( is_negative )
              prefix_char = '-';
            else if( print_sign )
              prefix_char = '+';
            else if( print_blank )
              prefix_char = ' ';
          }

          break;

        case 'O':

          if( ! fall_through ) {
            switch( isize = va_arg( ap, size_t ) * BITS_PER_CHAR ) {
#              if HAVE_INT8
              case 8:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED8 );
                break;
#              endif
#              if HAVE_INT16
              case 16:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED16 );
                break;
#              endif
#              if HAVE_INT32
              case 32:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED32 );
                break;
#              endif
#              if HAVE_INT64
              case 64:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED64 );
                break;
#              endif
              default:
                s = (char *)s_illegal_length;
                s_len = strlen(s);
                break_out = 1;
                break;
            }
            if( break_out ) break;
            fall_through = 1;
          }

        case 'o':

          if( ! fall_through ) {
            switch( long_level ) {
              case 0:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned int );
                break;
              case 1:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long );
                break;
              case 2:
              default:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long long );
                break;
            }
            fall_through = 1;
          }

          s = uprintf_conv_p2(
            imag
          , 3       /*  convert radix 2^3  */
          , *fmt
          , buf
          , buf_end
          , &s_len
          );
          if( adjust_precision ) {
            if( precision > UPRINTF_MAX_PRECISION )
              precision = UPRINTF_MAX_PRECISION; 
            while ( s_len < precision ) 
              *--s = '0' , s_len++ ; 
          }
          if( alternate_form && *s != '0' ) {   
            *--s = '0';
            s_len++;
          }

          break;

        case 'X':

          if( ! fall_through ) {
            switch( isize = va_arg( ap, size_t ) * BITS_PER_CHAR ) {
#              if HAVE_INT8
              case 8:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED8 );
                break;
#              endif
#              if HAVE_INT16
              case 16:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED16 );
                break;
#              endif
#              if HAVE_INT32
              case 32:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED32 );
                break;
#              endif
#              if HAVE_INT64
              case 64:
                imag = (BIG_UNSIGNED) va_arg( ap, UNSIGNED64 );
                break;
#              endif
              default:
                s = (char *)s_illegal_length;
                s_len = strlen(s);
                break_out = 1;
                break;
            }
            if( break_out ) break;
            fall_through = 1;
          }

        case 'x':

          if( ! fall_through ) {
            switch( long_level ) {
              case 0:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned int );
                break;
              case 1:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long );
                break;
              case 2:
              default:
                imag = (BIG_UNSIGNED) va_arg( ap, unsigned long long );
                break;
            }
            fall_through = 1;
          }

        case 'p':

          if( ! fall_through ) {
            imag = (INT_FOR_POINTER) va_arg( ap, void * );
            precision = ( BITS_PER_CHAR * sizeof(void *) ) / 4;
            adjust_precision = 1;
            pad_char = ' ';
            fall_through = 1;
          }

          s = uprintf_conv_p2( 
            imag
          , 4      /*  convert radix 2^4  */
          , *fmt
          , buf
          , buf_end
          , &s_len
          );
          if( adjust_precision ) {
            if( precision > UPRINTF_MAX_PRECISION )
              precision = UPRINTF_MAX_PRECISION; 
            while ( s_len < precision ) 
              *--s = '0' , s_len++ ; 
          }
          if( alternate_form ) {
            *--s = 'x';
            *--s = '0';
            s_len += 2;
          }

          break;

        case 's':

          s = va_arg( ap, char * );
          if( ! s ) s = (char *)s_null;
          if( alternate_form && adjust_width ) {
            s_len = min_width;
            adjust_width = 0;
          } else {
            s_len = strlen(s);
            if( adjust_precision && precision < s_len )
              s_len = precision;
          }
          pad_char = ' ';

          break;

        /*  for format c, an alternative format has been defined:
         *  if a positive width specification is given, this number
         *  of identical characters will be printed. for missing or
         *  non-positive width, no characters will be printed.
         */
        case 'c':

          *( s = buf ) = (char) (va_arg( ap, int ));
          s_len = 1;
          if( alternate_form ) {
            pad_char = *s;
            if( (!adjust_width) || (adjust == LEFT) || (min_width == 0) ) 
              adjust_width = NO, s_len = 0;
          } else {
            pad_char = ' ';
          }
          break;

        /*  a new format specifier: C
         *  like c for printable characters; otherwise, an escape sequence
         *  of 3 octal digits will be printed.
         */
        case 'C':

          s = buf;
          ch = (unsigned char)(va_arg( ap, unsigned char ));
          if( ch == '\\' ) {   /*  backslash must always be escaped  */
            s[0] = s[1] = '\\';
            s_len = 2;
          } else if( isprint(ch) ) {
            *s = ch;
            s_len = 1;
          } else {
            *s = '\\';
            s[1] = '0' + ((ch >> 6) & 07);
            s[2] = '0' + ((ch >> 3) & 07);
            s[3] = '0' + (ch & 07);
            s_len = 4;
          }

        /*  a new format specifier: r
         *  indentation (useful only with a width specification, often *),
         *    - negative or missing width: do nothing
         *    - width == 0: newline
         *    - width > 0: newline and blank padding
         */
        case 'r':

          s = buf;
          s_len = 0;
          if( ! adjust_width ) break;
          if( (adjust == LEFT) && (min_width > 0) ) {
            adjust_width = NO;
            break;
          }
          *s = '\n';
          s_len = 1;
          adjust = LEFT;
          min_width++;
          pad_char = ' ';
          break;

        case 'f':
        case 'e':
        case 'E':
        case 'g':
        case 'G':

          /*  the FP formats always require a precision, so we define a default:  */
          if( ! adjust_precision )
            precision = UPRINTF_FLOAT_DIGITS;

          if( precision > UPRINTF_MAX_PRECISION ) 
            precision = UPRINTF_MAX_PRECISION; 

          switch( long_level ) {
            case 0 : 
              fpnum = (double) va_arg( ap, float );
              break;
            case 1 :
              fpnum = va_arg( ap, double );
              break;
            default:   /*  long double is currently not defined  */
              s = (char *)s_illegal_format;
              s_len = strlen(s);
              break_out = 1;
              break;
          }
          if( break_out ) break;

          double2decimal( fpnum, &numbuf, &decexp, &is_negative, &is_regular );

          if( ! is_regular ) {   /*  treat INF and NAN separately:  */

            if( isnan( fpnum ) ) {    /*  NANs get neither - nor + signs  */
              if( print_sign || print_blank )
                prefix_char = ' ';
            }
            if( isinf( fpnum ) ) {    /*  INFs are signed  */
              if( is_negative )
                prefix_char = '-';
              else if( print_sign )
                prefix_char = '+';
              else if( print_blank )
                prefix_char = ' ';
            }
            s = (char *)&numbuf;
            s_len = strlen( s );
            pad_char = ' ';       /*  NAN or INF are never '0'-padded  */
            break;
          }

          if( is_negative )
            prefix_char = '-';
          else if( print_sign )
            prefix_char = '+';
          else if( print_blank )
            prefix_char = ' ';

          s = uprintf_convert_fp( 
            (char *)&numbuf, decexp, precision, *fmt, alternate_form, buf, buf_end 
          ); 
          s_len = buf_end - s;

          break;

        case '%':

          *( s = buf ) = '%';
          s_len = 1;
          pad_char = ' ';
          break;

        case NUL:

          /*  The last character of the format string was %; we ignore it:  */

          continue;

        default:

          /*  The default case is for unrecognized %'s.  */

          s = (char *)s_illegal_format;
          s_len = strlen(s);
          break;

      }   /*  switch( conversion specifier )  */

      /*  we are now ready to print:  s contains the main portion of the output,
       *  and prefix_char, iff not NUL, contains a possible prefix,
       *
       *  prefix and left padding requires some caution:
       *    - if we pad with '0', we print the prefix first;
       *    - if we pad with ' ', we print first the padding
       *  note that for right padding, both cases are equivalent.
       */
      if( prefix_char != NUL ) {
        if( pad_char == '0' ) {
          if( cc == MAXINT ) return -cc;
          if( cc++, ( (*callback)( prefix_char, cookie ) == EOF ) ) 
            return -cc;
          prefix_char = NUL;
          min_width--;
        } else {
          * --s = prefix_char, s_len++;
        }
      }
      if( adjust_width && adjust == RIGHT )
        for( i = s_len; i < min_width; i++ ) {
          if( cc == MAXINT ) return -cc;
          if( cc++, ( (*callback)( pad_char, cookie ) == EOF ) ) 
            return -cc;
        }

      /*  Print the string s (NOTE: s is /not/, in general, NUL-terminated!)  */
      for( i = 0; i < s_len; i++ ) {
        switch( escape_level ) {
          case 0:
            escape_it = NO;
            break;
          case 1:
            if( isprint( s[i] ) && (s[i] != '\\') )
              escape_it = ( strchr( escape_chars, s[i] ) != NULL );
            else
              escape_it = YES;
            break;
          case 2:
          default:
            if( isalnum( s[i] ) || ( s[i] == '_' ) )
              escape_it = NO;
            else
              escape_it = ( strchr( escape_chars, s[i] ) == NULL );
            break;
        }
        if( escape_it ) {
          if( cc >= MAXINT - 2 ) return -cc;
          if( cc++, ( (*callback)( '\\', cookie ) == EOF ) ) 
            return -cc;
          if( cc++, ( (*callback)( ((s[i] >> 6 ) & 07 ) + '0', cookie ) == EOF ) ) 
            return -cc;
          if( cc++, ( (*callback)( ((s[i] >> 3 ) & 07 ) + '0', cookie ) == EOF ) ) 
            return -cc;
          if( cc++, ( (*callback)( ((s[i]      ) & 07 ) + '0', cookie ) == EOF ) ) 
            return -cc;

        } else {
          if( cc == MAXINT ) return -cc;
          if( cc++, ( (*callback)( s[i], cookie ) == EOF ) ) 
            return -cc;
        }
      }

      /*  do padding on right, if we are left-adjusting:  */
      if( adjust_width && adjust == LEFT )
        for( i = s_len; i < min_width; i++ ) {
          if( cc == MAXINT ) return -cc;
          if( cc++, ( (*callback)( pad_char, cookie ) == EOF ) ) 
            return -cc;
        }

      /*  done. skip the conversion specifier:  */
      fmt++;

    }  /*  if( is_a_%_thingy )  */

  }  /*  while( *fmt )  */

  return cc;
}


/*
 *we now implement several members of the ?printf family:
 *
 *   we use a callback mechanism for character output, to implement
 *   different output methods (stream, string, and others) in a
 *   consistent way. 
 *
 *
 *  udprintf():  output to a dynamically growing string:
 *    parameters:
 *       - pbuffer: points to a pointer to a buffer for the created string;
 *                  this buffer must have been allocated via malloc, or the
 *                  pointer to the buffer must be NULL (it will then be 
 *                   allocated automatically).
 *       - psize:   points to an integer representing the size of
 *                  the buffer. must be in [ 1 ; MAXINT ] (it is an error
 *                   to pass a value outside of this range).
 *                  if uprintf has to allocate the buffer, it will use
 *                  this size.
 *         pbuffer will be realloc()ated, if necessary, and *psize
 *         increased accordingly, by factors of 2, or up to MAXINT,
 *         whichever is smaller.
 *       - format, ap: format string and variable arguments, as in vprintf
 *
 *   Return value: On success, the non-negative number of characters printed, not
 *   counting the terminating NUL. On error, the negative number of the character
 *   to be printed when the error occurred (-1 for the most early error).
 *   On error, the character indicated by the return value may or may not be in
 *   the output buffer.
 *   On return, whether successful or not, the buffer is guaranteed to be either 
 *   NUL-terminated or NULL; the latter can only happen if it was NULL initally 
 *   and could not be allocated, or if the size argument was out of range initially.
 *   There will never be more than MAXINT characters allocated.
 *
 *   For udprintf, the cookie of the callback function is a special structure
 *   containing bookkeeping information; note that although size_t may be larger 
 *   than int, size will never take up values larger than MAXINT and thus can 
 *   safely be assigned to int:
 */
struct udprintf_struct {
  char * buffer;
  size_t size;
  unsigned int index;
};

/*   This callback function assumes that the buffer is valid and has a 
 *   positive size. Note that the buffer remains valid even if realloc 
 *   fails (except for some broken systems where realloc may render the
 *   old buffer invalid on failure).
 */
int udprintf_callback( char c, void * cookie ) {
  struct udprintf_struct *ps = (struct udprintf_struct *)cookie;
  char *pnew;
  size_t newsize;

  if( ps->index >= ps->size ) {
    if( ps->size > MAXINT / 2 ) {
      if( ps->size == MAXINT ) {
        ps->buffer[ ps->size - 1 ] = NUL;
        return EOF;
      }
      newsize = MAXINT;
    } else {
      newsize = ps->size * 2;
    }
    if( ( pnew = (char *)realloc(ps->buffer, newsize) ) ) {
      ps->buffer = pnew;
      ps->size = newsize;
    } else {
      ps->buffer[ ps->size - 1 ] = NUL;
      return EOF;
    }
  }
  return (ps->buffer)[ (ps->index)++ ] = c;
}

int vudprintf( char ** pbuffer, size_t * psize, const char *format, va_list ap ) {
  int cc;
  struct udprintf_struct s;

  if( ( *psize <= 0 ) || ( *psize > MAXINT ) ) {
    if( *pbuffer ) free( *pbuffer );
    *pbuffer = NULL;
    return -1;
  }
  s.buffer = * pbuffer;
  s.size = * psize;
  s.index = 0;

  if( ! s.buffer ) s.buffer = (char *)malloc( s.size );
  if( ! s.buffer ) return -1;

  if( ( cc = vuprintf( udprintf_callback, (void *) &s, format, ap ) ) < 0 ) { 
    *pbuffer = s.buffer;
    *psize = s.size;
    /*  on error, we don't bother reallocating the buffer, so
     *  we may have to overwrite the last character with the NUL:
     */
    if( s.index >= s.size ) s.index--;
    *pbuffer[ s.index ] = NUL;
    return cc; 
  }

  /*  append the regular terminating NUL;  */
  if( udprintf_callback( NUL, (void *) &s ) == EOF ) {
    *pbuffer = s.buffer;
    *psize = s.size;
    if( s.index >= s.size ) s.index--;
    *pbuffer[ s.index ] = NUL;
    if( cc < MAXINT )
      return -cc-1;
    else
      /*   although -MAXINT-1 /can/ be represented on existing hardware,
       *   we don't rely on this:
       */
      return -cc;
  }

  *pbuffer = s.buffer;
  *psize = s.size;

  return cc;
}

int udprintf( char ** pbuffer, size_t * psize, const char *format, ... ) {
  va_list vap;
  int cc;

  va_start( vap, format );
  cc = vudprintf( pbuffer, psize, format, vap );
  va_end( vap );

  return cc;
}


/*
 *   ufprintf(): stream output (similar to standard fprintf)
 *
 *   the cookie for ufprintf_callback is the stream (ie a FILE *)
 */

int ufprintf_callback( char c, void * cookie ) {
  return putc( c, (FILE *)cookie );
}

int vufprintf( FILE *stream, const char *format, va_list ap ) {
  return vuprintf( ufprintf_callback, (void *)stream, format, ap );
}

int ufprintf( FILE *stream, const char *format, ... ) {
  va_list vap;
  int cc;

  va_start( vap, format );
  cc = vufprintf( stream, format, vap );
  va_end( vap );

  return cc;
}


/*
 *  usnprintf():  output in fixed-length buffer of length n (including the NUL)
 *    return value:  
 *      - non-negative:  success; value is number of characters printed
 *      - negative:  failure; number of the character about to be printed
 *                   when the error occurred (typical: -n means: overflow)
 *    the resulting string is guaranteed to be NUL-terminated (even in case
 *    of an error); it will be of length 0 if n <= 0, so the buffer _must_ 
 *    take at least 1 character.
 */
struct usnprintf_struct {
  char * bp;
  char * be;
};

int usnprintf_callback( char c, void * cookie ) {
  struct usnprintf_struct * ps = (struct usnprintf_struct *)cookie;

  if( ps->bp < ps->be ) 
    return( *( (ps->bp) ++ ) = c );
  else
    return EOF;
}

int vusnprintf( char * buffer, size_t n, const char *format, va_list ap ) {
  int cc;
  struct usnprintf_struct s;

  s.bp = buffer;
  s.be = buffer + n - 1;
  /*                ^ ^ ^  reserve space for terminating NUL  */

  cc = vuprintf( usnprintf_callback, (void *) &s, format, ap );
  /*  the resulting string is guaranteed to be NUL-terminated;
   *  note that this will work correctly even for n <= 0
   */
  *( s.bp ) = NUL;

  return cc;    /*  on buffer overflow, cc == -n will be returned  */
}

int usnprintf( char * buffer, size_t n, const char *format, ... ) {
  va_list vap;
  int cc;

  va_start( vap, format );
  cc = vusnprintf( buffer, n, format, vap );
  va_end( vap );

  return cc;
}


/*   uprintf():  generic output with callback mechanism.
 *
 *     int callback( char c, void * ) takes the character c and a
 *     cookie of type void *, which can be used to transfer additional
 *     information or store bookkeeping information.
 *     the callback function should return c in case of success,
 *     and EOF on error (like putc).
 *
 *   vuprintf() is defined above and contains the format string parser
 */
int uprintf( int callback( char, void * ), void * cookie, const char *format, ... ) {
  va_list vap;
  int cc;

  va_start( vap, format );
  cc = vuprintf( callback, cookie, format, vap );
  va_end( vap );

  return cc;
}

#endif


