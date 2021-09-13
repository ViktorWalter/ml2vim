/* jupp.c
 *
 *  Copyright (C) 1998 Timo Felbinger 
 *  email: Timo.Felbinger@quantum.physik.uni-potsdam.de
 *
 *  This file contains standard macros, classes and functions, and
 *  it handles a lot of early stuff. It is the first file to be
 *  #included in most of my programs. this is a stripped version
 *  containing stuff required by ml2vim.
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
 */


#ifndef __JUPP_C
#define __JUPP_C

/*
 *  config.h should have been created by configure from config.h.in:
 */
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <values.h>
#include <getopt.h>
#include <signal.h>

#define NO    0
#define FALSE 0
#define YES   1
#define TRUE  1
#define NUL   '\0'
#define TheCowsComeHome NO

#define until(X)  while(!(X))

#if ! HAVE_ISINF
#  define isinf(X) ( !isnan(X) && !finite(X) )
#endif

/*
 *  low-level error handling
 */

/*  counter for general error conditions: */
int problems = 0;

/*  counter for signals caught and handled: */
int signals_handled = 0;

/*  this value should be only increased via |= and finally be returned by main: */
int retval = 0;
/*  use the following constants to |= into retval: */
#define RETVAL_UNDEFINED  0x0001
#define RETVAL_MEMORY     0x0002
#define RETVAL_FP         0x0004
#define RETVAL_SYSCALL    0x0008
#define RETVAL_LOWLEVEL   0x0010
#define RETVAL_SEGFAULT   0x0020
#define RETVAL_USER       0x0040
#define RETVAL_SECURITY   0x0080

/*
 *  a more flexible replacement for the ?printf family of functions
 *  (this also solves the problem of missing family members on some
 *   systems; eg, Digital Unix 4.0c doesn't know snprintf): 
 */
#include "double2decimal.c"
#include "uprintf.c"

/*  create date-time-string in canonical 15 characters + '\0' format
 *  (buffer s must take 16 bytes). this may create a Y10K problem...
 */
void utc_date_time( char *s, time_t ctm ) {
  struct tm *ptm;

  ptm = gmtime( &ctm );
  usnprintf( 
    s, 16, "%04u%02u%02u.%02u%02u%02u"
    , 1900 + ptm->tm_year, (ptm->tm_mon)+1, ptm->tm_mday
    , ptm->tm_hour, ptm->tm_min, ptm->tm_sec
  );
}
char *utc_now( char *s ) {
  utc_date_time( s, time(NULL) );
  return s;
}

FILE *stdlog = NULL;
FILE *childlog = NULL;
void trace ( char *fmt, ... ) {
  if( debug ) {
    char now[16];
    va_list vap;
    va_start( vap, fmt );
    utc_now( now );
    if( ! stdlog )
      stdlog = fopen( "ml2vim.log", "a" );
    if( stdlog ) {
      ufprintf( stdlog, "\n%s: ", now );
      vufprintf( stdlog, fmt, vap );
      if( errno )
        ufprintf( stdlog, "  errno:%d", errno );
      fflush( stdlog );
    }
    va_end( vap );
  }
}
void ctrace ( char *fmt, ... ) {
  if( debug ) {
    char now[16];
    va_list vap;
    va_start( vap, fmt );
    utc_now( now );
    if( ! childlog )
      childlog = fopen( "ml2vim_child.log", "a" );
    if( childlog ) {
      ufprintf( childlog, "\n%s: ", now );
      vufprintf( childlog, fmt, vap );
      if( errno )
        ufprintf( childlog, "  errno:%d", errno );
      fflush( childlog );
    }
    va_end( vap );
  }
}

/*  a function to deal with serious errors at an early stage where 
 *  more advanced exception handling may not yet be available (if it
 *  is implemented at all).
 */
void die( const char * msg, int err ) {
  if( debug ) {
    trace( "%s: %\\s: %d\n", PROGRAM, "", msg, err );
    trace( "%s: exiting", PROGRAM );
    fclose( stdlog );
  }
  ufprintf( stderr, "\n%s: %\\s: %d\n", PROGRAM, "", msg, err );
  exit( retval );
}

void * safe_malloc( size_t n ) {
  void *p = malloc( n );
  if( ! p )
    /*  we seem to be short on memory, so we probably can't do much  */
    /*  about it, so we just exit:                                   */
    die( "malloc() failed", (int)n );
  return p;
}

void safe_free( void ** pp ) {
  free( *pp );
  *pp = NULL;
}

/*
 *  exec_child: replace current process by the given cmdline.
 *  This function is guaranteed to never return.
 *  The command line will be parsed into individual arguments;
 *  arguments are
 *    - either any sequence of nonblank characters, separated by whitespace,
 *    - or quoted arguments, starting and ending with a `"'; a backslash 
 *      escapes any following char except NUL, or starts an octal escape 
 *      sequence of up to 3 octal digits.
 */
void exec_child( char * cmdline ) {
  char **argv = NULL;
  int argc;
  int pass;
  int quoted;
  int escaped;
  unsigned char c;
  size_t len;
  char *p;

  /*  pass 1:  count arguments, mallocate argv                    */
  /*  pass 2:  count argument lengths, mallocate argv's elements  */
  /*  pass 3:  copy arguments                                     */

  ctrace( "exec_child: got cmdline >>>%s<<<", cmdline );

  for( pass = 1; pass <= 3; ++pass ) {

    ctrace( "exec_child: starting pass %d", pass );

    for( p = cmdline, argc = 0 ; *p ; ++argc ) {
      if( ( quoted = ( *p == '"' ) ) ) ++p;

      len = 0;
      do {
        if( ( escaped = ( *p == '\\' ) ) ) ++p;
        if( ! *p ) break;

        if( escaped ) {

          if( isdigit( *p ) ) {
            c = (unsigned char)*(p++) - (unsigned char)'0';
            if( isdigit( *p ) ) {
              c = c * 8 + (unsigned char)*(p++) - (unsigned char)'0';
              if( isdigit( *p ) )
                c = c * 8 + (unsigned char)*(p++) - (unsigned char)'0';
            }
            if( pass == 3 ) argv[ argc ] [ len ] = (char)c;
          } else {
            if( pass == 3 ) argv[ argc ] [ len ] = *p;
            ++p;
          }
          ++len;

        } else {

          if( quoted ? (*p == '"') : isspace( *p ) ) break;
          if( pass == 3 ) argv[ argc ] [ len ] = *p;
          ++len, ++p;

        }

      } while( TRUE );  /*  loop over one argument  */
      if( pass == 3 )
        argv[ argc ] [ len ] = NUL;

      if( pass == 2 ) 
        argv[ argc ] = (char *)safe_malloc( len + 1 );
      while( *p && isspace( *p ) ) 
        ++p;
    }  /*  loop over arguments  */

    if( pass == 1 )
      argv = (char **)safe_malloc( (size_t)( (argc + 1) * sizeof(char *) ) );
  }  /*  loop over passes  */
  argv[ argc ] = NULL;

  ctrace( "exec_child: about to execute:" );
  for( argc = 0; argv[argc]; ++argc ) {
    ctrace( "  >>>%s<<<", argv[argc] );
  }
  execvp( *argv, argv );
  exit( 2 );
}

#endif

