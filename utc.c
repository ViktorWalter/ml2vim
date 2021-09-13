/*  utc.c
 *
 *  Read and set system time in canonical format.
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
 */

#define PROGRAM "utc"

int debug = 0;

#include "jupp.c"

int main( int argc, char *(argv[]) ) {
  struct tm newtime;
  /* time_t newcaltime; */
  struct timespec ts;
  static char utc_buffer[16];

  if( argc <= 1 ) {
    utc_now( utc_buffer );
    fputs( utc_buffer, stdout );
    fflush( stdout );
    return 0;
  }

  if( strlen( argv[1] ) != 15 ) {
    ufprintf( stderr, "utc: argument not in canonical format.\n" );
    fflush( stderr );
    return 1;
  }
  if( argv[1][8] != '.' ) {
    if( argv[1][8] != ':' ) {
      ufprintf( stderr, "utc: argument not in canonical format.\n" );
      fflush( stderr );
      return 1;
    }
  }

  putenv( "TZ=:UTC" );

  /*   The following call of tzset is redundant under Linux, but
   *   required eg by Digital Unix:
   */
  tzset();

  sscanf( argv[1] + 13, "%u", &(newtime.tm_sec) );
  argv[1][13] = 0;
  sscanf( argv[1] + 11, "%u", &(newtime.tm_min) );
  argv[1][11] = 0;
  sscanf( argv[1] +  9, "%u", &(newtime.tm_hour) );
  argv[1][ 9] = 0;
  sscanf( argv[1] +  6, "%u", &(newtime.tm_mday) );
  argv[1][ 6] = 0;
  sscanf( argv[1] +  4, "%u", &(newtime.tm_mon) );
  newtime.tm_mon--;
  argv[1][ 4] = 0;
  sscanf( argv[1], "%u", &(newtime.tm_year) );
  newtime.tm_year -= 1900;
  newtime.tm_isdst = 0;

  errno = 0;
  ts.tv_nsec = 0;
  /* newcaltime = mktime( &newtime ); */
  /* if( newcaltime != -1 ) { */
  if(1) {
    errno = 0;
    if( clock_settime(CLOCK_REALTIME, &ts) ) {
      ufprintf( stderr, "utc: clock_settime() failed: %s", strerror(errno) );
      fflush( stderr );
      return -1;
    } else {
      return 0;
    }
  } else {
    ufprintf( stderr, "utc: mktime() failed: argument format screwed up?" );
    fflush( stderr );
    return -1;
  }

  return 0;
}

