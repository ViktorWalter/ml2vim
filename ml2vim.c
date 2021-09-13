#define PROGRAM "ml2vim"

/*
 *  ml2vim: an auxiliary program connecting vim and mathematica
 *  (we speak mathlink!)
 *
 *  Copyright (C) 1999 Timo Felbinger
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
 *  This file is divided into 4 sections (for historical reasons, not in
 *  logical order):
 *   - SECTION III:
 *      Input from vim is read line by line.
 *      Consecutive input lines bearing identical tags are bundled into
 *      packages and scheduled for delivery to mma.
 *   - SECTION II:
 *      Packages are delivered to mma, one complete block of input lines at
 *      a time. To keep synchronized, we wait for the kernel's new input prompt
 *      before sending the next packet.
 *   - SECTION IV:
 *      Return packets from mma are broken into appropriately tagged lines,
 *      which are scheduled for delivery to vim.
 *   - SECTION I:
 *      Packages are delivered to vim's asynchronous input channel.
 *      Alternatively, lines can also be delivered synchronously. this could
 *      be used (currently, it isn't) for immediate replies of this program to
 *      input received from vim.
 *  preceeding SECTION I, there are some more general definitions and declarations
 *  which didn't really fit anywhere else, and a small section on VIEWERS.
 *
 *  This file is in alpha stage and still contains a lot of debugging code,
 *  which might be removed at some time.
 */

int debug = 0;  /*  whether we run in debug mode (to be set from command line)  */

#include "config.h"
#include "jupp.c"
#include "mathlink.h"

/*  The following descriptors should connect us with our parent:  */
#define STDIN     0
#define STDOUT    1
#define STDERR    2
#define ASYNC_OUT 3

#define MAX_TAG_LEN 32

#define BUFSIZE   512  /*  just initial size. it will grow as necessary  */


/*  global variables for mathlink  */

MLENV mlenv;  /* (pointer to) playground for mathlink library        */
MLINK mlink;  /* handle for open mathlink connection                 */
long *mlerr;  /* mathlink error. haven't figured out the details yet */

/*  the following flags may be raised by signal handler:  */
volatile int leave_mainloop;
volatile int got_sigchld;

/*
 *  options:
 *  They can be set from vim by submitting option lines
 *  Global options can be set by prepending a bang and they can be overridden
 *  individually on a per-cell basis
 */

/*
 *  for slow terminals, in particular, we can reduce the
 *  refresh rate when Vim receives input from Mma.
 *  keyboard input will always cause a screen refresh, if necessary.
 */
enum refresh_option { 
  IMMEDIATE,    /*  after every line             */
  BLOCK,        /*  after every complete block   */
  CELL,         /*  after every cell             */
  LAZY          /*  at certain intervals         */
};
struct option_s {
  /*  -b, --blank: whether to allow blank output lines  */
  int allow_blank;        
  /*  -d, --display: whether we want to receive display packets        */
  /* int display_output;       <---  obsolete:  use suppress instead!  */
  /*  -p, --piggyback:  combine single line output with tag            */
  int piggyback_single_line_output;   
  /*  -r{i,b,c,l}, --refresh={immediate,block,cell,lazy):      */
  /*     how often to refresh the display:                     */
  enum refresh_option refresh;        /*  how often to update the display  */
  /*  -s{a,o,t,d,m,x}, --suppress={all,output,text,display,messages,dummy}  */
  int suppress_output
    , suppress_text
    , suppress_display
    , suppress_message
    , suppress_dummy;
  /*  -v=name,"command", --viewer=name,"command"  */
  char *viewer_name, *viewer_command;

  /*  the following are still defunct (may be activated in a later release)  */
  int reformat;              /*  whether to reformat the output  */
  int linelength;            /*  line length limit (when reformatting output) */
} global_options;

/*
 *  initialize_options:
 *    This will initialize an option structure with the global
 *    settings, or it will initialize the global settings with
 *    compile-time defaults.
 */
void initialize_options( struct option_s *po ) {
  if( po == &global_options ) {
    po->allow_blank = TRUE;
    /*  po->display_output = FALSE;  */
    po->piggyback_single_line_output = TRUE;
    po->refresh = IMMEDIATE;
    po->suppress_output = po->suppress_text = FALSE;
    po->suppress_display = po->suppress_message = po->suppress_dummy = FALSE;
    po->viewer_name = po->viewer_command = NULL;

    po->reformat = FALSE;
    po->linelength = 96;
  } else {
    memcpy( po, &global_options, sizeof( struct option_s ) );
    if( global_options.viewer_name ) {
      po->viewer_name = (char *)safe_malloc( strlen( global_options.viewer_name ) + 1 );
      strcpy( po->viewer_name, global_options.viewer_name );
    }
    if( global_options.viewer_command ) {
      po->viewer_command = (char *)safe_malloc( strlen( global_options.viewer_command ) + 1 );
      strcpy( po->viewer_command, global_options.viewer_command );
    }
  }
}

/*  this should really be written in C++ ...  */
void destruct_options( struct option_s *po ) {
  if( po->viewer_name ) free( po->viewer_name );
  if( po->viewer_command ) free( po->viewer_command );
}

int swrite( int fd, char *s ) {
  ssize_t to_write = strlen( s );
  return ( write( fd, s, to_write ) == to_write );
}

/*
 *
 *  VIEWERS:
 *
 *  child processes, meant for viewing postscript pictures.
 *  the postscript code must come in a RETURNTEXTPKT (_not_ in a
 *  DISPLAYPKT as usual), and must be true postscript.
 *
 *  You can get this behaviour by defining
 *
 *    $DisplayFunction = DisplayString[ #, "EPS" ] <> "\n" &
 *
 *  in Mathematica. The trailing newline makes it easier to
 *  detect the end of the output in external filters.
 *
 *  Currently, I don't know a good postscript viewer which can read
 *  EPS from stdin (ghostview and even gv-3.5.8 can, in principle, but
 *  the output sucks as they won't use the BoundingBox coordinates).
 *
 */

/*  viewers are stored in a doubly-linked list:  */
struct viewer_s {

  char *name;   /*  viewers are identified by a name  */
  long int mma_tag_num;   /*  this is to identify obsolete viewers  */
  pid_t pid;
  int viewer_in, viewer_out, viewer_err;
  FILE *outstream;
  struct viewer_s *next, *prev;
} * viewers;

/*
 * launch viewer:
 *   fork() a new viewer child process and prepend it to the list of viewers.
 *   returns a pointer to the new viewer on success, or NULL on error.
 */
struct viewer_s * launch_viewer( char *name, long int mma_tag_num, char *command ) {
  struct viewer_s *pnew = NULL;
  int fd_to_viewer[2];
  int fd_from_viewer[2];
  int fd_err_viewer[2];
  FILE *outstream;
  int fd;

  fd_to_viewer[0] = fd_to_viewer[1] = -1;
  fd_from_viewer[0] = fd_from_viewer[1] = -1;
  fd_err_viewer[0] = fd_err_viewer[1] = -1;
  outstream = NULL;

  trace( "launch_viewer: name: >>>%s<<<, cmdline: >>>%s<<<", name, command );

  if( ! command )
    return NULL;

  if( pipe( fd_to_viewer ) < 0 ) {
    ufprintf( stderr, "launch_viewer: pipe() failed for fd_to_viewer" );
    fflush( stderr );
    goto launch_viewer_error;
  }
  if( ! ( outstream = fdopen( fd_to_viewer[1], "w" ) ) ) {
    ufprintf( stderr, "launch_viewer: fdopen() failed for fd_to_viewer[1]" );
    fflush( stderr );
    goto launch_viewer_error;
  }
  if( pipe( fd_from_viewer ) < 0 ) {
    ufprintf( stderr, "launch_viewer: pipe() failed for fd_from_viewer" );
    fflush( stderr );
    goto launch_viewer_error;
  }
  if( pipe( fd_err_viewer ) < 0 ) {
    ufprintf( stderr, "launch_viewer: pipe() failed for fd_err_viewer" );
    fflush( stderr );
    goto launch_viewer_error;
  }
  pnew = (struct viewer_s *)safe_malloc( sizeof( struct viewer_s ) );
  pnew->name = (char *)malloc( strlen( name ) + 1 );
  strcpy( pnew->name, name );
  pnew->mma_tag_num = mma_tag_num;

  if( ( pnew->pid = fork() ) == -1 ) {
    ufprintf( stderr, "launch_viewer: fork() failed" );
    fflush( stderr );
    goto launch_viewer_error;
  }

  if( pnew->pid > 0 ) {
    trace( "launch_viewer: pid = %d", pnew->pid );
    trace( 
      "  got descriptors: to: %d/%d  from: %d/%d  err: %d/%d "
    , fd_to_viewer[0] , fd_to_viewer[1]
    , fd_from_viewer[0] , fd_from_viewer[1]
    , fd_err_viewer[0] , fd_err_viewer[1]
    );

    pnew->viewer_in = fd_to_viewer[1];
    pnew->outstream = outstream;
    close( fd_to_viewer[0] );

    pnew->viewer_out = fd_from_viewer[0];
    close( fd_from_viewer[1] );

    pnew->viewer_err = fd_err_viewer[0];
    close( fd_from_viewer[1] );

    pnew->prev = NULL;

    if( viewers ) viewers->prev = pnew;

    pnew->next = viewers;
    viewers = pnew;

    ctrace( "launch_viewer (parent): will now return" );
    return pnew;

  } else {

    ctrace( "launch_viewer (child): hello, world! I'm process %d", getpid() );

    dup2( fd_to_viewer[0], 0 );
    close( fd_to_viewer[0] );
    fclose( outstream );

    /*  viewers will live in a separate session together with all their
     *  children, if any, so we can easily kill all of them at once, if
     *  necessary:
     */
    if( ( fd = open( "/dev/tty", O_RDWR|O_NOCTTY) ) >= 0 ) {
      ioctl( fd, TIOCNOTTY, NULL );
      close( fd );
    }
    setsid();

    dup2( fd_from_viewer[1], 1 );
    close( fd_from_viewer[1] );
    close( fd_from_viewer[0] );

    dup2( fd_err_viewer[1], 2 );
    close( fd_err_viewer[1] );
    close( fd_err_viewer[0] );

    /*  currently, we don't really read anything from viewers, so  */
    /*  we have to redirect stdout and stderr to avoid blocking:   */
    freopen( "/dev/null", "w", stdout );
    freopen( "/dev/null", "w", stderr );

    ctrace( "launch_viewer (child): about to call exec_child" );
    exec_child( command );

    /*  this point will never be reached  */
  }

  trace( "launch_viewer: error, bailing out", errno );
  launch_viewer_error:
    /*  on error, try to undo all resource allocation:  */
    if( fd_to_viewer[0] > 0 ) close( fd_to_viewer[0] );
    if( outstream )
      fclose( outstream );
    else 
      if( fd_to_viewer[1] > 0 ) close( fd_to_viewer[1] );
    if( fd_from_viewer[0] > 0 ) close( fd_from_viewer[0] );
    if( fd_from_viewer[1] > 0 ) close( fd_from_viewer[1] );
    if( fd_err_viewer[0] > 0 ) close( fd_err_viewer[0] );
    if( fd_err_viewer[1] > 0 ) close( fd_err_viewer[1] );
    if( pnew ) {
      if( pnew->name ) free( pnew->name );
      free( pnew );
    }
    return NULL;
}

void unlink_viewer( struct viewer_s *pv ) {
  trace( "unlink_viewer: going to unlink >>>%s<<<", pv->name );
  if( pv->prev ) pv->prev->next = pv->next;
  if( pv->next ) pv->next->prev = pv->prev;
  if( pv->name ) free( pv->name );
  free( pv );
}

/*
 * kill_all_viewers:
 *  This will kill all viewers by sending a SIGTERM, but will not check
 *  whether they actually die. It can be called atexit() from ml2vim.
 */
void kill_all_viewers( void ) {
  struct viewer_s *pv;
  for( pv = viewers; pv; pv = pv->next )
    if( pv->pid )
      kill( - pv->pid, SIGTERM ), pv->pid = 0;
}

void kill_viewer( struct viewer_s * pv ) {
  trace( "kill_viewer: going to kill >>>%s<<<", pv->name );
  if( pv->pid )
    kill( - pv->pid, SIGTERM ), pv->pid = 0;
}

struct viewer_s *putc_to_viewer( int c, struct viewer_s *pv ) {
  static long int total = 0;
  if( fputc( c, pv->outstream ) == EOF ) {
    ufprintf( 
      stderr
    , "\nputc_to_viewer: fputc() failed, shutting down viewer %s. total: %ld"
    , pv->name, total
    );
    trace( "putc_to_viewer: fputc() failed, killing viewer %s", pv->name );
    kill_viewer( pv );
    pv = NULL;
  }
  ++total;
  return pv;
}

/*
 * a hygienic measure:
 *   wait() for dead childs (currently, the only childs are viewers) and thus 
 *   avoid zombies. This will also update the viewer list should a viewer exit 
 *   unexpectedly.
 */
void check_for_dead_childs( void ) {
  pid_t vpid;
  int status;
  struct viewer_s *pv;

  do {
    if( ! ( vpid = waitpid( -1, & status, WNOHANG ) ) )
      break;

    if( vpid == -1 )      /*  this should never occur, but anyway...  */
      die( "ml2vim: waitpid() failed. errno = %d. This is a fatal error.\n", errno );

    trace( "check_for_dead_childs: got vpid: %d", vpid );
    sleep( 1 );

    for( pv = viewers; pv; pv = pv->next )
      if( vpid == pv->pid ) {
        unlink_viewer( pv );
        break;
      }

  } while( TRUE );
}

/*
 *  launch a viewer named name, executing cmdline.
 *  if an obsolete viewer of this name already exists, it is killed and
 *  removed from the list of viewers.
 *  a pointer to the new viewer is returned, or NULL on error.
 */
struct viewer_s *launch_or_relaunch_viewer( 
  char *name
, long int mma_tag_num
, char *cmdline 
) {
  struct viewer_s *pv;

  trace( "launch_or_relaunch_viewer: name: >>>%s<<<, cmdline: >>>%s<<<", name, cmdline );
  for( pv = viewers; pv; pv = pv->next )
    if( ! strcmp( name, pv->name ) ) {
      if( pv->mma_tag_num == mma_tag_num )
        return pv;   /*  this one is still up-to-date  */
      trace( "  found obsolete viewer, going to kill it..." );
      kill_viewer( pv );
      unlink_viewer( pv );
      break;
    }

  return launch_viewer( name, mma_tag_num, cmdline );
}


/*
 *    SECTION I:  asynchronous transfer to vim:
 *
 *     newline-terminated lines can be scheduled and will subsequently
 *     be delivered asynchronously to vim.
 */


/*  struct line_to_vim:
 *    used to build up a linked list of lines scheduled for asynchronous delivery
 */
struct line_to_vim {
  char *buf;
  char *bp;
  char *be;
  struct line_to_vim *next;
} * next_to_vim   /* first block in linked list (next to be delivered) */
, * last_to_vim   /* last block in linked list */
, * free_to_vim;  /* pointer to list of free blocks */
                  /* FIXME:  introduce free-list for other data structures? */

/* schedule_line_to_vim:
 *
 *  Schedules a line for asynchronous delivery to vim.
 *  The line may contain any data but should be ASCII and terminated by
 *  a single newline. No other newlines should be contained.
 *  No NUL should be present in the data.
 *
 *  If do_copy, this function will malloc()ate memory and copy the line.
 *  Otherwise, it will assume that line had been malloc()ated; it will
 *  just store the pointer and will free() line after delivery.
 */
void schedule_line_to_vim( 
  char *line
, char *lineend
, int do_copy 
) {
  struct line_to_vim * p;

  if( do_copy ) {
    char * newline = (char *)safe_malloc( lineend - line );
    memcpy( newline, line, lineend - line );
    line = newline;
  }
  if( free_to_vim ) {  /*  check whether we have any already allocated blocks  */
    p = free_to_vim;
    free_to_vim = p->next;
  } else {
    p = (struct line_to_vim *)safe_malloc( sizeof( struct line_to_vim ) );
  }
  p->next = NULL;
  p->bp = p->buf = line;
  p->be = lineend;
  if( last_to_vim ) {
    last_to_vim->next = p;
    last_to_vim = p;
  } else {
    next_to_vim = last_to_vim = p;
  }
}


/* deliver_line_to_vim
 *   This will attempt to write() to vim as much as possible of the the line
 *   pointed to by next_to_vim. 
 *   NOTE:
 *   - This function assumes that a line is actually scheduled for delivery;
 *     the caller has to check this.
 *   - The called should select() ASYNC_OUT for writing before calling this
 *     function or else it may block.
 */
void deliver_line_to_vim( void ) {
  ssize_t count;
  struct line_to_vim * p;

  trace( "deliver_line_to_vim: start" );
  errno = 0;
  count = write( ASYNC_OUT, next_to_vim->bp, next_to_vim->be - next_to_vim->bp );
  if( count > 0 ) {
    if( (next_to_vim->bp += count) >= next_to_vim->be ) {
      free( next_to_vim->buf );
      if( next_to_vim == last_to_vim )
        last_to_vim = NULL;
      p = next_to_vim;
      next_to_vim = next_to_vim->next;
      p->next = free_to_vim;
      free_to_vim = p;
    }
  }
  switch( errno ) {
    case EAGAIN:  /*  not an error for pipes  */
    case EINTR:   /*  call was interrupted: we just retry later  */
      errno = 0;  /*  and fall through  */
    case 0:
      return;
    default:
      die( "deliver_line_to_vim: write() failed", errno );
  }
  trace( "deliver_line_to_vim: end:  count = %I", sizeof(ssize_t), count );
}

/*
 *  write_line_to_wim:
 *    A single line will be transfered synchronously (to vim?) via a given
 *    descriptor. If the listener on this descriptor is indeed vim, the line
 *    should be terminated by a newline and contain no other newlines.
 *    No terminating NUL must be present.
 *    This function will block until all data has been transfered.
 */
void write_line_to_vim(
  int  fd
, char *line
, char *lend
) {
  ssize_t to_write;
  ssize_t written;

  trace( "write_line_to_vim: start" );
  to_write = lend - line;
  while( to_write > 0 ) {
    errno = 0;
    written = write( fd, line, to_write );
    if( written < 0 || errno )
      die( "write_line_to_vim: write() failed", errno );
    line += written, to_write -= written;
  }
  trace( "write_line_to_vim: end" );
}


/*
 *
 *SECTION II:  transfer to Mathematica
 *
 */

/*
 *  The following variables ensure synchronization:
 *
 *  we can be in one out of two different states, indicated by the flag  */

int waiting_for_mma_reply;

/*  - If unset, we can send a new input packet to mma for evaluation and set 
 *    the flag.
 *  - If set, don't send more input, but we will receive everything mma
 *    sends to us. If we get a new INPUTNAMEPKT, which contains mma's prompt
 *    for more input, we reset the flag.
 *  Note that immediately after the mathlink connection to a fresh mma kernel is
 *  established, the kernel will send the first input prompt "In[1]".
 *  The flag is therefore initialized to TRUE to indicate that we are waiting for
 *  just this first prompt.
 *
 *  Whenever an INPUTNAMEPKT is received from mma, we store the number in  */

long int current_mma_in_prompt;

/*  so we can use it as the mma's "official" tag number in all lines returned to vim
 *  in response to the following input.
 *
 *  In the variable  */

long int current_vim_tag_num;

/*  we note vims tag number of the input lines. this number is also included in all
 *  lines returned to vim, so that vim can insert them in the appropriate position.
 *
 *  It is possible to submit input to mma which will not create any output at all;
 *  if this is the case, we will create a dummy output packet to indicate that the
 *  computation is finished. The following flag indicates whether we have to do this:
 */
int need_dummy_output;


/*
 *  struct block_to_mma:
 *    Structure to collect an input block for mma, which will be transfered as an
 *    argument of EnterTextPacket.
 *    The packet will be NUL-terminated before shipping.
 *    We keep the block around while the evaluation is going on so we have the
 *    options, which are part of the block, available when the results arrive.
 */
struct block_to_mma {
  char *buf;
  char *bp;
  char *be;
  struct option_s options;
  long int vim_tag_num;   /*  this is to be able to identify the return lines  */
  struct block_to_mma *next;
} * btm_in_work   /*  to collect input lines from vim until we have a complete block  */
, * btm_in_eval   /*  the block which is just beeing evaluated by mma  */
, * next_to_mma   /*  top of queue of blocks scheduled for delivery  */
, * last_to_mma;

void free_btm( struct block_to_mma *pb ) {
  free( pb->buf );
  free( pb );
  destruct_options( &(pb->options) );
}

/*
 *  Some functions to query option settings. They will return local
 *  options, if available, or the global setting otherwise:
 */

int get_option_allow_blank( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).allow_blank;
  else
    return global_options.allow_blank;
}

/*
 * int get_option_display_output( void ) {
 *   if( btm_in_eval )
 *     return (btm_in_eval->options).display_output;
 *   else
 *     return global_options.display_output;
 * }
 */

int get_option_piggyback_single_line_output( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).piggyback_single_line_output;
  else
    return global_options.piggyback_single_line_output;
}

int get_option_refresh( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).refresh;
  else
    return global_options.refresh;
}

int get_option_suppress_output( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).suppress_output;
  else
    return global_options.suppress_output;
}

int get_option_suppress_text( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).suppress_text;
  else
    return global_options.suppress_text;
}

int get_option_suppress_display( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).suppress_display;
  else
    return global_options.suppress_display;
}

int get_option_suppress_message( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).suppress_message;
  else
    return global_options.suppress_message;
}

int get_option_suppress_dummy( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).suppress_dummy;
  else
    return global_options.suppress_dummy;
}

char * get_option_viewer_name( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).viewer_name;
  else
    return global_options.viewer_name;
}

char * get_option_viewer_command( void ) {
  if( btm_in_eval )
    return (btm_in_eval->options).viewer_command;
  else
    return global_options.viewer_command;
}

/*
 *  append_to_current_block_to_mma:
 *    Append a line to the current block which will later be scheduled for
 *    shipment to mma. A new block will be allocated if none exists.
 *
 *    No terminating character will be appended by this function, but
 *    the buffer will always be allocated sufficiently large to take up
 *    one additional character, so that NUL can be appended just before
 *    shipment.
 *    The input line will be copied; there is no termination required.
 */
void append_to_current_block_to_mma( char *line, char *lend ) {
  if( ! btm_in_work ) {
    btm_in_work = (struct block_to_mma *)safe_malloc( sizeof( struct block_to_mma ) );
    btm_in_work->next = NULL;
    btm_in_work->bp = btm_in_work->buf = (char *)safe_malloc( (lend - line) + 1 );
    btm_in_work->be = btm_in_work->buf + (lend - line) + 1;
    initialize_options( &(btm_in_work->options) );
  }
  if( lend - line >= btm_in_work->be - btm_in_work->bp ) {
    size_t newsize = (lend - line) + (btm_in_work->bp - btm_in_work->buf) + 1;
    char *newp = (char *)realloc( btm_in_work->buf, newsize );
    if( ! newp )
      die( "append_to_current_block_to_mma: realloc() failed", 0 );
    btm_in_work->bp += (newp - btm_in_work->buf);
    btm_in_work->buf = newp;
    btm_in_work->be = newp + newsize;
  }
  memcpy( btm_in_work->bp, line, (lend - line) );
  btm_in_work->bp += (lend - line);
}

/*
 *  schedule_block_to_mma:
 *    This function schedules a block for delivery via mathlink to the
 *    mathematica kernel. It assumes that the block has been malloc()ated
 *    and will not be modified by other functions in the future. The block
 *    will be free()d after the evaluation is finished.
 */
void schedule_block_to_mma( struct block_to_mma *block ) {
  trace( "schedule_block_to_mma: start:  block: %p   buf: %p   next: %p", block, block->buf, block->next );
  *(block->bp) = NUL;   /*  space for this is guaranteed to exist  */
  trace( "  payload: %s", block->buf );
  if( last_to_mma ) {   /*  the queue is non-empty  */
    last_to_mma->next = block;
    last_to_mma = block;
  } else {
    next_to_mma = last_to_mma = block;
  }
  trace( "schedule_block_to_mma: end" );
}

/*  write_mlink:
 *  write a block of data to the mma kernel for evaluation.
 *  this function will send the complete block and flush the link, so it 
 *  can block if mma is not ready to accept the data. however, since we
 *  always wait for the input prompt before sending new input, we are
 *  probably on the safe side.
 */
void write_mlink( void ) {
  struct block_to_mma *p;
  int rv;

  p = next_to_mma;
  trace( "write_mlink: start:  p: %p  next: %p  buf = %p >>%s<<<", p, p->next, p->buf, p->buf );
  rv = MLPutFunction( mlink, "EnterTextPacket", 1 );
  trace( "  MLPutFunction says %d", rv );
  rv = MLPutByteString( mlink, (unsigned char *)next_to_mma->buf, next_to_mma->bp - next_to_mma->buf );
  trace( "  MLPutByteString says %d", rv );
  rv = MLEndPacket( mlink );
  trace( "  MLEndPacket says %d", rv );

  /*  Now, the link *must* be flushed explicitely;  */
  /*  this was well hidden in Wolfram's docu!       */
  rv = MLFlush( mlink );
  trace( "  MLFlush says %d", rv );
  current_vim_tag_num = next_to_mma->vim_tag_num;
  waiting_for_mma_reply = TRUE;
  need_dummy_output = TRUE;
  if( next_to_mma == last_to_mma )
    next_to_mma = last_to_mma = NULL;
  else
    next_to_mma = next_to_mma->next;
  btm_in_eval = p;
  trace( "write_mlink: end" );
}



/*
 *
 *  SECTION III: reading from vim
 *
 */


void handle_input_line_from_vim(
  char * line
, char * lend
) {
  char *rbracket;
  long int this_num;

  sscanf( line, "i[%ld]", &this_num );
  rbracket = strchr( line, ']' );

  if( this_num > 0 )
    die( "handle_input_line_from_vim: input line has positive tag number", (int)this_num );

  if( btm_in_work )
    if( this_num != btm_in_work->vim_tag_num ) {
      schedule_block_to_mma( btm_in_work );
      btm_in_work = NULL;
    }

  if( lend <= rbracket + 1 ) return;     /*  line was empty except for tag  */
  rbracket[1] = ' '; /* make sure there is whitespace if input lines are joined */
  append_to_current_block_to_mma( rbracket + 1, lend );
  btm_in_work->vim_tag_num = this_num;
}


/*
 * parse_possibly_quoted_string:
 *   parse a string pointed to by *pline.
 *   the string can be
 *    - either any sequence of nonblank characters, ended by any whitespace or NUL,
 *    - or quoted, ie starting and ending with a `"'
 *   a backslash escapes any following char or introduces a octal escape sequence.
 *   NUL cannot be escaped (but can be created from a octal escape sequence).
 *   The resulting string will be copied to a NUL-terminated mallocated buffer, and
 *   pointer to the buffer is returned.
 *   The reference parameter pline will point to the first non-parsed character, which
 *   is probably whitespace or NUL, the length of the string is returned in plen.
 */
char * parse_possibly_quoted_string( char **pline, size_t *plen ) {
  int pass;
  size_t len;
  int is_quoted;
  int is_escaped;
  char *s;
  char *rv = NULL;
  unsigned char c;

  if( ( is_quoted = ( **pline == '"' ) ) )
    ++ *pline;

  /*  pass 1: count length          */
  /*  pass 2: copy to return value  */
  for( pass = 1; pass <= 2; ++pass ) {
    s = *pline;
    len = 0;
    do {
      if( ( is_escaped = ( *s == '\\' ) ) ) ++s;
      if( ! * s ) break;

      if( is_escaped ) {

        if( isdigit( *s ) ) {
          c = (unsigned char)*(s++) - (unsigned char)'0';
          if( isdigit( *s ) ) {
            c = c * 8 + (unsigned char)*(s++) - (unsigned char)'0';
            if( isdigit( *s ) )
              c = c * 8 + (unsigned char)*(s++) - (unsigned char)'0';
          }
          if( pass == 2 ) rv[ len ] = (char)c;
          ++len;
        } else {
          if( pass == 2 ) rv[ len ] = *s;
          ++len, ++s;
        }

      } else {

        if( is_quoted ? (*s == '"') : isspace( *s ) ) break;
        if( pass == 2 ) rv [ len ] = *s;
        ++len, ++s;

      }
    } while( TRUE );
    if( pass == 1 )
      rv = safe_malloc( len + 1 );
    else
      rv[ len ] = NUL;
  }
  if( *s ) ++s;
  *pline = s;
  *plen = len;
  return rv;
}


char * handle_viewer_option( char * s, int is_global ) {
  char * comma;
  char * command;
  int cmd_referenced;
  size_t len;

  trace( "handle_viewer_option: >>>%s<<<", s );
  if( *s != '=' ) {
    ufprintf( stderr, "Parse error in `viewer'-option: missing `='. Remainder of line is discarded.\n", *s ) 
    , fflush( stderr );
    trace( "  parse error: missing `='" );
    return NULL;
  }
  if( ! ( comma = strchr( ++s, ',' ) ) ) {
    ufprintf( stderr, "Parse error in `viewer'-option: missing `,'. Remainder of line is discarded.\n", *s ) 
    , fflush( stderr );
    trace( "  parse error: missing `,'" );
    return NULL;
  }
  if( is_global ) {
    if( global_options.viewer_name )
      safe_free( (void **) & (global_options.viewer_name) );
    memcpy( 
      global_options.viewer_name = (char *)safe_malloc( (comma - s) + 1 )
    , s
    , comma - s 
    ) , global_options.viewer_name[ comma - s ] = NUL;
  }
  if( btm_in_work ) {
    if( (btm_in_work->options).viewer_name )
      safe_free( (void **) & ( (btm_in_work->options).viewer_name ) );
    memcpy( 
      (btm_in_work->options).viewer_name = (char *)safe_malloc( (comma - s) + 1 )
    , s
    , comma - s 
    ) , (btm_in_work->options).viewer_name[ comma - s ] = NUL;
  }

  s = comma + 1;
  command = parse_possibly_quoted_string( &s, &len );
  cmd_referenced = NO;
  if( is_global ) {
    if( global_options.viewer_command )
      free( & (global_options.viewer_command) );
    global_options.viewer_command = command, cmd_referenced = YES;
  }
  if( btm_in_work ) {
    if( (btm_in_work->options).viewer_command )
      free( (btm_in_work->options).viewer_command );
    if( cmd_referenced ) {
      memcpy(
        (btm_in_work->options).viewer_command = (char *)safe_malloc( len + 1 )
      , command
      , len
      ), (btm_in_work->options).viewer_command [ len ] = NUL;
    } else {
      (btm_in_work->options).viewer_command = command, cmd_referenced = YES;
    }
  }
  trace( "viewer command set: >>>%s<<<", command );
  if( ! cmd_referenced ) free( command );

  return s;
}


/*
 * handle_short_option:
 *  Parses and handles a short option.
 *  The argument must be NUL-terminated.
 *  The argument is read up to the end of the option, and the pointer
 *  to the following character is returned.
 *  This may be nonblank and non-NUL, which is taken
 *  to be another short option.
 */
char * handle_short_option( char *s, int is_global ) {
  int inverse = FALSE;

  if( *s == '/' )
    inverse = !inverse, ++s;

  if( *s == 'b' ) {

    if( is_global )
      global_options.allow_blank = ! inverse;
    if( btm_in_work )
      (btm_in_work->options).allow_blank = ! inverse;
    ++s;

/*
 *   } else if( *s == 'd' ) {
 * 
 *     if( is_global )
 *       global_options.display_output = ! inverse;
 *     if( btm_in_work )
 *       (btm_in_work->options).display_output = ! inverse;
 *     ++s;
 */

  } else if( *s == '0' ) {

    /*  this is an explicit NOP option, having no effect at all  */
    ++s;

  } else if( *s == 'p' ) {

    if( is_global )
      global_options.piggyback_single_line_output = ! inverse;
    if( btm_in_work )
      (btm_in_work->options).piggyback_single_line_output = ! inverse;
    ++s;

  } else if( *s == 'r' ) {

    enum refresh_option nr;
    switch( * ++s ) {
      case 'i': nr = IMMEDIATE; break;
      case 'b': nr = BLOCK; break;
      case 'c': nr = CELL; break;
      case 'l': nr = LAZY; break;
      default:
        ufprintf( stderr, "unkown refresh option: %c\n", *s );
        fflush( stderr );
        nr = IMMEDIATE;
        break;
    }
    if( is_global )
      global_options.refresh = nr;
    if( btm_in_work )
      (btm_in_work->options).refresh = nr;
    ++s;

  } else if( *s == 's' ) {

    int fallthrough = FALSE;
    switch( * ++s ) {
      case 'a':
        fallthrough = TRUE;
      case 'o':
        if( is_global )
          global_options.suppress_output = ! inverse;
        if( btm_in_work )
          (btm_in_work->options).suppress_output = ! inverse;
        if( ! fallthrough )
          break;
      case 't':
        if( is_global )
          global_options.suppress_text = ! inverse;
        if( btm_in_work )
          (btm_in_work->options).suppress_text = ! inverse;
        if( ! fallthrough )
          break;
      case 'd':
        if( is_global )
          global_options.suppress_display = ! inverse;
        if( btm_in_work )
          (btm_in_work->options).suppress_display = ! inverse;
        if( ! fallthrough )
          break;
      case 'm':
        if( is_global )
          global_options.suppress_message = ! inverse;
        if( btm_in_work )
          (btm_in_work->options).suppress_message = ! inverse;
        if( ! fallthrough )
          break;
      case 'x':
        if( is_global )
          global_options.suppress_dummy = ! inverse;
        if( btm_in_work )
          (btm_in_work->options).suppress_dummy = ! inverse;
        break;
      default:
        ufprintf( stderr, "unkown suppress option: %c\n", *s ), fflush( stderr );
    }
    ++s;

  } else if( *s == 'v' ) {

    s = handle_viewer_option( ++s, is_global );

  } else {

    ufprintf( stderr, "unknown short option: %c\n", *s );
    fflush( stderr );
    ++s;

  }
  return s;
}

/*
 * handle_long_option:
 *
 *
 */
char * handle_long_option( char *s, int is_global ) {
  int inverse = FALSE;

  if( *s == '/' )
    inverse = !inverse, ++s;

  if( s[0] == 'n' )
    if( s[1] == 'o' )
      inverse = !inverse, s += 2;

  if( ! strncmp( s, "blank", 5 ) ) {
    s += 5;

    if( is_global )
      global_options.allow_blank = ! inverse;
    if( btm_in_work )
      (btm_in_work->options).allow_blank = ! inverse;

/*
 *   } else if( ! strncmp( s, "display", 7 ) ) {
 *     s += 7;
 * 
 *     if( is_global )
 *       global_options.display_output = ! inverse;
 *     if( btm_in_work )
 *       (btm_in_work->options).display_output = ! inverse;
 */

  } else if( ! strncmp( s, "piggyback", 9 ) ) {
    s += 9;

    if( is_global )
      global_options.piggyback_single_line_output = ! inverse;
    if( btm_in_work )
      (btm_in_work->options).piggyback_single_line_output = ! inverse;

  } else if( ! strncmp( s, "refresh", 7 ) ) {
    enum refresh_option nr;

    s += 7;

    if( *(s++) != '=' ) {
      ufprintf( stderr, "handle_long_option: missing `='\n" );
      fflush( stderr );
      return s;
    }
    if( ! strncmp( s, "immediate", 9 ) )
      s += 9, nr = IMMEDIATE;
    else if( ! strncmp( s, "block", 5 ) )
      s += 5, nr = BLOCK;
    else if( ! strncmp( s, "cell", 4 ) )
      s += 4, nr = CELL;
    else if( ! strncmp( s, "lazy", 4 ) )
      s += 4, nr = LAZY;
    else {
      ufprintf( stderr, "handle_long_option: unknown refresh option: >>>%\\s<<<\n", NULL, s );
      fflush( stderr );
      while( isalnum( *s ) )
        ++s;
      return s;
    }

    if( is_global )
      global_options.refresh = nr;
    if( btm_in_work )
      (btm_in_work->options).refresh = nr;

  } else if( ! strncmp( s, "suppress", 8 ) ) {
    s += 8;

    if( *(s++) != '=' ) {
      ufprintf( stderr, "handle_long_option: missing `='\n" );
      fflush( stderr );
      return s;
    }
    while( TRUE ) {
      if( ! strncmp( s, "all", 3 ) ) {
        s += 3;
        if( is_global ) global_options.suppress_output = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_output = ! inverse;
        if( is_global ) global_options.suppress_text = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_text = ! inverse;
        if( is_global ) global_options.suppress_display = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_display = ! inverse;
        if( is_global ) global_options.suppress_message = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_message = ! inverse;
        if( is_global ) global_options.suppress_dummy = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_dummy = ! inverse;
      } else if( ! strncmp( s, "output", 6 ) ) {
        s += 6;
        if( is_global ) global_options.suppress_output = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_output = ! inverse;
      } else if( ! strncmp( s, "text", 4 ) ) {
        s += 4;
        if( is_global ) global_options.suppress_text = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_text = ! inverse;
      } else if( ! strncmp( s, "display", 7 ) ) {
        s += 7;
        if( is_global ) global_options.suppress_display = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_display = ! inverse;
      } else if( ! strncmp( s, "message", 7 ) ) {
        s += 7;
        if( is_global ) global_options.suppress_message = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_message = ! inverse;
      } else if( ! strncmp( s, "dummy", 5 ) ) {
        s += 5;
        if( is_global ) global_options.suppress_dummy = ! inverse;
        if( btm_in_work ) (btm_in_work->options).suppress_dummy = ! inverse;
      } else {
        ufprintf( stderr, "handle_long_option: unknown suppress option: >>>%\\s<<<\n", NULL, s );
        fflush( stderr );
        while( isalnum( *s ) )
          ++s;
        return s;
      }

      if( *s != ',' )
        break;
      else
        ++s;
    }

  } else if( ! strncmp( s, "viewer", 6 ) ) {
    s += 6;

    s = handle_viewer_option( s, is_global );

  } else if( ! strncmp( s, "0", 1 ) ) {
    ++s;

  } else {

    ufprintf( stderr, "unknown long option: >>>%\\s<<<\n", NULL, s );
    fflush( stderr );
    while( isalnum( *s ) )
      ++s;

  }
  return s;
}


/*
 *  handle_option_line_from_vim:
 *  This function deals with option lines, whose first non-blank character
 *  is a dash. Option lines may contain global options, which have a bang
 *  preceeding their name, and local options, which have no preceeding bang.
 *  Local options must immediately follow a input block which has not yet been
 *  scheduled for delivery to Mma.
 */
void handle_option_line_from_vim(
  char * line
) {
  char *newline;
  int is_global;
  int is_long;

  while( *line ) {
    if( *line != '-' ) {
      ufprintf( stderr, "parse error in option line: missing dash\n" );
      fflush( stderr );
      return;
    }
    ++line;

    if( *line == '-' )
      is_long = TRUE, ++line;
    else
      is_long = FALSE;

    if( *line == '!' )
      is_global = TRUE, ++line;
    else
      is_global = FALSE;

    if( ! ( is_global || btm_in_work ) ) {
      ufprintf( stderr, "only global options allowed here" );
      fflush( stderr );
      return;
    }

    /*
     *  newline should now be greater than line and point to either
     *   - the terminating NUL of the option line, or
     *   - a white space following the option
     */

    while( *line && ! isspace( *line ) ) {

      if( is_long ) {
        newline = handle_long_option( line, is_global );
      } else {
        newline = handle_short_option( line, is_global );
      }

      if( ! newline ) {
        /*  an error occurred, we discard whatever might remain of this line:  */
        return;
      } else if( newline > line ) {
        line = newline;
      } else {
        ufprintf( stderr, "Strange parse error in option line: option of length zero encountered.\n" );
        fflush( stderr );
        return;
      }

    }
    while( isspace( *line ) )
      ++line;
  }
}


/*
 *  handle_line_from_vim
 *    A iline received from vim will be parsed and handled.
 *    The buffer may be messed with but will not be free()d.
 *    The buffer must be NUL-terminated.
 *
 *  This function itself will just determine the line type and call the
 *  appropriate handler. Currently, the following are recognized:
 *    - input lines, which contain an input tag
 *    - option lines, introduced by a dash
 *    - lines containing exactly the string "SUBMIT", indicating that
 *      the current input block is complete and can be submitted to mma.
 *    - some other lines containing special commands (see below)
 *  All other lines are treated as an error.
 */
void handle_line_from_vim( 
  char * line
, char * lend
) {
  trace( "handle_line_from_vim: start: line = %s", line );

  if( *lend != NUL ) {
    ufprintf( stderr, "error in option line: missing terminating NUL\n" );
    fflush( stderr );
    return;
  }

  while( isspace( *line ) )
    ++line;

  if( *line == '-' ) {

    handle_option_line_from_vim( line);

  } else if (
       (lend - line >= 4)
    && (line[0] == 'i')
    && (line[1] == '[')
    && ( strchr( line, ']' ) >= line + 3 )
  ) {

    handle_input_line_from_vim( line, lend );

  } else if( ! strcmp( line, "SUBMIT" ) ) {

    if( btm_in_work ) {
      schedule_block_to_mma( btm_in_work );
      btm_in_work = NULL;
    }

  } else if( ! strcmp( line, "ABORT" ) ) {

    MLPutMessage( mlink, MLAbortMessage );

  } else if( ! strcmp( line, "QUIT" ) ) {

    leave_mainloop = TRUE;

  } else if( ! strcmp( line, "INTERRUPT" ) ) {

    MLPutMessage( mlink, MLInterruptMessage );

  } else if( ! strcmp( line, "PURGE INPUT" ) ) {

    if( btm_in_work )
      free_btm( btm_in_work ), btm_in_work = NULL;

    while( (last_to_mma = next_to_mma) ) {
      next_to_mma = next_to_mma->next;
      free_btm( last_to_mma );
    }

  } else if( ! strcmp( line, "PURGE OUTPUT" ) ) {

    if( next_to_vim ) {

      trace( "purge output: start" );
      if( next_to_vim->bp > next_to_vim->buf )
        /*  this line has been partially transferred, we have  */
        /*  to terminate it properly:                          */
        if( ! swrite( ASYNC_OUT, "<<<further output (if any) purged!\n" ) ) {
          ufprintf( stderr, "write() failed while purging output" );
          fflush( stderr );
        }

      while( (last_to_vim = next_to_vim) ) {
        trace( "  purge output: next_to_vim = %p", next_to_vim );
        free( next_to_vim->buf );
        next_to_vim = next_to_vim->next;
        last_to_vim->next = free_to_vim;
        free_to_vim = last_to_vim;
      }
      trace( "purge output: done" );

    }

  } else if( ! strcmp( line, "?STATUS" ) ) {

    /*  FIXME:  issue status report here  */


  } else {
    ufprintf( stderr, "can't handle: >>>%\\s<<<\n", NULL, line );
    fflush( stderr );
  }

  trace( "handle_line_from_vim: end" );
}


/*
 *  buffer to collect incoming data from vim:
 */
char * from_vim_buf
   , * from_vim_bp
   , * from_vim_be;

/*
 *  read_from_vim:
 *    Read available data from stdin. This function should only be called
 *    after select(); otherwise, it may block.
 */
void read_from_vim( void ) {
  ssize_t rsize;
  char * newstart;
  char * newend;

  trace( "read_from_vim: start" );
  if( from_vim_be - from_vim_bp < BUFSIZE ) {
    char *newp = (char *)realloc( from_vim_buf, 2 * (from_vim_be - from_vim_buf) );
    if( !newp )
      die( "read_from_vim: realloc() failed", 0 );
    from_vim_bp = (from_vim_bp - from_vim_buf) + newp;
    from_vim_be = 2 * (from_vim_be - from_vim_buf) + newp;
    from_vim_buf = newp;
  }
  errno = 0;
  rsize = read( STDIN, from_vim_bp, from_vim_be - from_vim_bp );
  trace( "  rsize = %I", sizeof(ssize_t), rsize );
  if( rsize <= 0 )
    die( "read_from_vim(): read() failed", errno );
  if( rsize > 0 ) {
    newstart = from_vim_buf;
    newend = from_vim_bp + rsize;
    while( from_vim_bp < newend ) {
      if( *from_vim_bp == '\n' ) {
        *from_vim_bp = NUL;
        handle_line_from_vim( newstart, from_vim_bp );
        newstart = from_vim_bp + 1;
      }
      ++from_vim_bp;
    }
    if( newstart > from_vim_buf ) {
      char *pc;
      from_vim_bp -= (newstart - from_vim_buf);
      for( pc = from_vim_buf; newstart < newend; )
        *(pc++) = *( newstart++ );
    }
  }
  trace( "read_from_vim: end" );
}


/*
 *  handle_vim:
 *    (Attempt to) deliver pending packets asynchronously to vim and read 
 *    incoming data from stdin.
 */
void handle_vim( void ) {
  fd_set fd_read, fd_write;
  int n;
  struct timeval timeout;
  int rv;

  timeout.tv_sec = 0;
  timeout.tv_usec = 10000;   /*  delay a little to avoid busy waiting  */
  FD_ZERO( &fd_read );
  FD_ZERO( &fd_write );
  n = 0;
  FD_SET( STDIN, &fd_read );
  if( STDIN > n ) n = STDIN;

  if( next_to_vim ) {
    FD_SET( ASYNC_OUT, &fd_write );
    if( ASYNC_OUT > n ) n = ASYNC_OUT;
    rv = select( n+1, &fd_read, &fd_write, NULL, &timeout );
  } else {
    rv = select( n+1, &fd_read, NULL, NULL, &timeout );
  }
  if( rv == -1 ) {
    die( "handle_vim: select() failed", errno );
  }
  if( rv ) {
    if( next_to_vim )
      if( FD_ISSET( ASYNC_OUT, &fd_write ) )
        deliver_line_to_vim();
    if( FD_ISSET( STDIN, &fd_read ) )
      read_from_vim();
  }
}


/*
 *
 *  SECTION IV: reading from mma
 *
 *    This is essentially a switch (in read_mlink(), see below) which will
 *    read one complete packet from the link and call one of the following
 *    functions to handle the type of packet we received.
 */

void send_refresh_command_to_vim( void ) {
  char *outpacket = (char *)safe_malloc( MAX_TAG_LEN + 32 );
  usnprintf( 
    outpacket, MAX_TAG_LEN + 32
  , "M[%ld,%lu] REFRESH\n"
  , current_vim_tag_num, current_mma_in_prompt 
  );
  schedule_line_to_vim( outpacket, outpacket + strlen(outpacket), NO );
}


/* INPUTNAMEPKT:
 *   These are important, since they contain mma's prompt for more input.
 *   Here we also create a dummy output packet, if required.
 */
void ml_read_inputnamepacket( void ) {
  char *s;
  long int new_in_prompt;
  char *outpacket;

  trace( "ml_read_inputnamepacket: start" );
  MLGetString( mlink, (const char **)&s );
  if( !s ) 
    die( "ml_read_inputnamepacket: MLGetString() failed", 0 );
  if( sscanf( s, "In[%lu]", &new_in_prompt ) != 1 )
    die( "ml_read_inputnamepacket: sscanf() failed", 0 );
  trace( "ml_read_inputnamepacket: got number: %ld", new_in_prompt );
  MLDisownString( mlink, s );
  if( new_in_prompt < current_mma_in_prompt )
    die( "ml_read_inputnamepacket: input prompt reused", (int)new_in_prompt );

  if( need_dummy_output && ! get_option_suppress_dummy() ) {
    outpacket = (char *)safe_malloc( MAX_TAG_LEN + 32 );
    usnprintf( 
      outpacket, MAX_TAG_LEN + 32
    , "X[%ld,%lu/%c]  ---no output---\n"
    , current_vim_tag_num, current_mma_in_prompt 
    , get_option_piggyback_single_line_output() ? 'p' : '0'
    );
    schedule_line_to_vim( outpacket, outpacket + strlen(outpacket), NO );
    if( get_option_refresh() <= BLOCK )
      send_refresh_command_to_vim();
    need_dummy_output = FALSE;
  }

  if( current_mma_in_prompt >= 0 ) {
    /*  send message to vim: end of this input block:  */
    outpacket = (char *)safe_malloc( MAX_TAG_LEN + 32 );
    usnprintf( 
      outpacket, MAX_TAG_LEN + 32
    , "M[%ld,%lu] EOC\n"
    , current_vim_tag_num, current_mma_in_prompt 
    );
    schedule_line_to_vim( outpacket, outpacket + strlen(outpacket), NO );
    if( get_option_refresh() != LAZY )
        send_refresh_command_to_vim();
  }

  if( btm_in_eval ) {
    free( btm_in_eval->buf );
    free( btm_in_eval );
    btm_in_eval = NULL;
  }

  current_mma_in_prompt = new_in_prompt;
  waiting_for_mma_reply = FALSE;
  trace( "ml_read_inputnamepacket: end" );
}

/*
 * OUTPUTNAMEPKT:
 *   They contain the output prompt, like "Out[42]".
 *   We just use them to verify we are in sync, by comparing against
 *   the current_mma_in_prompt.
 */
void ml_read_outputnamepacket( void ) {
  const char *s;
  long int outnum;

  trace( "ml_read_outputnamepacket: start" );

  MLGetString( mlink, &s );
  if( !s )
    die( "ml_read_inputnamepacket: MLGetString() failed", 0 );
  if( sscanf( s, "Out[%lu]", &outnum ) != 1 )
    die( "ml_read_outputnnamepacket: sscanf() failed", 0 );
  trace( "ml_read_outputnamepacket: got number: %ld", outnum );
  if( outnum != current_mma_in_prompt )
    die( "ml_read_outputnamepacket: out of sync", (int)outnum );
  MLDisownString( mlink, s );
  trace( "ml_read_outputnamepacket: end" );
}

/*
 * RETURNTEXTPKT:
 *  The most important reply: they contain the actual results
 *  (in the standard interface: what is displayed after "Out[42]=").
 *  Payload of such a packet may also be piped to an external
 *  viewer program.
 */
void ml_read_returntextpacket( void ) {
  char *line, *payload, *ptmp;
  char *sstart,  *s1, *s2;
  int is_octal_newline;
  int do_display;
  int first_line;
  char *name, *command;
  struct viewer_s *pv;

  trace( "ml_read_returntextpacket: start" );

  MLGetString( mlink, (const char **)&sstart );

  if( ! sstart )
    die( "ml_read_returntextpacket: MLGetString() failed", '?' );

  trace( "  got %ld bytes from the link", (long int)strlen( sstart ) );

  do_display = ! get_option_suppress_output();
  if( ( name = get_option_viewer_name() ) && ( command = get_option_viewer_command() ) )
    pv = launch_or_relaunch_viewer( name, current_mma_in_prompt, command );
  else
    pv = NULL;

  if( do_display || pv ) {

    s1 = s2 = sstart;
    first_line = TRUE;
    while( *s1 ) {
      is_octal_newline = FALSE;
      while( *s2 && *s2 != '\n' ) {
        if( *s2 == '\\' )
          if( s2[1] == '0' )
            if( s2[2] == '1' )
              if( s2[3] == '2' ) {
                is_octal_newline = TRUE;
                if( pv ) pv = putc_to_viewer( '\n', pv );
                break;
              }
        if( pv ) pv = putc_to_viewer( *s2, pv );
        ++s2;
      }
      if( do_display ) {
        line = (char *)safe_malloc( s2 - s1 + MAX_TAG_LEN + 1 );
        usnprintf( line, MAX_TAG_LEN, "o[%ld,%lu/p] ", current_vim_tag_num, current_mma_in_prompt );
        for( payload = line; *payload; payload++ )
          ;
        ptmp = payload;
        while( s1 < s2 )
          *payload++ = *s1++;
        *payload++ = '\n';

        if(
               ! get_option_piggyback_single_line_output()
            || ! first_line
            || *s1    /*  ie: not the last line  */
        )
          *(strchr( line, 'p' )) = '0';

        if( ! get_option_allow_blank() ) {
          trace( "  blanks are disallowed, payload is: >>>%s<<<", ptmp );
          while( *ptmp != '\n' )
            if( ( *ptmp != ' ' ) && ( *ptmp != '\t' ) )
              break;
            else
              ++ptmp;
          if( *ptmp != '\n' ) {
            schedule_line_to_vim( line, payload, NO );
            if( get_option_refresh() == IMMEDIATE )
              send_refresh_command_to_vim();
            first_line = FALSE;
            need_dummy_output = FALSE;
          }
        } else {
          schedule_line_to_vim( line, payload, NO );
          if( get_option_refresh() == IMMEDIATE )
            send_refresh_command_to_vim();
          first_line = FALSE;
          need_dummy_output = FALSE;
        }
      }
      if( is_octal_newline )
        s2 += 4;
      else if( *s2 == '\n' )
        ++s2;
      s1 = s2;
    }
    if( get_option_refresh() == BLOCK )
      send_refresh_command_to_vim();

    MLDisownString( mlink, sstart );
  }
  if( pv )
    fflush( pv->outstream );

  trace( "ml_read_returntextpacket: end" );
}

/*
 * MESSAGEPKT:
 *   Contains error and warning messages.
 */
void ml_read_messagepacket( void ) {
  char *line, *payload;
  const char *func, *mtag;
  char *sstart, *s1, *s2;
  int len;
  int is_octal_newline;

  trace( "ml_read_messagepacket: start" );
  
  if( ! get_option_suppress_message() ) {

    MLGetSymbol( mlink, &func );
    if( ! func )
      die( "ml_read_messagepacket: MLGetSymbol() failed", 0 );
    MLGetString( mlink, &mtag );
    if( ! mtag )
      die( "ml_read_messagepacket: MLGetString() failed for message tag", 0 );
    len = strlen(func) + strlen(mtag) + MAX_TAG_LEN;
    line = (char *)safe_malloc( len + 1 );
    usnprintf( 
      line, len, "m[%ld,%lu] %s::%s:\n"
    , current_vim_tag_num, current_mma_in_prompt, func, mtag
    );
    schedule_line_to_vim( line, line + strlen(line), NO );
    if( get_option_refresh() == IMMEDIATE )
      send_refresh_command_to_vim();

    MLNewPacket( mlink );  /* the actual text is in a separate textpacket  */
    if( MLNextPacket( mlink ) != TEXTPKT )
      die( "ml_read_messagepacket: failed to receive a textpacket", 0 );
    MLGetString( mlink, (const char **)&sstart );
    if( ! sstart )
      die( "ml_read_messagepacket: MLGetByteString() failed for message text", 0 );

    s1 = s2 = sstart;
    while( *s1 ) {
      is_octal_newline = FALSE;
      while( *s2 && (*s2 != '\n') ) {
        if( *s2 == '\\' )
          if( s2[1] == '0' )
            if( s2[2] == '1' )
              if( s2[3] == '2' ) {
                is_octal_newline = TRUE;
                break;
              }
        s2++;
      }
      line = (char *)safe_malloc( s2 - s1 + MAX_TAG_LEN + 1 );
      usnprintf( line, MAX_TAG_LEN, "m[%ld,%lu] ", current_vim_tag_num, current_mma_in_prompt );
      for( payload = line; *payload; payload++ )
        ;
      while( s1 < s2 )
        *payload++ = *s1++;
      *payload++ = '\n';
      if( is_octal_newline )
        s2 += 4, s1 += 4;
      else if( *s2 == '\n' )
        ++s2, ++s1;
      schedule_line_to_vim( line, payload, NO );
      if( get_option_refresh() == IMMEDIATE )
        send_refresh_command_to_vim();
    }
    need_dummy_output = FALSE;

    if( get_option_refresh() == BLOCK )
      send_refresh_command_to_vim();

    MLDisownSymbol( mlink, func );
    MLDisownString( mlink, mtag );
    MLDisownString( mlink, sstart );

  }

  trace( "ml_read_messagepacket: end" );
}


/*
 * TEXTPKT:
 *   May be caused by mma's Print[]-function.
 */
void ml_read_textpacket( void ) {
  char *line, *payload;
  const char *sstart, *s1, *s2;
  int is_octal_newline;

  trace( "ml_read_textpacket: start" );

  if( ! get_option_suppress_text() ) {

    MLGetString( mlink, &sstart );
    if( ! sstart )
      die( "ml_read_textpacket: MLGetString() failed", 0 );
    s1 = s2 = sstart;
    while( *s1 ) {
      is_octal_newline = FALSE;
      while( *s2 && (*s2 != '\n') ) {
        if( *s2 == '\\' )
          if( s2[1] == '0' )
            if( s2[2] == '1' )
              if( s2[3] == '2' ) {
                is_octal_newline = TRUE;
                break;
              }
        s2++;
      }
      line = (char *)safe_malloc( s2 - s1 + MAX_TAG_LEN + 1 );
      usnprintf( line, MAX_TAG_LEN, "t[%ld,%lu] ", current_vim_tag_num, current_mma_in_prompt );
      for( payload = line; *payload; payload++ )
        ;
      while( s1 < s2 )
        *payload++ = *s1++;
      *payload++ = '\n';
      if( is_octal_newline )
        s2 += 4, s1 += 4;
      else if( *s2 == '\n' )
        ++s2, ++s1;
      schedule_line_to_vim( line, payload, NO );
      if( get_option_refresh() == IMMEDIATE )
        send_refresh_command_to_vim();
    }

    if( get_option_refresh() == BLOCK )
      send_refresh_command_to_vim();

    need_dummy_output = FALSE;
    MLDisownString( mlink, sstart );

  }

  trace( "ml_read_textpacket: end" );
}

/*
 * DISPLAYPKT:
 *   will be created by Plot[] etc. and by default contains pseudo-postscript.
 *   Use 
 *     DisplayFunction := DisplayString[ #, "EPS" ] &
 *   to get real PS.
 *   (To the people at Wolfram.com:  *Why* can't this produce real PS at default?????
 *    Real PS instead of MPS would have saved, up to now, several man-Millenia,
 *    and many Gb of usenet traffic!)
 *   Output of this packets tends to be lengthy and is rarely useful (you can,
 *   however, export it manually into a .EPS file).
 *   For automatic on-screen display, you should consider sending the postscript
 *   in a RETURNTEXTPKT and using a --viewer -option. You probably want to suppress 
 *   these packets globally by setting --!suppress-display.
 */
void ml_read_displaypacket( void ) {
  char *line, *payload;
  const char *sstart, *s1, *s2;
  int is_octal_newline;

  trace( "ml_read_displaypacket: start" );

  MLGetString( mlink, &sstart );
  if( ! sstart )
    die( "ml_read_displaypacket: MLGetString() failed", 0 );

  trace( "  got %ld bytes from the link", (long int)strlen( sstart ) );

  if( ! get_option_suppress_display() ) {

    s1 = s2 = sstart;
    while( *s1 ) {
      is_octal_newline = FALSE;

      while( *s2 && (*s2 != '\n') ) {
        if( *s2 == '\\' )
          if( s2[1] == '0' )
            if( s2[2] == '1' )
              if( s2[3] == '2' ) {
                is_octal_newline = TRUE;
                break;
              }
        s2++;
      }

      line = (char *)safe_malloc( s2 - s1 + MAX_TAG_LEN + 1 );
      usnprintf( line, MAX_TAG_LEN, "d[%ld,%lu] ", current_vim_tag_num, current_mma_in_prompt );
      for( payload = line; *payload; payload++ )
        ;
      while( s1 < s2 )
        *payload++ = *s1++;
      *payload++ = '\n';
      schedule_line_to_vim( line, payload, NO );
      if( get_option_refresh() == IMMEDIATE )
        send_refresh_command_to_vim();
      if( is_octal_newline )
        s2 += 4;
      else if( *s2 == '\n' )
        ++s2;
      s1 = s2;
    }
    if( get_option_refresh() == BLOCK ) {
      send_refresh_command_to_vim();
      need_dummy_output = FALSE;
    }
  }

  MLDisownString( mlink, sstart );
  trace( "ml_read_displaypacket: end" );
}

/*
 *  read_mlink:
 *    This will read a complete packet from mathlink.
 *    for every packet type we receive, a suitable handler will be called.
 */
void read_mlink( void ) {
  int ptype;
  trace( "read_mlink: start" );
  ptype = MLNextPacket( mlink );
  trace( "read_mlink: packet type is %d", ptype );
  switch( ptype ) {
    case INPUTNAMEPKT:
      ml_read_inputnamepacket();
      break;
    case OUTPUTNAMEPKT:
      ml_read_outputnamepacket();
      break;
    case RETURNTEXTPKT:
      ml_read_returntextpacket();
      break;
    case MESSAGEPKT:
      ml_read_messagepacket();
      break;
    case TEXTPKT:
      ml_read_textpacket();
      break;
    case DISPLAYPKT:
      ml_read_displaypacket();
      break;
    case ILLEGALPKT:   /*  we get one of these when the kernel dies  */
      die( "read_mlink: ILLEGALPKT received (is the kernel dead?)", 0 );
    default:
      break;  /*  unkown type, we just discard it (FIXME: do more here?) */
  }
  /* the current packet should be empty by now, so we discard whatever might be left: */
  MLNewPacket( mlink );
  trace( "read_mlink: end" );
}


/*
 *  handle_mathlink:
 *    This function will look for and call appropriate handlers for any pending 
 *    packets incoming from and outgoing to Mma.
 */
void handle_mathlink( void ) {
  int rv;
  /*
   * trace( "handle_mathlink: start" );
   * if( waiting_for_mma_reply )
   *   trace( "  still waiting for reply to input %ld", current_mma_in_prompt );
   * else
   *   trace( "  ready to ship another block via mathlink. next_to_mma = %p", next_to_mma );
   */

  while( (rv = MLReady( mlink )) ) {
    if( rv < 0 )   /* error, probably broken link */
      die( "handle_mathlink: MLReady returns error:", rv );
    read_mlink();
  }
  while( next_to_mma && ! waiting_for_mma_reply )
    write_mlink();
}


/*
 *  launch_kernel:
 *    Launches the Mma kernel and establishes a mathlink connection.
 *    FIXME:  Currently, most options are hardwired.
 *            This should be more flexible.
 */
void launch_kernel( void ) {
  int mma_argc;
  char *mma_argv[7];
  int rv;

  trace( "launch_kernel: start" );
  mma_argc = 4;
  mma_argv[0] = "ml2vim";
  mma_argv[1] = "-linkname";
  if( ! ( mma_argv[2] = getenv( "MMA_COMMAND" ) ) ) 
    mma_argv[2] = MMA_COMMAND;
  mma_argv[3] = "-linklaunch";
  mma_argv[4] = "-LinkProtocol";
  mma_argv[5] = "pipes";
  mma_argv[6] = NULL;
  if( ! ( mlink = MLOpenArgv( mlenv, mma_argv, mma_argv + 5, mlerr ) ) )
    die( "launch_kernel: MLOpenArgv() failed", 0 );
  if( ! ( rv = MLActivate( mlink ) ) )
    die( "launch_kernel:  MLActivate() failed", 0 );
  trace( "launch_kernel: end: MLActivate says %d", rv );
}


void sighandler( int sig ) {
  switch( sig ) {
    case SIGCHLD:
      got_sigchld = TRUE;
      signal( SIGCHLD, sighandler );
      break;
    case SIGTERM:
      leave_mainloop = TRUE;
      signal( SIGTERM, sighandler );
      break;
    case SIGHUP:
      leave_mainloop = TRUE;
      signal( SIGHUP, sighandler );
      break;
    case SIGINT:
      leave_mainloop = TRUE;
      signal( SIGINT, sighandler );
      break;
    case SIGQUIT:
      exit(1);
      break;
    default:
      break;
  }
}

#if HAVE_GETOPT_LONG
  enum cmd_option_vals {
    CMD_VERSION = 1000, CMD_DEBUG, CMD_HELP
  };

  struct option cmd_options[] = {
    { "version" , NO , NULL , CMD_VERSION }
  , { "debug" , NO , NULL , CMD_DEBUG }
  , { "help" , NO , NULL , CMD_HELP }
  , { NULL, 0, NULL, 0 }
  };
#else
  /*  An awful kludge for those who don't have Gnu's getopt_long().  */
  /*  Will work as long as we have only a small set of long options  */
#  define CMD_VERSION 1000
#  define CMD_DEBUG   1001
#  define CMD_HELP    1002
  void *cmd_options;
  int getopt_long( int argc, char *argv[], char *dummy0, void *dummy1, void *dummy2 ) {
    static int index = 0;
    do {
      if( ++index >= argc ) return EOF;
      if( *argv[index] != '-' )
        continue;
      if( ! strncmp( argv[index], "--version", 9 ) )
        return CMD_VERSION;
      else if( ! strncmp( argv[index], "--debug", 7 ) )
        return CMD_DEBUG;
      else if( ! strncmp( argv[index], "--help", 6 ) )
        return CMD_HELP;
      else
        return '?';
    } while( TRUE );
  }
#endif

int main( int argc, char *(argv[]) ) {

  int show_version = FALSE;
  int show_help = FALSE;
  pid_t ppid;
  int rval = 0;
  int option;

  ppid = getppid();

  do {
    switch( ( option = getopt_long( argc, argv, "", cmd_options, NULL ) ) ) {
      case CMD_VERSION: 
        show_version = TRUE; 
        break;
      case CMD_DEBUG:
        debug = TRUE; 
        break;
      case CMD_HELP:
        show_version = TRUE; 
        show_help = TRUE; 
        break;
      case EOF:
        break;
      case '?':
      default:
        ufprintf( stderr, "\n%s: illegal command line argument.\n", PROGRAM );
        rval = 1;
        show_version = TRUE; 
        show_help = TRUE;
        break;
    }
  } until( option == EOF );

  if( show_version ) {
    ufprintf( stderr, "\n\nml2vim Version %s  (C) 1999 Timo Felbinger", VERSION );
    ufprintf( 
      stderr, "\n\nCompiled by %s on %s at %sutc."
    , COMPILED_BY, COMPILED_ON, TIME_OF_COMPILATION 
    );
    ufprintf( stderr, "\n\nThis program is free software and you are welcome to redistribute it under the" );
    ufprintf( stderr, "\nGNU public license, either version 2 or, at your option, any later version." );
    ufprintf( stderr, "\nYou should have received a copy of the GNU General Public License along with" );
    ufprintf( stderr, "\nthis program (in a file called LICENSE); if not, write to the Free Software" );
    ufprintf( stderr, "\nFoundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA, or" );
    ufprintf( stderr, "\nvisit http://www.gnu.org." );
    fflush( stderr );
    if( ! show_help ) exit( rval );
  }

  if( ! show_help ) {
    /*  make the asynchronous output to vim nonblocking  */
    if( fcntl( ASYNC_OUT, F_SETFL, O_NONBLOCK ) == -1 ) {
      /*  EBADF is a typical error when starting on the command line:  */
      if( errno == EBADF )
        ufprintf( stderr, "\nml2vim: fatal: descriptor 3 must be open on startup\n" );
      else
        ufprintf( stderr, "\nml2vim: fatal: fcntl() failed, errno = %d\n", errno );
      show_help = TRUE;
      rval = 2;
    }
  }

  if( show_help ) {
    ufprintf( stderr, "\nThis program should be called from within Vim (say `:help Mma' for more" );
    ufprintf( stderr, "\ninformation. However, you can test it on the command line using a call" );
    ufprintf( stderr, "\nlike `ml2vim 3>&1'. The following command line options are understood:" );
    ufprintf( stderr, "\n  --help     issue this help message and exit" );
    ufprintf( stderr, "\n  --version  show version information and exit" );
    ufprintf( stderr, "\n  --debug    log actions to ml2vim.log in current directory\n" );
    fflush( stderr );
    exit( rval );
  }

  /*  initializations:  */

  initialize_options( &global_options );

  viewers = NULL;
  atexit( & kill_all_viewers );

  next_to_vim = last_to_vim = free_to_vim = NULL;

  from_vim_be = (
    ( from_vim_bp = from_vim_buf = (char *)safe_malloc( BUFSIZE ) )
    + BUFSIZE
  );

  /* after start, the kernel will send us a first prompt. we wait for it: */
  waiting_for_mma_reply = TRUE;
  need_dummy_output = FALSE;
  current_mma_in_prompt = -1;

  btm_in_work = btm_in_eval = next_to_mma = last_to_mma = NULL;

  if( ! ( mlenv = MLInitialize(NULL) ) )
    die( "main: MLInitialize() failed", 0 );

  launch_kernel();

  leave_mainloop = got_sigchld = FALSE;

  signal( SIGTERM, sighandler );
  signal( SIGQUIT, sighandler );
  signal( SIGCHLD, sighandler );
  signal( SIGHUP, sighandler );
  signal( SIGINT, sighandler );

  do {

    handle_vim();
    if( leave_mainloop ) {
      trace( "mainloop: someone asked me to exit, so I will leave now" );
      break;
    }

    handle_mathlink();

    if( got_sigchld ) {
      check_for_dead_childs();    /*  beware of zombies!  */
      got_sigchld = FALSE;
    }

    if( getppid() != ppid ) {
      trace( "Ooops, our parent is gone?  We better exit, too!" );
      break;
      /*  (this is a kludge, as Vim currently will not reliably kill its childs)  */
    }

  } until( TheCowsComeHome );

  trace( "main: exited from main loop, will deinitialize mlink connection" );

  MLClose( mlink );
  MLDeinitialize( mlenv );

  return rval;
}


