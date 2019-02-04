// Copyright (C) 2011 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "app.h"
#include "scheme/scheme.h"

scheme *sc=NULL;;
FILE *log_file=NULL;

// Called from the app framework.
void appInit(char *log_file_str)
{
    sc=scheme_init_new();

    FILE *log_file=fopen(log_file_str,"w");
    // we can fail due to permissions not set - 
    // as this may be happening before the runtime requests appear...
    if (log_file!=NULL) scheme_set_output_port_file(sc, log_file);
    else scheme_set_output_port_file(sc, stdout);
}

// Called from the app framework.
void appDeinit()
{
  if (log_file!=NULL) {
    fclose(log_file);
  }
  int a;
}

void appEval(char *code)
{
    scheme_load_string(sc,code);
    fflush(log_file);
}

