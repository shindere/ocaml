/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/*
 Core is not written when the process' current directory at crash time
 is not writable
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{
  char *p = NULL;
  if ( chdir("/") < 0) perror("chdir");
  *p = 'a';
  return 0;
}
