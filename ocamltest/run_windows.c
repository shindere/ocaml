/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sébastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Run programs with rediretions and timeouts under Windows */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#include "run.h"

/* is_defined(str) returns 1 iff str points to a non-empty string */
/* Otherwise returns 0 */
static inline int is_defined(const char *str)
{
  return (str != NULL) && (*str != '\0');
}

void defaultLogger(void *where, const char *format, va_list ap)
{
  vfprintf(stderr, format, ap);
}

void mylog(Logger *logger, void *loggerData, char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  logger(loggerData, fmt, ap);
  va_end(ap);
}

void error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg, ...)
{
  va_list ap;
  Logger *logger = (settings->logger != NULL) ? settings->logger
                                              : defaultLogger;
  void *loggerData = settings->loggerData;
  va_start(ap, msg);
  mylog(logger, loggerData, "%s:%d: ", file, line);
  logger(loggerData, msg, ap);
  mylog(logger, loggerData, "\n");
  va_end(ap);
}

#define error(msg, ...) \
error_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/*
  Note: the ## __VA_ARGS__ construct is gcc specific.
  For a more portable (but also more complex) solution, see
  http://stackoverflow.com/questions/20818800/variadic-macro-and-trailing-comma
*/

void myperror_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg, ...)
{
  va_list ap;
  Logger *logger = (settings->logger != NULL) ? settings->logger
                                              : defaultLogger;
  void *loggerData = settings->loggerData;
  va_start(ap, msg);
  mylog(logger, loggerData, "%s:%d: ", file, line);
  logger(loggerData, msg, ap);
  mylog(logger, loggerData, ": %s\n", strerror(errno));
  va_end(ap);
}

#define myperror(msg, ...) \
myperror_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/* Same remark as for the error macro. */

void open_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  myperror_with_location(file, line, settings, "Can not open %s", msg);
}

#define open_error(filename) \
open_error_with_location(__FILE__, __LINE__, settings, filename)

void realpath_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  myperror_with_location(file, line, settings, "realpath(\"%s\") failed", msg);
}

#define realpath_error(filename) \
realpath_error_with_location(__FILE__, __LINE__, settings, filename)

int run_command(const command_settings *settings)
{
  return 0;
}
