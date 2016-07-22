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
#include <wtypes.h>
#include <windows.h>
#include <process.h>
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

char *find_program(const char *program_name)
{
  int max_path_length = 512;
  LPCTSTR searchpath = NULL, extension = NULL;
  char **filepart = NULL;
  char *fullpath = malloc(max_path_length);
  if (fullpath == NULL) return NULL;
  
  DWORD result = SearchPath
  (
    searchpath,
    program_name,
    extension,
    max_path_length,
    fullpath,
    filepart
  );
  if (result == 0)
  {
    free(fullpath);
    return NULL;
  }
  if (result <= max_path_length) return fullpath;

  /* fullpath was too small, allocate a bigger one */
  free(fullpath);
  
  result++; /* Take '\0' into account */

  fullpath = malloc(result);
  if (fullpath == NULL) return NULL;
  SearchPath
  (
    searchpath,
    program_name,
    extension,
    result,
    fullpath,
    filepart
  );
  return fullpath;
}

char *commandline_of_arguments(char **arguments) 
{
  char *commandline = NULL, **arguments_p, *commandline_p;
  int args = 0; /* Number of arguments */
  int commandline_length = 0;

  if (*arguments == NULL) return NULL;
  /* From here we know there is at least one argument */

  /* First compute number of arguments and commandline length */
  for (arguments_p = arguments; *arguments_p != NULL; arguments_p++)
  {
    args++;
    commandline_length += strlen(*arguments_p);
  }
  commandline_length += args; /* args-1 ' ' between arguments + final '\0' */

  /* Allocate memore andy accumulate arguments separated by spaces */
  commandline = malloc(commandline_length);
  if (commandline == NULL) return NULL;
  commandline_p = commandline;
  for (arguments_p = arguments; *arguments_p!=NULL; arguments_p++)
  {
    int l = strlen(*arguments_p);
    memcpy(commandline_p, *arguments_p, l);
    commandline_p += l;
    *commandline_p = ' ';
    commandline_p++;
  }
  commandline[commandline_length] = '\0';
  return commandline;
}

int run_command(const command_settings *settings)
{
  BOOL ret;
  int stdout_redirected = 0, stderr_redirected = 0;
  char *program = NULL;
  char *commandline = NULL;
  LPSECURITY_ATTRIBUTES process_attributes = NULL;
  LPSECURITY_ATTRIBUTES thread_attributes = NULL;
  BOOL inherit_handles = TRUE;

  DWORD creation_flags = 0;
  LPVOID environment = NULL;
  LPCTSTR current_directory = NULL;
  STARTUPINFO startup_info;
  HANDLE stdin_handle, stdout_handle, stderr_handle;
  PROCESS_INFORMATION process_info;
  DWORD wait_result, timeout, status;
  
  program = find_program(settings->program);

  commandline = commandline_of_arguments(settings->argv);

  stdin_handle = GetStdHandle(STD_INPUT_HANDLE);
  if (is_defined(settings->stdin_filename))
  {
    DWORD desired_access = GENERIC_READ;
    DWORD share_mode = FILE_SHARE_READ;
    SECURITY_ATTRIBUTES security_attributes;
    DWORD creation_disposition = OPEN_EXISTING;
    DWORD flags_and_attributes = FILE_ATTRIBUTE_NORMAL;
    HANDLE template_file = NULL;
    security_attributes.nLength = sizeof(SECURITY_ATTRIBUTES);
    security_attributes.lpSecurityDescriptor = NULL;
    security_attributes.bInheritHandle = TRUE;
    stdin_handle = CreateFile
    (
      settings->stdin_filename,
      desired_access,
      share_mode,
      &security_attributes, 
      creation_disposition,
      flags_and_attributes,
      template_file
    );
    if (stdin_handle == INVALID_HANDLE_VALUE) return -1;
  }

  stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
  if (is_defined(settings->stdout_filename))
  {
    DWORD desired_access = settings->append? FILE_APPEND_DATA : GENERIC_WRITE;
    DWORD share_mode = FILE_SHARE_READ;
    SECURITY_ATTRIBUTES security_attributes;
    DWORD creation_disposition = settings->append ? OPEN_ALWAYS : CREATE_ALWAYS;
    DWORD flags_and_attributes = FILE_ATTRIBUTE_NORMAL;
    HANDLE template_file = NULL;
    security_attributes.nLength = sizeof(SECURITY_ATTRIBUTES);
    security_attributes.lpSecurityDescriptor = NULL;
    security_attributes.bInheritHandle = TRUE;
    stdin_handle = CreateFile
    (
      settings->stdout_filename,
      desired_access,
      share_mode,
      &security_attributes, 
      creation_disposition,
      flags_and_attributes,
      template_file
    );
    if (stdin_handle == INVALID_HANDLE_VALUE) return -1;
    stdout_redirected = 1;
  }

  stderr_handle = GetStdHandle(STD_ERROR_HANDLE);
  if (is_defined(settings->stderr_filename))
  {
    if (stdout_redirected)
    {
      if (strcmp(settings->stdout_filename, settings->stderr_filename) == 0)
      {
        stderr_handle = stdout_handle;
        stderr_redirected = 1;
      }
    }
    
    if (! stderr_redirected)
    {
      DWORD desired_access = settings->append? FILE_APPEND_DATA : GENERIC_WRITE;
      DWORD share_mode = FILE_SHARE_READ;
      SECURITY_ATTRIBUTES security_attributes;
      DWORD creation_disposition = settings->append ? OPEN_ALWAYS : CREATE_ALWAYS;
      DWORD flags_and_attributes = FILE_ATTRIBUTE_NORMAL;
      HANDLE template_file = NULL;
      security_attributes.nLength = sizeof(SECURITY_ATTRIBUTES);
      security_attributes.lpSecurityDescriptor = NULL;
      security_attributes.bInheritHandle = TRUE;
      stdin_handle = CreateFile
      (
        settings->stdout_filename,
        desired_access,
        share_mode,
        &security_attributes, 
        creation_disposition,
        flags_and_attributes,
        template_file
      );
      if (stdin_handle == INVALID_HANDLE_VALUE) return -1;
      stderr_redirected = 1;
    }
  }

  ZeroMemory(&startup_info, sizeof(STARTUPINFO));
  startup_info.cb = sizeof(STARTUPINFO);
  startup_info.dwFlags = STARTF_USESTDHANDLES;
  startup_info.hStdInput = stdin_handle;
  startup_info.hStdOutput = stdout_handle;
  startup_info.hStdError = stderr_handle;
  ret = CreateProcess(
    program,
    commandline,
    process_attributes,
    thread_attributes,
    inherit_handles,
    creation_flags,
    environment,
    current_directory,
    &startup_info,
    &process_info
  );
  if (ret==FALSE) return -1;
  if (settings->timeout == 0) timeout = INFINITE;
  else timeout = settings->timeout * 1000;
  wait_result = WaitForSingleObject(process_info.hProcess, timeout);
  if (wait_result == WAIT_OBJECT_0)
  {
    /* The child has terminated before the timeout has expired */
    GetExitCodeProcess(process_info.hProcess, &status);
  } else if (wait_result == WAIT_TIMEOUT) {
    /* The timeout has expired, terminate the process */
    TerminateProcess(process_info.hProcess, 0);
    status = -1;
  } else status = -1;
  free(program);
  free(commandline);
  return status;
}
