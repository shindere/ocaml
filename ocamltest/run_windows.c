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
#include "run_common.h"

static void report_error(
  char *file, int line,
  const command_settings *settings,
  char *message)
{
  char error_message[1024];
  DWORD error = GetLastError();
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, error, 0,
    (LPTSTR) &error_message, sizeof(error_message), NULL);
  error_with_location(file, line, settings, "%s: %s", message, error_message);
}

static char *find_program(const char *program_name)
{
  int max_path_length = 512;
  LPCTSTR searchpath = NULL, extension = ".exe";
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
    /* It may be an absolute path, return a copy of it */
    int l = strlen(program_name) + 1;
    free(fullpath);
    fullpath = malloc(l);
    if (fullpath != NULL) strcpy(fullpath, program_name);
    return fullpath;
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

static char *commandline_of_arguments(char **arguments) 
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

static HANDLE create_input_handle(const char *filename)
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
  return CreateFile
  (
    filename,
    desired_access,
    share_mode,
    &security_attributes, 
    creation_disposition,
    flags_and_attributes,
    template_file
  );
}

static HANDLE create_output_handle(const char *filename, int append)
{
  DWORD desired_access = append ? FILE_APPEND_DATA : GENERIC_WRITE;
  DWORD share_mode = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
  SECURITY_ATTRIBUTES security_attributes;
  DWORD creation_disposition = append ? OPEN_ALWAYS : CREATE_ALWAYS;
  DWORD flags_and_attributes = FILE_ATTRIBUTE_NORMAL;
  HANDLE template_file = NULL;
  security_attributes.nLength = sizeof(SECURITY_ATTRIBUTES);
  security_attributes.lpSecurityDescriptor = NULL;
  security_attributes.bInheritHandle = TRUE;
  return CreateFile
  (
    filename,
    desired_access,
    share_mode,
    &security_attributes, 
    creation_disposition,
    flags_and_attributes,
    template_file
  );
}

#define checkerr(condition, message) \
if ( condition ) \
{ \
  report_error(__FILE__, __LINE__, settings, message); \
  status = -1; \
  goto cleanup; \
} else { }

int run_command(const command_settings *settings)
{
  BOOL process_created;
  int stdin_redirected = 0, stdout_redirected = 0, stderr_redirected = 0;
  int combined = 0; /* stdout and stderr are redirected to the same file */
  int wait_again = 0;
  char *program = NULL;
  char *commandline = NULL;
  LPSECURITY_ATTRIBUTES process_attributes = NULL;
  LPSECURITY_ATTRIBUTES thread_attributes = NULL;
  BOOL inherit_handles = TRUE;

  DWORD creation_flags = 0;
  LPVOID environment = NULL;
  LPCTSTR current_directory = NULL;
  STARTUPINFO startup_info;
  PROCESS_INFORMATION process_info;
  DWORD wait_result, timeout, status;

  ZeroMemory(&startup_info, sizeof(STARTUPINFO));
  startup_info.cb = sizeof(STARTUPINFO);
  startup_info.dwFlags = STARTF_USESTDHANDLES;

  program = find_program(settings->program);
  checkerr( (program == NULL), "Could not find program to execute");

  commandline = commandline_of_arguments(settings->argv);

  if (is_defined(settings->stdin_filename))
  {
    startup_info.hStdInput = create_input_handle(settings->stdin_filename);
    checkerr( (startup_info.hStdInput == INVALID_HANDLE_VALUE),
      "Could not redirect standard input");
    stdin_redirected = 1;
  } else startup_info.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

  if (is_defined(settings->stdout_filename))
  {
    startup_info.hStdOutput = create_output_handle(
      settings->stdout_filename, settings->append
    );
    checkerr( (startup_info.hStdOutput == INVALID_HANDLE_VALUE),
      "Could not redirect standard output");
    stdout_redirected = 1;
  } else startup_info.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);

  if (is_defined(settings->stderr_filename))
  {
    if (stdout_redirected)
    {
      if (strcmp(settings->stdout_filename, settings->stderr_filename) == 0)
      {
        startup_info.hStdError = startup_info.hStdOutput;
        stderr_redirected = 1;
        combined = 1;
      }
    }
    
    if (! stderr_redirected)
    {
      startup_info.hStdError = create_output_handle
      (
        settings->stderr_filename, settings->append
      );
      checkerr( (startup_info.hStdError == INVALID_HANDLE_VALUE),
        "Could not redirect standard error");
      stderr_redirected = 1;
    }
  } else startup_info.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  process_created = CreateProcess(
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
  checkerr( (! process_created), "CreateProcess failed");

  CloseHandle(process_info.hThread); /* Not needed so closed ASAP */

  if (settings->timeout == 0) timeout = INFINITE;
  else timeout = settings->timeout * 1000;

  wait_result = WaitForSingleObject(process_info.hProcess, timeout);
  if (wait_result == WAIT_OBJECT_0)
  {
    /* The child has terminated before the timeout has expired */
    checkerr( (! GetExitCodeProcess(process_info.hProcess, &status)),
      "GetExitCodeProcess failed");
  } else if (wait_result == WAIT_TIMEOUT) {
    /* The timeout has expired, terminate the process */
    checkerr( (! TerminateProcess(process_info.hProcess, 0)),
      "TerminateProcess failed");
    status = -1;
    wait_again = 1;
  } else {
    error_with_location(__FILE__, __LINE__, settings, "WaitForSingleObject failed\n");
    report_error(__FILE__, __LINE__, settings, "Failure while waiting for process termination");
    status = -1;
  }

cleanup:
  free(program);
  free(commandline);
  if (stdin_redirected) CloseHandle(startup_info.hStdInput);
  if (stdout_redirected) CloseHandle(startup_info.hStdOutput);
  if (stderr_redirected && !combined) CloseHandle(startup_info.hStdError);
  if (wait_again)
  {
    /* Wait agian but this time just 1sec to avoid being blocked */
    WaitForSingleObject(process_info.hProcess, 1000);
  }
  if (process_created) CloseHandle(process_info.hProcess);
  return status;
}
