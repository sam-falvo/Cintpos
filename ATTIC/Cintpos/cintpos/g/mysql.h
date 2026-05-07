/*

This is the BCPL header file for the MySQL interface (not to
be confused with <mysql/mysql.h> used by C programs).

Implemented by Martin Richards (c) July 2006


All MySQL features are accessed by calles of the form:

rc := sys(Sys_mysql, op, <args>) where op is
typically a manifest constant such as mysql_connect.

All the C API functions are intended to have corresponding sys
calls.  The sys(Sys_mysql,...) operation performed by domysql defined
in the C program domysql.c.

Usage example:

GET "mysql.h"

LET start() = VALOF
{ LET mysql = 0
  AND result = 0
  AND row = 0
  AND chbuf = VEC 255/bytesperword)

  UNLESS sys(Sys_mysql, mysql_init, @mysql) DO
  { writef("Cannot initialze MySQL*n")
    GOTO fin
  }

  UNLESS sys(Sys_mysql, mysql_real_connect,
             "localhost",
             "user", "password",
             "db1", 0, 0, 0) DO
  { writef("%n: %s*n",
           sys(Sys_mysql, mysql_errno, mysql),
           sys(Sys_mysql, mysql_error, mysql, chbuf))
    GOTO fin
  }

  IF sys(Sys_mysql, mysql_query,
         mysql,
         "SELECT col1, col2 FROM table1") DO
  { writef("%n: %s*n",
           sys(Sys_mysql, mysql_errno, mysql),
           sys(Sys_mysql, mysql_error, mysql, chbuf))
    GOTO fin
  }

  result := sys(Sys_mysql, mysql_store_result, mysql)
  { LET row = sys(Sys_mysql, mysql_fetch_row, result)
    UNLESS row BREAK
    writef("%n - %s*n", row!0, row!1)
  } REPEAT
  sys(Sys_mysql, mysql_free_result, mysql)

fin:
  IF mysql DO sys(Sys_mysql, mysql_close, mysql)
  RESULTIS 0
}
*/

MANIFEST {
mysql_startup=1        // sqlbase := sys(mysql_startup)
                       // => sqlbase!0   mysql
                       // => sqlbase!1   result
                       // => sqlbase!2   row
mysql_init
mysql_affected_rows

}
