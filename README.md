# sqlcl-shell

Emacs interaction with Oracle SQLcl command line utility.


This Emacs package allows you to connect to an Oracle database >= 11g
from within Emacs using Oracle SQLcl command line utility on a Linux client.

To install and setup SqlCmdLine you need.
Oracle Java 11 or higher (Not guaranteed that all functionality will work with OpenJDK)
Oracle Java is NOT licensed if runtime is with SQLcl.

Unzipped version of SQLcl
https://www.oracle.com/database/sqldeveloper/technologies/sqlcl/

There is no need of an Oracle Client installed since the connection is done
using EZ-connect like: 

conn schema@//host:port/SERVICE_NAME

So if we have a schema demo on the host myhost with an Oracle Database listener on port 1521 and
a service called MYDEMODB the connection is done as

```
conn demo@//myhost:1521/MYDEMODB
```

Description

Before attempting to use this package.
Verify you can connect to your database(s) with SQLcl using EZ-connect as described above.
Setup the following two environment variables

```
SQLCL_PATH -> Should point to the directory where you unzipped your downloaded SQLcl.
```

Example:

You have a home directory /home/joe and you unzipped SQLcl under this directory.

```
export SQLCL_PATH=/home/joe/sqlcl
```

You should also put the included login.sql to your SQLPATH (Default path for Oracle scripts)
This file includes a specific login prompt that sqlcl-shell.el looks for when passing and fetching
data between Emacs and SQLcl so it a essential part of getting it to work. And it also
gives you a better SQL prompt.

In the example using Joe's home directory he uses ~/orascript (/home/joe/orascript) as the
default directory for sql files.

So put the login.sql in /home/joe/orascrip and then export SQLPATH as

```
export SQLPATH=/home/joe/orascript
```

In your personal Emacs configuration file you need the following (change load-path accordingly)
since this package not yet is part of MELPA or any other public repository.

```
(add-to-list 'load-path "~/Documents/emacs-packages/sqlcl-shell")
;; Initalize sqlcl-shell
(require 'sqlcl-shell)
```

In the example with Joe it means we put the gitrepo for sqlcl-shell in
/home/joe/Documents/emacs-packages/sqlcl-shell

Reload your Emacs config if necessary.

To start an interactive SQLcl session from Emacs use

M-x sqlcl-shell-run

