# sqlcl-shell

Emacs interaction with Oracle SQLcl command line utility.


This Emacs package allows you to connect to an Oracle database >= 11g
from within Emacs using Oracle SQLcl command line utility on a Linux client.

To install and setup SQLcl you need.
Oracle Java 11 or higher (Not guaranteed that all functionality will work with OpenJDK)
Oracle Java is NOT licensed when used with SQLcl.

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

## Description

Before attempting to use this Emacs lisp package.
Verify you can connect to your database(s) with SQLcl using EZ-connect as described above.
Setup the following two environment variables

```
SQLCL_PATH -> Should point to the 'bin' directory where you unzipped your downloaded SQLcl.
```

Example:

You have a home directory /home/joe and you unzipped SQLcl under this directory.

```
export SQLCL_PATH=/home/joe/sqlcl/bin
```

If you want a better SQL prompt in SQLcl you should copy the included xlogin.sql to
your preferd SQLPATH directory and rename it to login.sql

In the example using Joe's home directory he uses ~/orascript (/home/joe/orascript) as the
default directory for sql files.

So put the login.sql in /home/joe/orascrip and then export SQLPATH as

```
export SQLPATH=/home/joe/orascript
```

In your personal Emacs configuration file you need the following (change load-path accordingly)
since this library not yet is part of MELPA or any other public repository.

```
(add-to-list 'load-path "~/Documents/emacs-packages/sqlcl-shell")
;; Initalize sqlcl-shell
(require 'sqlcl-shell)
```

In the example with Joe it means we put the gitrepo for sqlcl-shell in
/home/joe/Documents/emacs-packages/sqlcl-shell

Reload your Emacs config if necessary.

To start an interactive SQLcl session from Emacs use

M-x sqlcl-shell-run (where M is your Meta-key).

## Need more connections ?

If you need more then one connection you should do the following.
Switch to the current opened *SQLcl* buffer.

Then use `M-x rename-buffer` to rename `*SQLcl*` to something else like `sqlcl-hr-freepdb1` if you
are connected to the HR schema on the FREEPDB1 database.

Now you can invoke `M-x sqlcl-shell-run` and connect to another schema.

## Editing SQL and send code to the SQLcl buffer.

You probably will edit your SQL and PL/SQL code in another editor using the SQL syntax highlighting provided by sql.el
If you use SQLcl rather then SQL*PLUS you can add the following code to your emacs config file to be able to
send your SQL text over the '*SQLCL*' buffer.

```
(defun sqlcl-send-region-or-buffer (start end)
  "Send the selected region or the entire buffer to the SQLcl process.
If a region is active, send it. Otherwise, send the whole buffer."
  (interactive "r")
  ;; Ensure the *SQLcl* buffer exists and get the process.
  (let ((sqlcl-buffer (get-buffer "*SQLcl*")))
    (if (not sqlcl-buffer)
        (message "SQLcl buffer not found. Make sure `sqlcl-shell.el` is running.")
      (let ((sqlcl-process (get-buffer-process sqlcl-buffer)))
        (if (not sqlcl-process)
            (message "SQLcl process is not active.")
          (let ((text (if (use-region-p)
                          (buffer-substring-no-properties start end)
                        (buffer-substring-no-properties (point-min) (point-max)))))
            ;; Switch to the SQLcl buffer and send the text.
            (with-current-buffer sqlcl-buffer
              (goto-char (point-max)) ; Move to the end of the buffer.
              (insert text)           ; Insert the text.
              (comint-send-input))    ; Send the input to the process.
            (message "Text sent to SQLcl.")))))))

;; Optional: Bind this function to a convenient key.
(global-set-key (kbd "C-c s") 'sqlcl-send-region-or-buffer)
```


## History

2023-08-26 Release 1.0.1

Fixed issue handling new connection within SQLcl where password entered was not hidden.
Now `sqlcl-shell-run` uses `comint-password-prompt-regexp` and prompts for password correctly in hiden mode.

Now also support login as /nolog when entering this as username SQLcl starts without any active connection.
You can enter a new connection as:

```
conn <username>@//<hostname>:<portno>/<service_name>
```

Example:

```
conn hr@//localhost:1521/freepdb1
```

2023-10-03 Release 1.0.2

Fixed in issue where Emacs getenv not alwasy find environment variables
So changed method to more stable way of getting value of SQLCL_PATH variable

2023-11-30 Release  1.0.3

There might be a problem readning environment variables like SQLCL_PATH if you run Emacs as a daemon.
This means Emacs might not be able to see your environment correctly. Added an option to 
"hardcode" the path to SQLcl. You can change that if SQLcl do not start in Emacs by changing
the following code block to your location of SQLcl.

```
(setq sqlcl-binary
      (substitute-in-file-name
       (if (string= (shell-command-to-string "$SHELL --login -c 'echo -n $SQLCL_PATH'") "")
           ;; You might need to change the row below yourself
           "/home/uhellstr/opt/instantclient_21_10/sqlcl/bin/sql" <-- Change this line to match location of SQLcl if SQLCL_PATH returns an empty string.
         (concat (shell-command-to-string "$SHELL --login -c 'echo -n $SQLCL_PATH'") "/bin/sql" ))))
```

2024-12-27 Release  1.0.4

Complete rewrite how we get the PATH to sql binary no need for using any personal modifications from
last release 1.0.3 anymore. You still need to have "../bin" in your PATH variable.

2024-12-29 Release 1.0.5

Better cursor point handling , but still not perfect some more work is needed.
Added example on how to copy SQL code over to the SQLcl buffer from another buffer in README.md
