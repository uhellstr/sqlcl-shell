;;; sqlcl.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ulf Hellström
;;
;; Author: Ulf Hellström <oraminute@gmail.com> Epico Tech
;; Maintainer: Ulf Hellström <oraminute@gmail.com>
;; Created: augusti 01, 2023
;; Modified: augusti 01, 2023
;; Version: 0.0.1
;; Keywords: languages lisp unix linux database oracle sqlcl
;; Homepage: https://github.com/uhellstr/sqlcl.el
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package allows you to connect to an Oracle database >= 11g
;;  from within Emacs using Oracle SqlCmdLine on a Linux client.
;;
;;  To install and setup SqlCmdLine you need.
;;  Oracle Java 11 or higher (Not garanteed that all functionality will work with OpenJDK)
;;  Oracle Java is NOT licensed if runtime is used for SQLCmdLine.
;;
;;  Unzipped version of sqlcmdline
;;  https://www.oracle.com/database/sqldeveloper/technologies/sqlcl/
;;
;;  There is no need of an Oracle Client installed since the connection is done
;;  using EZ-connect like conn <schema>@//host:port/SERVICE_NAME
;;
;;  So if we have a schem demo on the host myhost with an Oracle Database listener on port 1521 and
;;  a service called MYDEMODB the connection is done as
;;
;;  conn demo@//myhost:1521/MYDEMODB
;;
;;
;;  Description
;;
;;  Before attemting to use this package.
;;  Verify you can connect to your database(s) with SqlCMDLine using EZ-connect as described above.
;;  Setup the following two environment variables
;;
;;  SQLCL_PATH -> Should point to the directory where you unzipped your downloaded sqlCmdLine
;;
;;  Example:
;;
;;  You have a home directory /home/joe and you unzipped sqlcl under this directory.
;;
;;  export SQLCL_PATH=/home/joe/sqlcl
;;
;;  You should also put the included login.sql to your SQLPATH (Default path for Oracle scripts)
;;  This file includes a specific login prompt that sqlcl.el looks for when passing and fetching
;;  data between Emacs and sqlCmdLine so it a essential part of getting it to work.
;;
;;  In the example using Joe's home directory he uses ~/orascript (/home/joe/orascript) as the
;;  default directory for sql files.
;;
;;  So put the login.sql in /home/joe/orascrip and then export SQLPATH as
;;
;;  export SQLPATH=/home/joe/orascript
;;
;;  In your Emacs your personal Emacs configuration file you need to add something like the following
;;  since this package not yet is part of MELPA or any other public repository.
;;
;;  (add-to-list 'load-path "~/Documents/emacs-packages/sqlcl-shell")
;;
;;  In the example with Joe it means we put the gitrepo for sqlcl-shell in
;;  /home/joe/Documents/emacs-packages/sqlcl-shell
;;
;;  Reload your Emacs config if necessary.
;;
;;  To start an interactive sqlcmdline session from Emacs use
;;
;;  M-x sqlcl
;;
;;  You will be prompted for username,password, host, listner port and service name.
;;
;;
;;; Code:

(provide 'sqlcl)

(setq-default message-log-max nil)
(setq sqlcl-binary (concat (getenv "SQLCL_PATH") "/bin/sql"))
(defvar sqlcl-proc-path sqlcl-binary "Path to the program used by sqlcl-run")
(defvar sqlcl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for sqlcl-run")

;; Define a regular expression for a SQL prompt like SYS @ XEPDB1 >
(defvar sqlcl-prompt-regexp "^([A-Z0-9]+)(\s*)@(\s*)([A-Z0-9]+)(\s*)>" "Prompt for `sqlcl-run'")

;; Prompt for username
(setq *my-ora-username* nil)
(defun sqlcl.get-ora-username()
"Prompt user for Oracle schema/username"
    (interactive)
    (setq *my-ora-username* (read-string "Username : ")))

;; Prompt for password
 (setq *my-ora-secret-password* nil)
 (defun sqlcl.get-ora-password ()
 "Prompt user for a password"
        (interactive)
        (setq *my-ora-secret-password* (read-passwd "Password : ")))

;; Prompt for hostname
(setq *my-ora-hostname* nil)
(defun sqlcl.get-ora-hostname()
"Prompt user for Oracle Host or scan-listener"
    (interactive)
    (setq *my-ora-hostname* (read-string "Hostname : ")))

;; Prompt for listener port
(setq *my-ora-portno* nil)
(defun sqlcl.get-ora-port()
"Prompt user for Oracle Listener port"
    (interactive)
    (setq *my-ora-portno* (read-string "Oracle Listener Port : ")))

;; Prompt for Service name
(setq *my-ora-servicename* nil)
(defun sqlcl.get-ora-service ()
"Prompt user for Oracle Servicename"
        (interactive)
        (setq *my-ora-servicename* (read-string "Oracle Servicename : ")))

;; Function for building connection string
(defun sqlcl.get-oracle-connection-properties ()
"Function for setting up a EZ-Connection string for SQLCmdline"
        (get-ora-username)
        (get-ora-password)
        (get-ora-hostname)
        (get-ora-port)
        (get-ora-service)
        (setq sqlcl-proc-args (concat *my-ora-username* "/" *my-ora-secret-password* "@//" *my-ora-hostname* ":" *my-ora-portno* "/" *my-ora-servicename*)))

(defun sqlcl-run ()
  "Run an inferior instance of `sqlcl' inside Emacs."
  (interactive)
  (get-oracle-connection-properties)
  (let* ((sqlcl-program sqlcl-proc-path)
         (buffer (comint-check-proc "SQLcl")))
    ;; pop to the "*SQLcl*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'sqlcl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*SQLcl*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (funcall 'make-comint-in-buffer "SQLcl" buffer
             sqlcl-program nil sqlcl-proc-args)
      (sqlcl-mode))))

(defun sqlcl-initialize ()
  "Helper function to initialize Sqlcl"
  (setq sqlcl-proc-args nil)
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (add-hook 'comint-output-filter-functions #'(lambda (txt) (message txt))))

(define-derived-mode sqlcl-mode comint-mode "Sqlcl"
  "Major mode for `run-sqlcl'. \\<sqlcl-mode-map>"
  nil "Sqlcl"
  ;; this sets up the prompt so it matches things like: SYSTEM @ XEPDB1
  (setq comint-prompt-regexp sqlcl-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(sqlcl-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sqlcl-prompt-regexp))

(add-hook 'sqlcl-mode-hook 'sqlcl-initialize)

;;; sqlcl.el ends here
