;;; sqlcl-shell.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022-2023  Free Software Foundation, Inc.
;;
;;       ___  ___  _       _
;;      / __|/ _ \| |   __| |
;;      \__ \ (_) | |__/ _| |
;;      |___/\__\_\____\__|_|
;;
;; Author: Ulf Hellström <oraminute@gmail.com> Epico Tech
;; Maintainer: Ulf Hellström <oraminute@gmail.com>
;; Created: augusti 01, 2023
;; Modified: augusti 02, 2023
;; Version: 1.0.1
;; Keywords: languages lisp unix linux database oracle sqlcl
;; Homepage: https://github.com/uhellstr/sqlcl-shell.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.;
;;
;;; Commentary:
;;
;;  This Emacs Lisp library allows you to connect to an Oracle database >= 11g
;;  from within Emacs using Oracle SQLcl on a Linux client.
;;
;;  To install and setup SQLcl you need.
;;  Oracle Java 11 or higher (Not garanteed that all functionality will work with OpenJDK)
;;  Oracle Java is NOT licensed if runtime is used for SQLcl.
;;
;;  Unzipped version of SQLcl
;;  https://www.oracle.com/database/sqldeveloper/technologies/sqlcl/
;;
;;  There is no need of an Oracle Client installed since the connection is done
;;  using EZ-connect like conn <schema>@//host:port/SERVICE_NAME
;;
;;  So if we have a schema demo on the host myhost with an Oracle Database listener on port 1521 and
;;  a service called MYDEMODB the connection is done as
;;
;;  conn demo@//myhost:1521/MYDEMODB
;;
;;
;;  Description
;;
;;  Before attemting to use this package.
;;  Verify you can connect to your database(s) with SQLcl using EZ-connect as described above.
;;
;;  Setup the following two environment variables
;;
;;  SQLCL_PATH -> Should point to the directory where you unzipped your downloaded SQLcl
;;
;;  Example:
;;
;;  You have a home directory /home/joe and you unzipped sqlcl under this directory.
;;
;;  export SQLCL_PATH=/home/joe/sqlcl/bin
;;  export PATH=$PATH:$SQLCL_PATH
;;
;;  If you want a better SQL prompt in SQLcl you should copy the included xlogin.sql to
;;  your preferd SQLPATH directory and rename it to login.sql
;;
;;  In the example using Joe's home directory he uses ~/orascript (/home/joe/orascript) as the
;;  default directory for sql files.
;;
;;  So put the login.sql in /home/joe/orascrip and then export SQLPATH as
;;
;;  export SQLPATH=/home/joe/orascript
;;
;;  In your personal Emacs configuration file you need to add something like the following
;;  if downnloading from github.
;;
;;  (add-to-list 'load-path "~/Documents/emacs-packages/sqlcl-shell")
;;  ;; Initialize sqlcl-shell
;;  (require 'sqlcl-shell)
;;
;;  In the example with Joe it means we put the gitrepo for sqlcl-shell in
;;  /home/joe/Documents/emacs-packages/sqlcl-shell
;;
;;  Reload your Emacs config if necessary.
;;
;;  To start an interactive SQLcl session from Emacs use
;;
;;  M-x sqlcl-shell-run
;;
;;  You will be prompted for username,password, host, listner port and service name.
;;
;;
;;; Code:

(require 'comint)

(provide 'sqlcl-shell)

;; Define the password prompt
(setq comint-password-prompt-regexp "^Password[?]?.*\\(\\**\\)?")

(setq-default message-log-max nil)

;; Find path for SQLcl binary and add /bin/sql to base path for SQLCL_PATH
;; Note: If you start Emacs as daemon you might need to hard code the path as below

(defun sqlcl-shell-get-path ()
  "Parse the PATH environment variable for the path containing sqlcl.
Return the full path to the sqlcl directory if found.
Raise an error if sqlcl is not found in PATH."
  (let* ((path-env (getenv "PATH"))
         (path-list (split-string path-env path-separator))
         (sqlcl-path (seq-find (lambda (p) (string-match-p "sqlcl" p)) path-list)))
    (if sqlcl-path
        sqlcl-path
      (error "The PATH variable does not contain 'sqlcl'"))))

(let ((sqlcl-path (sqlcl-shell-get-path)))
  "Add the SQLcl executable to the PATH."
  (setq sqlcl-binary (concat sqlcl-path "/sql")))

;; Define customizable variable
(defgroup sqlcl-shell-sql nil
  "Define group for cutomizable variable"
  :group 'convenience)
(defcustom sqlcl-shell-sql-path "~/orascript"
  "Define SQLPATH for sqlcl-shell"
  :type 'string
  :group 'sqlcl-shell-sql
  :local t)

;; Set Oracle environment variable SQLPATH to value of customizable variable.
(setenv "SQLPATH" (concat sqlcl-shell-sql-path ":."))
;; Set cwd to SQLPATH
(setq sqlcl-cwd sqlcl-shell-sql-path)
(cd sqlcl-cwd)
;; Define emacsclient as default editor when using edit command in SQLcl.
(setq sql-editor "emacsclient -t")
(setenv "EDITOR" sql-editor)
;; Setup
(defvar sqlcl-shell-proc-path sqlcl-binary "Path to the program used by sqlcl-run.")
(defvar sqlcl-shell-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `sqlcl-shell-run'.")

;; Define a regular expression for a SQL prompt like SYS @ XEPDB1 >
(defvar sqlcl-shell-prompt-regexp "^([A-Z0-9]+)(\s*)@(\s*)([A-Z0-9]+)(\s*)>" "Prompt for `sqlcl-shell-run'.")

;; Prompt for username
(setq *my-ora-username* nil)
(defun sqlcl-shell-get-ora-username()
  "Prompt user for Oracle schema/username."
  (interactive)
  (setq *my-ora-username* (read-string "Username : ")))

;; Prompt for password
(setq *my-ora-secret-password* nil)
(defun sqlcl-shell-get-ora-password ()
  "Prompt user for a password"
  (interactive)
  (setq *my-ora-secret-password* (read-passwd "Password : ")))

;; Prompt for hostname
(setq *my-ora-hostname* nil)
(defun sqlcl-shell-get-ora-hostname()
  "Prompt user for Oracle Host or scan-listener."
  (interactive)
  (setq *my-ora-hostname* (read-string "Hostname : ")))

;; Prompt for listener port
(setq *my-ora-portno* nil)
(defun sqlcl-shell-get-ora-port()
  "Prompt user for Oracle Listener port."
  (interactive)
  (setq *my-ora-portno* (read-string "Oracle Listener Port default 1521 : "))
  (when (string= *my-ora-portno* "")
    (setq *my-ora-portno* "1521")))

;; Prompt for Service name
(setq *my-ora-servicename* nil)
(defun sqlcl-shell-get-ora-service ()
  "Prompt user for Oracle Servicename."
  (interactive)
  (setq *my-ora-servicename* (read-string "Oracle Servicename : ")))

;; Function to check for sys user
(defun sqlcl-shell-is-sys-user-p ()
  "Check if entered username is either sys or SYS."
  (string-equal (downcase *my-ora-username*) "sys"))

;; Function to check if nolog
(defun sqlcl-shell-is-nolog-p ()
  "Check if entered username is /NOLOG or /nolog."
  (string-equal (downcase *my-ora-username*) "/nolog"))

;; Function for building connection string

(defun sqlcl-shell-get-oracle-connection-properties ()
  "Function for setting up a EZ-Connection string for SQLcl."
  (sqlcl-shell-get-ora-username)
  (if (sqlcl-shell-is-nolog-p)
      (setq sqlcl-proc-args "/nolog")
    (progn
      (sqlcl-shell-get-ora-password)
      (sqlcl-shell-get-ora-hostname)
      (sqlcl-shell-get-ora-port)
      (sqlcl-shell-get-ora-service)
      (if (sqlcl-shell-is-sys-user-p)
          (setq sqlcl-proc-args (concat *my-ora-username* "/" *my-ora-secret-password* "@//" *my-ora-hostname* ":" *my-ora-portno* "/" *my-ora-servicename* " as sysdba"))
        (setq sqlcl-proc-args (concat *my-ora-username* "/" *my-ora-secret-password* "@//" *my-ora-hostname* ":" *my-ora-portno* "/" *my-ora-servicename*))))))

(defun sqlcl-shell-run ()
  "Run an inferior instance of `SQLcl' inside Emacs."
  (interactive)
  (sqlcl-shell-get-oracle-connection-properties)
  (let* ((sqlcl-program sqlcl-shell-proc-path)
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
      (sqlcl-shell-mode))
    ;; Configure password prompt handling
    (setq-local comint-password-prompt-regexp "^Password[?]?.*\\(\\**\\)?")
    (setq-local comint-password-hide-regexp "\\**")
    (setq-local comint-password-inc-mask 0) ; Do not show '*' for entered password
    ))

(defun sqlcl-shell-initialize ()
  "Helper function to initialize Sqlcl."
  (setq sqlcl-proc-args nil)
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (setq mode-line-format nil)
  (add-hook 'comint-output-filter-functions #'(lambda (txt) (message txt))))

(define-derived-mode sqlcl-shell-mode comint-mode "Sqlcl"
  "Major mode for `run-sqlcl'. \\<sqlcl-mode-map>."
  nil "Sqlcl"
  ;; this sets up the prompt so it matches things like: SYSTEM @ XEPDB1
  (setq comint-prompt-regexp sqlcl-shell-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-onl t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) sqlcl-shell-prompt-regexp)
  (with-current-buffer "*SQLcl*"
    (display-line-numbers-mode -1)))

(add-hook 'sqlcl-mode-hook 'sqlcl-shell-initialize)

;;; sqlcl-shell.el ends here
