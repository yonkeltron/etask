;;; etasks.el --- Rake-like task managment for Emacs
;; Copyright (C) 2009 by Jonathan E. Magen
;; Author: Jonathan E. Magen <yonkeltron [AT-NOSPAM] gmail [DOT-NOSPAM] com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA


;;; Code:

;; Set things up
(defvar *etask-tasks* (make-hash-table :test 'equal))

;; internally-used-utility-functions
(defun ok-to-mess-with-file-p (source &optional dest)
  (and (file-readable-p source)
       (if dest (file-writable-p dest) t)))

(defun object-to-string (obj) (prin1-to-string obj))

(defun etask-log (x)
  (get-buffer-create "*etask-output*")
  (with-current-buffer "*etask-output*"
;    (end-of-buffer)
    (insert (concat (object-to-string x) "\n"))))

(defun etask-get-task (name)
  (gethash name *etask-tasks*))

;; exec a command with the shell
(defun sh (command-string)
  "Executes a command string using the shell"
  (message (shell-command-to-string command-string)))

;; file manipulation helpers
(defun cp (source dest)
  "Copies a file from source to dest"
  (if (ok-to-mess-with-file-p source dest)
      (copy-file source dest)))

(defun mv (source dest)
  "Moves (renames) a file from source to dest"
  (if (ok-to-mess-with-file-p source dest)
      (rename-file source dest)))

(defun rm (filename)
  "Removes (deletes) a file called filename"
  (if (ok-to-mess-with-file-p filename)
      (delete-file filename)))

;; filelists and filelist iteration
(defun filelist (pattern)
 "Returns a list with all files matched by the expression pattern"
  (file-expand-wildcards pattern))

(defmacro filelist-each (pattern action)
  "Perform an action on each file matched by pattern"
  `(dolist (file (filelist ,pattern))
     (,action file)))

;; main task code
(defun task (name commands &optional deps)
  "Creates a new task with a NAME which runs COMMANDS"
  (let ((task-object (make-hash-table :test 'equal)))
    ;construct task-object
    (puthash "task-name" name task-object)
    (puthash "action" commands task-object)
    (puthash "deps" deps task-object)
    ;put task-object into global task list
    (puthash name task-object *etask-tasks*)))

(defun etask-run-task (taskname)
  "Runs a task with the given name"
  ; grap the task name from the user
  (interactive "MTask name to run: ")
  (let
      ;fetch the hash-table containing the task data
      ((task (gethash taskname *etask-tasks*)))
    ;log the beginning of a task run
    (etask-log (concat "Beginning run of task :" (gethash "task-name" task) " at " (current-time-string)))
    ; retrieve and run the function stored in "action"
    (funcall (gethash "action" task))))

;;; etasks.el ends here