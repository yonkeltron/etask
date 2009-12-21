;;; etasks.el --- Rake-like task managment for Emacs
;; Copyright (C) 2009 by Jonathan E. Magen
;; Version: 0.1
;; Author: Jonathan E. Magen <yonkeltron[AT-NOSPAM]gmail[DOT-NOSPAM]com>
;; Maintainer: Jonathan E. Magen <yonkeltron [AT-NOSPAM] gmail [DOT-NOSPAM] com>
;; Keywords: files, process, tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commentary: The goal here is to
;; produce a nice little task-management and automation package to
;; work the way that Ruby's Rake tool works.
;;
;; This is not intended to be a complete replacement, though I do
;; forsee some nice possibilities. In the meantime, it does provide a
;; few helper methods and features such as file manipulation helpers,
;; filelist building, filelist iteration and even some nice
;; logging. On the other hand, namespaces and dependencies are not yet
;; implemented. Patches, advice and suggestions are welcome.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(provide 'etasks)

;; Set things up
(defvar *etask-tasks* (make-hash-table :test 'equal))

;; internally-used-utility-functions
(defun etask-ok-to-mess-with-file-p (source &optional dest)
  "Checks if a file manipulation operation is permitted"
  (and (file-readable-p source)
       (if dest (file-writable-p dest) t)))

(defun object-to-string (obj)
  "Converts any object into a printable representation"
  (prin1-to-string obj t))

(defun etask-log (message)
  "Logs a MESSAGE to the *etask-output* buffer"
  (get-buffer-create "*etask-output*")
  (with-current-buffer "*etask-output*"
    (setq buffer-read-only nil)
    (insert (concat (object-to-string message) "\n"))
    (setq buffer-read-only t)))

(defun etask-get-task (name)
  "Returns the task object of a given NAME from the global task list"
  (gethash name *etask-tasks*))

(defun etask-exec (task-object)
  "Internally used function to execute an individual task's action"
  (etask-log (concat "Executing: " (gethash "task-name" task-object)))
  (funcall (gethash "action" task-object)))

;; exec a command with the shell
(defun etask-sh (command-string)
  "Executes a command string using the shell"
  (if (stringp command-string)
      (etask-log (concat "sh: " command-string ": " (shell-command-to-string command-string)))
    (error (concat "Commands passed to sh must be a string! Instead it was "
		   (object-to-string command-string)))))

;; file manipulation helpers
(defun etask-cp (source dest)
  "Copies a file from source to dest"
  (if (etask-ok-to-mess-with-file-p source dest)
      (etask-log (concat "cp " source " -> " dest ": " (copy-file source dest)))
    (error (concat "Unable to copy " source " -> " dest "!"))))

(defun etask-mv (source dest)
  "Moves (renames) a file from source to dest"
  (if (etask-ok-to-mess-with-file-p source dest)
      (etask-log (concat "mv " source " -> " dest ": " (rename-file source dest)))
    (error (concat "Unable to move " source " -> " dest "!"))))

(defun etask-rm (filename)
  "Removes (deletes) a file called filename"
  (if (etask-ok-to-mess-with-file-p filename)
      (etask-log (concat "rm " filename ": " (delete-file filename)))
    (error (concat "Unable to delete " filename "!"))))

;; filelists and filelist iteration
(defun etask-filelist (pattern)
 "Returns a list with all files matched by the expression pattern"
  (file-expand-wildcards pattern))

(defmacro etask-filelist-each (pattern action)
  "Perform an action on each file matched by pattern"
  `(dolist (file (etask-filelist ,pattern))
     (,action file)))

;; main task code
(defun etask-task (name commands &optional deps)
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
    (etask-log (concat "\nBeginning run of task "
		       (gethash "task-name" task)
		       " at "
		       (current-time-string)))
    ; execute task dependencies
    (dolist (task-dep-name (gethash "deps" task nil))
      (etask-exec (etask-get-task task-dep-name)))
    ; retrieve and run the function stored in "action"
    (etask-exec task)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; etasks.el ends here

