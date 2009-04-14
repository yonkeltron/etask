;;;;Copyright (C) 2009 by Jonathan E. Magen
;;;; ETasks System for Rake-like task management for Emacs


;;; Set things up
(defvar *etask-tasks* (make-hash-table :test 'equal))

;;; internally-used-utility-functions
(defun ok-to-mess-with-file-p (source &optional dest)
  (and (file-readable-p source)
       (if dest (file-writable-p dest) t)))

;;; exec a command with the shell
(defun sh (command-string)
  "Executes a command string using the shell"
  (message (shell-command-to-string command-string)))

;;; file manipulation helpers
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

;;; filelists and filelist iteration
(defun filelist (pattern)
  "Returns a list with all files matched by the expression pattern"
  (file-expand-wildcards pattern))

(defmacro filelist-each (pattern action)
  "Perform an action on each file matched by pattern"
  `(dolist (file (filelist ,pattern))
     (,action file)))

;;; main task code
(defun task (name commands &optional deps)
  "Creates a new task"
;;  (interactive)
  (let ((task-object (make-hash-table :test 'equal)))
    ;construct task-object
    (puthash "task-name" name task-object)
    (puthash "action" commands task-object)
    (puthash "deps" deps task-object)
    ;put task-object into global task list
    (puthash name task-object *etask-tasks*)))

(defun etask-run-task (taskname)
  "Runs a task with the given name"
  (interactive)

  (let ((task (gethash taskname *etask-tasks* nil))
	(no-task-found (lambda (name) (message (concat "Couldn't find a task called " name)))))
    (if (task)
	(progn
	  (message (concat "Beginning run of task " (gethash 'name task nil) " at " (current-time-string)))
	  ((gethash "action" task nil)))
      (no-task-found taskname))))
