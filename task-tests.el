;;;;
;; Quick demo of etask stuff
(require 'etask)

;;; This creates a file using shell commands before renaming and deleting the file.
;;; If all goes well, there is no file left after the test runs!
(etask-task "test-move-and-delete"
      (lambda ()
	(progn
	  (etask-sh "touch panda")
	  (etask-mv "panda" "bamboo")
	  (etask-sh "stat bamboo")
	  (etask-rm "bamboo"))))

;;; This test gives a little test of the shell command helper
(etask-task "test-shell"
      (lambda ()
	(etask-sh "echo 'Shell test...'")))

;;; Tests the logging functionality. Output goes to *etask-tasks* buffer
(etask-task "test-log"
      (lambda ()
	(etask-log "Now testing the log...")))

;;; This will hold dependency tests sooner or later
(etask-task "test-task"
      (lambda () (etask-log "Beginning dependency tests...")))

;;; run the tests if the buffer gets eval'd
(etask-run-task "test-log")
(etask-run-task "test-shell")
(etask-run-task "test-move-and-delete")


