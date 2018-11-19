;; this is a list of functions meant to organize contacts in a lisp db/file


;; EDITIONS TO THE ADDRESS BOOK

;; definition of global variables
(defvar *addressbook* nil)

;; first function: to "write" a name-char
(defun make-name (last first job mail phone)
  (list :lastname last :firstname first :job job :mail mail :phone phone))

;; function to add a name to the address book
(defun add-entry (name)
  (push name *addressbook*))



;; FUNCTIONS FOR IO

;; function to output the addressbook in a really readable format
(defun print-book ()
  (dolist (name *addressbook*)
    (format t "~{~a:~12t~a~%~}~%" name)))

;; functions to work around input
(defun prompt-read (prompt)
  (format *query-io* "~a:~10t" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-name ()
  (make-name
   (prompt-read "LAST NAME")
   (prompt-read "FIRST NAME")
   (prompt-read "JOB")
   (prompt-read "MAIL")
   (or (parse-integer (prompt-read "PHONE NUMBER") :junk-allowed t) 0)))

;; save the addressbook
(defun save-book (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *addressbook* out))))

;; read the addressbook
(defun load-book (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *addressbook* (read in)))))


;; MAIN LOOP FUNCTION
(defun edit-contacts ()
  (loop (add-entry(prompt-for-name))
     (if (not (y-or-n-p "Another? [y/N]: ")) (return))))


(defun main-menu()
  (loop
     (cond ((string-equal (prompt-read "whaddyawanado? ") "add") (edit-contacts))
	   ((string-equal (prompt-read "whaddyawanado? ") "save") (save-book (prompt-read "Save file as: ")))
	   ((string-equal (prompt-read "whaddyawanado? ") "load") (load-book (prompt-read "Load File: ")))
	   ((string-equal (prompt-read "whaddyawanado? ") "exit") (return)))))
