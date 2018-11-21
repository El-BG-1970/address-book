;; this is a list of functions meant to organize contacts in a lisp db/file


;; EDITIONS TO THE ADDRESS BOOK

;; definition of global variables
(defvar *addressbook* nil)
(defvar *welcomemsg* "Ich! Welcome to List' 2 Kontakt' ")
(defvar *helpmsg* "Hello, welcome to List' 2 Kontakt'. Here you can do multiple things; There is the command list: - add: add an entry to your addressbook - save: save the current addressbook to file - load: load addressbook from file - exit: exit")

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

;; querying the book
(defun where (&key last first job mail phone)
  #'(lambda (name)
       (and
	(if last (string-equal (getf name :last) last) t)
	(if last (string-equal (getf name :first) first) t)
	(if last (string-equal (getf name :job) job) t)
	(if last (string-equal (getf name :mail) mail) t)
	(if last (equal (getf name :phone) phone) t))))

(defun select (selector-function)
  (remove-if-not selector-function *addressbook*))

;; MAIN LOOP FUNCTION
(defun edit-contacts ()
  (loop (add-entry(prompt-for-name))
     (if (not (y-or-n-p "Another? [y/N]: ")) (return))))

(defun query-contacts ()
  (select
   (where 
   :last (prompt-read "LAST NAME")
   :first (prompt-read "FIRST NAME")
   :job (prompt-read "JOB")
   :mail (prompt-read "MAIL")
   :phone (parse-integer (prompt-read "PHONE NUMBER") :junk-allowed t))))


(defun main-menu()
  (format t "~a~%" *welcomemsg*)
  (loop
     (setq jose (prompt-read "Whatchawannado? "))
     (cond ((string-equal jose "add") (edit-contacts))
	   ((string-equal jose "save") (save-book (prompt-read "Save file as: ")))
	   ((string-equal jose "load") (load-book (prompt-read "Load File: ")))
	   ((string-equal jose "help") (format t "~a~%" *helpmsg*))
	   ((string-equal jose "search") (query-contacts))
	   ((string-equal jose "exit") (return)))))

