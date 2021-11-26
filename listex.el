(require 'cl-lib)

(defvar org-babel-default-header-args:listex '((:exports . "results")
                                               (:results . "value drawer")))

(defvar org-babel-header-args:listex '((:label . :any)
                                       (:lisp-label . :any)
                                       (:env . :any))
  "listex-specific header arguments.")

(defun org-babel-execute:listex (body params)
  (labels ((maybe-symbol (key) (let ((maybe-string (cdr (assq key params))))
                                 (when maybe-string (intern maybe-string)))))
    (let* ((expr (eval (car (read-from-string body))))
           (env (maybe-symbol :env))
           (label (maybe-symbol :label))
           (lisp-label (or (maybe-symbol :lisp-label)
                           label)))
      (when lisp-label
        (message "new lisp-label defined: %s" lisp-label)
        (eval `(setq-local ,lisp-label ',expr)))
      (listex
       (if env
           `(,env ,(if label
                       `((-label ,label) ,expr)
                     expr))
         expr)))))

(define-derived-mode listex-mode
  emacs-lisp-mode "LiSTeX"
  "Major mode for listex.
\\{listex-mode-map}")

(defvar listex-keyword-prefix "/")
(defvar listex-command-prefix "-")

(defvar listex-operator-prefix "%")
(defvar listex-operator-list '(+ * - =))

(defun listex:indentation (len)
  (eval `(concat ,@(cl-loop for i from 1 to len collect " "))))

(defvar listex-tex-macro-alist nil)
(defvar listex-lisp-macro-alist nil)
;; this is for the newcmdlet macro
(defvar listex-tex-macro--alist-user nil)

(defmacro listex:addcmd (alist key fmt &rest !args)
  `(setf (alist-get ',key ,alist)
         ,(cl-etypecase fmt
            (function fmt)
            (otherwise `(lambda (args)
                         ,(if fmt
                              `(format ,fmt ,@(cl-loop
                                               for a in !args
                                               collect `(listex:render-tex
                                                         ,a)))
                            `(listex:render-tex ,@!args)))))
         ))

(defmacro listex:newcmd (key fmt &rest !args)
  `(listex:addcmd listex-tex-macro-alist
                  ,key ,fmt ,@!args))

(defmacro newcmdlet (ncmds &rest body)
  `(let (listex-tex-macro--alist-user)
     ,@(loop for cmd in ncmds
             collect
             `(listex:addcmd listex-tex-macro--alist-user
                             ,(car cmd) ,(cadr cmd)
                             ,@(cddr cmd)))
     ,@body))

;; important macros
(listex:newcmd braced "{%s}" args)
(listex:newcmd progn "%s" args)






;; left right stuff
(listex:newcmd lr "\\left%s%s\\right%s" (car args) (cddr args) (cadr args))
(listex:newcmd lrp nil `(lr \( \) ,@args))
(listex:newcmd lrs nil `(lr \[ \] ,@args))
(listex:newcmd set nil `(lr /{ /} ,@args))

;; quantum mechanics
(listex:newcmd <| nil `(lr /langle | ,@args))
(listex:newcmd |> nil `(lr | /rangle ,@args))

;; exponents
(listex:newcmd ^ "%s^{%s}" (car args) (cdr args))
(listex:newcmd _ "%s_{%s}" (car args) (cdr args))
(listex:newcmd ^_ "%s^{%s}_{%s}" (car args) (cadr args) (caddr args))
(listex:newcmd _^ "%s_{%s}^{%s}" (car args) (cadr args) (caddr args))

;; wrapping
(listex:newcmd begend "%s%s%s"
               (car args)
               (cdr args)
               (car args))
(listex:newcmd env "\\begin{%s}\n%s\n\\end{%s}"
               (car args)
               (cdr args)
               (car args))
(listex:newcmd mat nil `(env pmatrix ,@args))

;; Math environments
(listex:newcmd $ nil `(begend $ ,@args))
(listex:newcmd $$ nil `(begend $$ ,@args))
(listex:newcmd eq nil `(env equation ,@args))
(listex:newcmd eq* nil `(env equation* ,@args))
(listex:newcmd al nil `(env align ,@args))
(listex:newcmd al* nil `(env align* ,@args))

;; force newlines in the output
(listex:newcmd terpri "\n")
(listex:newcmd br "\n")
(listex:newcmd nl "\n")

;; more convoluted example
(listex:newcmd matrix
               (lambda (args)
                 (let ((rows (car args))
                       (cols (cadr args))
                       (elements (cddr args)))
                   (cl-assert (eq (length elements) (* cols rows)))
                   (listex:render-tex
                    `(env pmatrix
                          ,@(cl-loop for el in elements
                                     with i = 0
                                     with buff = nil
                                     do (push el buff)
                                     do (incf i)
                                     if (eq (% i cols) 0)
                                     do (push '\\\\ buff)
                                     and collect (reverse buff)
                                     and do (setf buff nil)
                                     else
                                     do (push '& buff)))))))

(cl-deftype listex:keyword ()
  `(and symbol
        (satisfies ,(lambda (k)
                      (string-prefix-p listex-keyword-prefix
                                       (format "%s" k))))))

(cl-deftype listex:car-is-atom ()
  '(satisfies (lambda (expr) (atom (car expr)))))

(cl-deftype listex:command ()
  `(and list
        listex:car-is-atom
        (satisfies ,(lambda (expr)
                      (let ((name (format "%s" (car expr))))
                        (string-prefix-p listex-command-prefix
                                         name))))))

(cl-deftype listex:operator ()
  `(and list
        listex:car-is-atom
        (satisfies ,(lambda (expr)
                      (let ((name (car expr)))
                        (or (member name listex-operator-list)
                            (string-prefix-p listex-operator-prefix
                                             (format "%s" name))))))))

(defmacro listex:macro-get-fun (name)
  `(or (alist-get ,name listex-tex-macro--alist-user)
       (alist-get ,name listex-tex-macro-alist)))

(cl-deftype listex:macro ()
  `(and list
        listex:car-is-atom
        (satisfies ,(lambda (expr) (listex:macro-get-fun (car expr))))))

(defun listex:render-tex (expr)
  "Render listex DSL into latex"
  (cl-etypecase expr
    (listex:keyword (format "\\%s"
                            (string-remove-prefix listex-keyword-prefix
                                                  (symbol-name expr))))
    (listex:command
     (let* ((args (mapcar (lambda (e) (cl-etypecase e
                                        (vector e)
                                        (otherwise `(braced ,e))))
                          (cdr expr)))
            (name (format "\\%s" (string-remove-prefix listex-command-prefix
                                                      (symbol-name (car expr)))))
            (args-strings (mapcar #'listex:render-tex args))
            ;(braced-strings (mapcar #'listex:braced args-strings))
            )
       (concat name (string-join args-strings))
       ))
    (listex:operator (let* ((name (car expr))
                            (namestr (symbol-name name))
                            (op (if (> (length namestr) 1)
                                    (string-remove-prefix listex-operator-prefix
                                                          namestr)
                                  namestr)))
                      (string-join (mapcar #'listex:render-tex (cdr expr))
                                   (format " %s " op))))
    (listex:macro (let* ((args (cdr expr))
                         (name (car expr))
                         (f (listex:macro-get-fun name)))
                    (funcall f args)))
    (list (string-join (mapcar #'listex:render-tex expr) " "))
    (atom (format "%s" expr))))

(defun listex (expr)
  (listex:render-tex expr))
