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

(cl-deftype listex:keyword ()
  '(and symbol
        (satisfies (lambda (k)
                     (string-prefix-p listex-keyword-prefix
                                      (format "%s" k))))))

(cl-deftype listex:car-is-atom ()
  '(satisfies (lambda (expr) (atom (car expr)))))

(cl-deftype listex:command ()
  '(and list
        listex:car-is-atom
        (satisfies (lambda (expr)
                     (let ((name (format "%s" (car expr))))
                       (string-prefix-p listex-command-prefix
                                        name))))))

(cl-deftype listex:operator ()
  '(and list
        listex:car-is-atom
        (satisfies (lambda (expr)
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

(defmacro listex:lisp-macro-get-fun (name)
  `(alist-get ,name listex-lisp-macro-alist))

(cl-deftype listex:lisp-macro ()
  `(and list
        listex:car-is-atom
        (satisfies ,(lambda (expr) (listex:lisp-macro-get-fun (car expr))))))

(defvar listex-tex-macro-alist nil)
(defvar listex-lisp-macro-alist nil)

;; TODO: remove and use cl-letf
;; this is for the newcmdlet macro
(defvar listex-tex-macro--alist-user nil)

(defmacro listex:addcmd (alist key fmt &rest !args)
  `(setf (alist-get ',key ,alist)
         ,(cl-etypecase fmt
            (function fmt)
            (string `(lambda (args)
                       (format ,fmt
                               ,@(cl-loop
                                  for a in !args
                                  collect `(listex:render-tex ,a))))))))

(defmacro listex:lisp-macro-alist-pair (alist key args list-or-fun)
  `(list '(alist-get ',key ,alist)
     ,(cl-etypecase list-or-fun
        (function list-or-fun)
        (list `(lambda ,args
                 ,list-or-fun)))))

(defmacro listex:defmacro (key !args list-or-fun)
  `(let ((args (listex:lisp-macro-alist-pair listex-lisp-macro-alist
                                             ,key
                                             ,!args
                                             ,list-or-fun)))
     (eval `(setf ,@args))))



(defmacro listex:newcmd (key fmt &rest !args)
  `(listex:addcmd listex-tex-macro-alist
                  ,key ,fmt ,@!args))

(defmacro newcmdlet (ncmds &rest body)
  `(let (listex-tex-macro--alist-user)
     ,@(cl-loop for cmd in ncmds
             collect
             `(listex:addcmd listex-tex-macro--alist-user
                             ,(car cmd) ,(cadr cmd)
                             ,@(cddr cmd)))
     ,@body))


;; set indentation for lt-macrolet and newcmdlet correctly
(progn
  (put 'lt-macrolet 'lisp-indent-function 'defun)
  (put 'lt-macrolet* 'lisp-indent-function 'defun)
  (put 'newcmdlet 'lisp-indent-function 'defun))

;; important macros
(listex:newcmd braced "{%s}" args)
(listex:newcmd progn "%s" args)

;; left right stuff
(listex:newcmd lr "\\left%s%s\\right%s" (car args) (cddr args) (cadr args))
(listex:defmacro lrp (&rest args) `(lr \( \) ,@args))
(listex:defmacro lrs (&rest args) `(lr \[ \] ,@args))
(listex:defmacro set (&rest args) `(lr /{ /} ,@args))

;; quantum mechanics
(listex:defmacro <| (&rest args) `(lr /langle | ,@args))
(listex:defmacro |> (&rest args) `(lr | /rangle ,@args))

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

(listex:defmacro mat (&rest args) `(env pmatrix ,@args))

;; Math environments
(listex:defmacro $ (&rest args) `(begend $ ,@args))
(listex:defmacro $$ (&rest args) `(begend $$ ,@args))
(listex:defmacro eq (&rest args) `(env equation ,@args))
(listex:defmacro eq* (&rest args) `(env equation* ,@args))
(listex:defmacro al (&rest args) `(env align ,@args))
(listex:defmacro al* (&rest args) `(env align* ,@args))

;; force newlines in the output
(listex:newcmd terpri "\n")
(listex:newcmd br "\n")
(listex:newcmd nl "\n")

;; more convoluted example
(listex:defmacro matrix
                 (rows cols &rest elements)
                 (progn
                   (cl-assert (eq (length elements) (* cols rows)))
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
                                    do (push '& buff)))))

(defun listex:render-tex (expr)
  "Main function to convert a listex DSL s-expression
   into a latex-compatible string."
  (cl-etypecase expr
    (listex:lisp-macro (let* ((args (cdr expr))
                              (name (car expr))
                              (f (listex:lisp-macro-get-fun name))
                              (new-expr (apply f args)))
                         (listex:render-tex new-expr)))
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
            (args-strings (mapcar #'listex:render-tex args)))
       (concat name (string-join args-strings))))
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

(defun listex:expand-lisp-macro (expr)
  "This function should expand all listex:lisp-macro
   s-expressions by the s-expression that they expand to,
   so that in some cases you can just get the whole.

   This works as it follows:

   - if an expression is a lisp-macro,
     then it will first expand its arguments
     and then return the expansion of the parent
     with the expansion of the arguments replaced.
   - If an expression is a command, tex-macro
     or an operator expression, then it will replace
     the same expression just with the elements replaced
     by their expansions.
   - Otherwise, it should replace just the bare expression."
  (cl-typecase expr
    (listex:lisp-macro (let* ((args (mapcar #'listex:expand-lisp-macro
                                            (cdr expr)))
                              (name (car expr))
                              (f (listex:lisp-macro-get-fun name)))
                         (apply f args)))
    ;; expand the arguments
    ((or listex:command
         listex:operator
         listex:macro) (let ((args (mapcar #'listex:expand-lisp-macro
                                           (cdr expr)))
                             (name (car expr)))
         `(,name ,@args)))
    (list (mapcar #'listex:expand-lisp-macro expr))
    (otherwise expr)))

(defmacro lt-macrolet (bindings &rest body)
  (let ((letf-args (cl-loop for b in bindings
                            collect (eval `(listex:lisp-macro-alist-pair
                                            listex-lisp-macro-alist
                                            ,@b)))))
    `(cl-letf (,@letf-args)
       (listex:expand-lisp-macro (progn ,@body)))))

(defmacro lt-macrolet* (bindings &rest body)
  (let ((init `(progn ,@body)))
    (cl-loop for b in (reverse bindings)
           with result = init
           do (setq result `(lt-macrolet (,b) ,result))
           finally
           return result)))

(provide 'listex)
