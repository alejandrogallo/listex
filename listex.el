;; [[file:README.org::*Prolog][Prolog:1]]
(require 'cl-lib)
;; Prolog:1 ends here

;; [[file:README.org::*org-src blocks][org-src blocks:2]]
(defvar org-babel-default-header-args:listex '((:exports . "results")
                                               (:results . "value drawer")
                                               (:eval . "t")))

(defvar org-babel-header-args:listex '((:label . :any)
                                       (:lisp-label . :any)
                                       (:env . :any)
                                       (:eval . :boolean))
  "listex-specific header arguments.")

(defun org-babel-execute:listex (body params)
  (cl-labels ((maybe-symbol (key) (let ((maybe-string (cdr (assq key params))))
                                    (when maybe-string (intern maybe-string)))))
    (let* ((should-eval (maybe-symbol :eval))
           (raw-expr (car (read-from-string body)))
           (expr (if should-eval (eval raw-expr) raw-expr))
           (env (maybe-symbol :env))
           (label (maybe-symbol :label))
           (lisp-label (or (maybe-symbol :lisp-label)
                           label)))
      (message "penis %s %s" should-eval (type-of should-eval))
      (when lisp-label
        (message "new lisp-label defined: %s" lisp-label)
        (eval `(setq-local ,lisp-label ',expr)))
      (listex
       (if env
           `(,env ,(if label
                       `((-label ,label) ,expr)
                     expr))
         expr)))))
;; org-src blocks:2 ends here

;; [[file:README.org::*Derived mode][Derived mode:1]]
(define-derived-mode listex-mode
  emacs-lisp-mode "LiSTeX"
  "Major mode for listex.
\\{listex-mode-map}")
;; Derived mode:1 ends here

;; [[file:README.org::*Misc][Misc:1]]
(defvar listex-keyword-prefix "/")
(defvar listex-command-prefix "-")

(defvar listex-operator-prefix "%")
(defvar listex-operator-list '(+ * - =))

(defun listex:indentation (len)
  (eval `(concat ,@(cl-loop for i from 1 to len collect " "))))
;; Misc:1 ends here

;; [[file:README.org::*Types][Types:1]]
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


(defmacro listex:lisp-macro-get-fun (name)
  `(alist-get ,name listex-lisp-macro-alist))

(cl-deftype listex:lisp-macro ()
  '(and list
        listex:car-is-atom
        (satisfies (lambda (expr) (listex:lisp-macro-get-fun (car expr))))))

(defvar listex-alias-alist nil
  "Alist holding all the aliases.")

(defmacro listex:get-alias (name)
  `(alist-get ,name listex-alias-alist))

(cl-deftype listex:alias ()
  '(and symbol
        (satisfies (lambda (expr) (listex:get-alias expr)))))
;; Types:1 ends here

;; [[file:README.org::*Implementation][Implementation:1]]
(defvar listex-lisp-macro-alist nil
  "Alist storing all listex macros that are defined.")

(defmacro listex:lisp-macro-alist-pair (alist key args list-or-fun)
  `(list '(alist-get ',key ,alist)
         ,(cl-etypecase list-or-fun
            (function list-or-fun)
            (list `(cl-flet ((f ,args ,list-or-fun))
                     (cl-function f))))))

(defmacro listex:defmacro (key !args list-or-fun)
  `(let ((args (listex:lisp-macro-alist-pair listex-lisp-macro-alist
                                             ,key
                                             ,!args
                                             ,list-or-fun)))
     (eval `(setf ,@args))))



(defmacro listex:newcmd--format-function (args fmt)
  `(format ,fmt ,@(cl-loop for a in args
                           ;; make sure that a is not a & identifier
                           ;; for functions like &rest
                           if (not (string-prefix-p "&" (symbol-name a)))
                           collect `(listex:render-tex ,a))))

(defmacro listex:newcmd (key args fmt)
  `(listex:defmacro ,key ,args (listex:newcmd--format-function ,args, fmt)))
;; Implementation:1 ends here

;; [[file:README.org::*Implementation][Implementation:2]]
(defmacro listex:defalias (alias key)
  `(cl-pushnew (cons ',alias ',key) listex-alias-alist :test #'cl-equalp))
;; Implementation:2 ends here

;; [[file:README.org::*TeX macro definitions][TeX macro definitions:1]]
;; important macros
(listex:newcmd braced (&rest body) "{%s}")
(listex:newcmd progn (&rest body) "%s")
(listex:newcmd list (&rest body) "%s")

;; left right stuff
(listex:newcmd lr (l r &rest body) "\\left%1$s %3$s \\right%2$s")
(listex:defmacro lrp (&rest args) `(lr \( \) ,@args))
(listex:defmacro lrs (&rest args) `(lr \[ \] ,@args))
(listex:defmacro set (&rest args) `(lr /{ /} ,@args))

;; quantum mechanics
(listex:defmacro <| (&rest args) `(lr /langle | ,@args))
(listex:defmacro |> (&rest args) `(lr | /rangle ,@args))

;; exponents
(listex:newcmd ^ (base &rest sup) "%s^{%s}")
(listex:newcmd _ (base &rest sub) "%s_{%s}")
(listex:newcmd ^_ (base sup sub) "%s^{%s}_{%s}")
(listex:newcmd _^ (base sub sup) "%s_{%s}^{%s}")

;; wrapping
(listex:newcmd begend (b &rest bod) "%s%s%1$s")
(listex:newcmd env (env-name &rest body) "\\begin{%1$s}\n%s\n\\end{%1$s}")

(listex:defmacro mat (&rest args) `(env pmatrix ,@args))

;; Math environments
(listex:defmacro $ (&rest args) `(begend $ ,@args))
(listex:defmacro $$ (&rest args) `(begend $$ ,@args))
(listex:defmacro eq (&rest args) `(env equation ,@args))
(listex:defmacro eq* (&rest args) `(env equation* ,@args))
(listex:defmacro al (&rest args) `(env align ,@args))
(listex:defmacro al* (&rest args) `(env align* ,@args))

;; force newlines in the output
(listex:newcmd terpri () "\n")
(listex:newcmd br () "\n")
(listex:newcmd nl () "\n")

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
                                    do (cl-incf i)
                                    if (eq (% i cols) 0)
                                    do (push '\\\\ buff)
                                    and collect (reverse buff)
                                    and do (setf buff nil)
                                    else
                                    do (push '& buff)))))
;; TeX macro definitions:1 ends here

;; [[file:README.org::*Implementation][Implementation:1]]
(defun listex:render-tex (expr)
  "Main function to convert a listex DSL s-expression
   into a latex-compatible string."
  (cl-etypecase expr
    (listex:lisp-macro (let* ((args (cdr expr))
                              (name (car expr))
                              (f (listex:lisp-macro-get-fun name))
                              (new-expr (apply f args)))
                         (listex:render-tex new-expr)))
    (listex:alias (let* ((replacement (listex:get-alias expr)))
                    (listex:render-tex replacement)))
    (listex:keyword (format "\\%s"
                            (string-remove-prefix listex-keyword-prefix
                                                  (symbol-name expr))))
    (listex:command
     (let* ((args (mapcar (lambda (e) (cl-etypecase e
                                        (vector e)
                                        (otherwise (format
                                                    "{%s}"
                                                    (listex:render-tex e)))))
                          (cdr expr)))
            (name (format "\\%s" (string-remove-prefix listex-command-prefix
                                                       (symbol-name
                                                        (car expr)))))
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
    (list (string-join (mapcar #'listex:render-tex expr) " "))
    (atom (format "%s" expr))))




(defun listex (expr)
  (listex:render-tex expr))
;; Implementation:1 ends here

;; [[file:README.org::*Macrolet][Macrolet:1]]
(cl-defun listex:expand-lisp-macro (expr &key recursive)
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
  (cl-flet ((expander (e) (listex:expand-lisp-macro e :recursive recursive)))
    (cl-typecase expr
      (listex:lisp-macro (let* ((name (car expr))
                                (args (mapcar #'expander (cdr expr)))
                                (f (listex:lisp-macro-get-fun name))
                                (new-expr (apply f args)))
                           (if recursive (expander new-expr) new-expr)))
      (listex:alias (let ((new-expr (listex:get-alias expr)))
                      (if recursive (expander new-expr) new-expr)))
      ;; expand the arguments
      ((or listex:command listex:operator)
       (let ((name (car expr))
             (args (mapcar #'expander (cdr expr))))
         `(,name ,@args)))
      (list (mapcar #'expander expr))
      (otherwise expr))))
;; Macrolet:1 ends here

;; [[file:README.org::*Macrolet][Macrolet:2]]
(defmacro listex:letconstruct (recursive
                               pair-constructor
                               alist bindings
                               &rest body)
  (let ((letf-args (cl-loop for b in bindings
                            collect (eval `(,pair-constructor
                                            ,alist
                                            ,@b)))))
    `(cl-letf (,@letf-args)
       (listex:expand-lisp-macro (progn ,@body) :recursive ,recursive))))

(defmacro listex:alias-alist-pair (alist key replacement)
  `(list '(alist-get ',key ,alist)
     ,(cl-etypecase replacement
        ((or atom cons) `',replacement))))

(defmacro lt-aliaslet (bindings &rest body)
  `(listex:letconstruct nil
                        listex:alias-alist-pair
                        listex-alias-alist
                        ,bindings
                        ,@body))

(defmacro lt-aliaslet* (bindings &rest body)
  `(listex:letconstruct t
                        listex:alias-alist-pair
                        listex-alias-alist
                        ,bindings
                        ,@body))

(defmacro lt-macrolet (bindings &rest body)
  `(listex:letconstruct nil
                        listex:lisp-macro-alist-pair
                        listex-lisp-macro-alist
                        ,bindings
                        ,@body))

(defmacro lt-macrolet* (bindings &rest body)
  `(listex:letconstruct t
                        listex:lisp-macro-alist-pair
                        listex-lisp-macro-alist
                        ,bindings
                        ,@body))

(defmacro lt-cmdlet (cmds &rest body)
  `(lt-macrolet ,(cl-loop for cmd in cmds
                          collect
                          (let ((key (car cmd))
                                (args (cadr cmd))
                                (fmt (caddr cmd)))
                            `(,key ,args
                                   (listex:newcmd--format-function ,args
                                                                   ,fmt))))
     ,@body))

;; set indentation for lt-macrolet and other let constructs correctly
(progn
  (put 'lt-aliaslet 'lisp-indent-function 'defun)
  (put 'lt-aliaslet* 'lisp-indent-function 'defun)
  (put 'lt-macrolet 'lisp-indent-function 'defun)
  (put 'lt-macrolet* 'lisp-indent-function 'defun)
  (put 'lt-cmdlet 'lisp-indent-function 'defun))
;; Macrolet:2 ends here

;; [[file:README.org::*Preview listex][Preview listex:1]]
(defun listex:render-last-sexpr (&optional not-eval?)
  (interactive "P")
  (let ((sexp (thing-at-point 'sexp)))
    (message (listex:render-tex (if not-eval?
                                    (read sexp)
                                    (eval (read sexp)))))))

(defun listex:render-defun (&optional not-eval?)
  (interactive "P")
  (let ((sexp (thing-at-point 'defun)))
    (message (listex:render-tex (if not-eval?
                                    (read sexp)
                                    (eval (read sexp)))))))
;; Preview listex:1 ends here

;; [[file:README.org::*Epilog][Epilog:1]]
(provide 'listex)
;; Epilog:1 ends here
