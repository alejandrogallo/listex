(defmacro assert-type (type &rest elements)
  `(dolist (expr ',elements)
     (cl-assert (cl-typep expr ',type)
                nil "%s is not of type %s" expr ',type)))

(defmacro assert-type! (type &rest elements)
  `(dolist (expr ',elements)
     (cl-assert (not (cl-typep expr ',type))
                nil "%s is not of type %s" expr ',type)))

;; keyword
(assert-type listex:keyword /int /sum /alpha)
(assert-type! listex:keyword int sum alpha)

;; command
(assert-type listex:command
             (-math) (-mathrm d x))
(assert-type! listex:command
              (2) -mathrm ((-mathrm d) x))

;; operator
(dolist (op listex-operator-list)
  (cl-assert (cl-typep `(,op) 'listex:operator)))
(assert-type listex:operator
             (+) (-) (*)
             (%custom) (%) (%%))

;; macro
(assert-type listex:macro
             (braced) (progn)
             (^) (^ A) (^ A B 2)
             (_) (_ A) (_ A B))
;; newcmdlet simple test
(newcmdlet ((not-a-tex-macro "not-a tex-macro"))
  (assert-type listex:macro
               (not-a-tex-macro)))
(assert-type! listex:macro
              (not-a-tex-macro))


;; lisp-macros
(assert-type listex:lisp-macro
             (lrp))
;; lt-macrolet
(lt-macrolet ((λ () '(this and that)))
  (assert-type listex:lisp-macro
               (λ)))
(assert-type! listex:lisp-macro
              (λ))

;; rendering
(defun assert-render (alist)
  (dolist (c alist)
    (cl-destructuring-bind (str . expr) c
      (let ((rendered (listex:render-tex expr)))
        (cl-assert (string= str rendered)
                   nil "expected: %s\nactual  : %s"
                   str rendered)))))

(assert-render
 '(("int" . (int))
   ("pretty raw" . (pretty raw))
   ("int" . int)
   ("% () strings work as in lisp %#" . "% () strings work as in lisp %#")

   ;; keywords
   ("\\int" . /int)
   ("\\int x \\d" . (/int x /d))

   ;; commands
   ("\\frac{A}{5}" . (-frac A 5))

   ;; operators
   ("A % B % C" . (% A B C))
   ("A + B + C" . (+ A B C))
   ("A = B = C" . (= A B C))
   ("A =& B =& C" . (%=& A B C))
   ("A &+\\\\ B &+\\\\ C" . (%&+\\\\ A B C))

   ;; tex macros
   ("{raw}" . (braced raw))
   ("raw" . (progn raw))
   ("A^{2}" . (^ A 2))
   ("A_{2}" . (_ A 2))

   ;; lisp macros
   ("\\left(\\i\\right)" . (lrp /i))))


;; lt-macrolet examples
(lt-macrolet ((Σ (up down) `(_ (^ (/sum /limits) ,up) ,down))
              (ω () `(5 6))
              (λ (a) `(-frac (-mathrm ,a)
                             (-mathbf ,a))))
  '(λ (+ (Σ (ω) (ω))
         (lrp (ω)))))
