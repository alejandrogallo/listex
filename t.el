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


;; rendering
(defun assert-render (alist)
  (dolist (c alist)
    (cl-destructuring-bind (str . expr) c
      (cl-assert (string= str (listex:render-tex expr))))))

(assert-render
 '(("int" . (int))
   ("pretty raw" . (pretty raw))
   ("int" . int)
   ("\\int" . /int)
   ("\\int x \\d" . (/int x /d))
   ("\\frac{A}{5}" . (-frac A 5))
   ("A % B % C" . (% A B C))
   ("A^{2}" . (^ A 2))))

;; lt-macrolet examples



(lt-macrolet ((Σ `(_ (^ (/sum /limits) ,(car args)) ,(cadr args)))
              (ω `(5 6))
              (λ `(-frac (-mathrm ,(car args))
                         (-mathbf ,(car args)))))
  '(λ (+ (Σ (ω) (ω))
         (lrp (ω)))))
