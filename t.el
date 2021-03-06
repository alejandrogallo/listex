;; [[file:README.org::*Tests][Tests:1]]
(defmacro assert-type (type &rest elements)
  `(dolist (expr ',elements)
     (cl-assert (cl-typep expr ',type)
                nil "%s is not of type %s" expr ',type)))

(defmacro assert-type! (type &rest elements)
  `(dolist (expr ',elements)
     (cl-assert (not (cl-typep expr ',type))
                nil "%s is not of type %s" expr ',type)))

;; keyword
(assert-type listex:keyword /int /sum /alpha // /%)
(assert-type! listex:keyword int sum alpha)

;; command
(assert-type listex:command
             (-math) (-mathrm d x))
(assert-type! listex:command
              ((-lala)) (2) -mathrm
              ((-mathrm d) x))

;; operator
(dolist (op listex-operator-list)
  (cl-assert (cl-typep `(,op) 'listex:operator)))
(assert-type listex:operator
             (+) (-) (*)
             (%custom) (%) (%%))
;; test listex-operator-list
(let (listex-operator-list)
  (assert-type listex:operator (%א) (%a))
  (assert-type! listex:operator (+) (-) (*)))

;; lisp-macros
(assert-type listex:lisp-macro
             (braced) (progn)
             (^) (^ A) (^ A B 2)
             (_) (_ A) (_ A B)
             (lrp))


(put 'assert-replacements 'lisp-indent-function 4)
(defmacro assert-replacements (letconstruct
                               type
                               binding-extractor
                               bindings
                               &rest replacements)
  `(progn
     ;; first of al make sure that the bindings are not in the binding space
     (assert-type! ,type ,@(mapcar binding-extractor bindings))
     ;; go throught the replacements alist
     ,@(cl-loop for r in replacements
                collect
                ;; make an assertment of the structure that comes
                ;; of of the replacement
                `(let ((should ',(cdr r))
                       (is (,letconstruct ,bindings
                                            ;; assert the types that are now
                                            ;; in the bindings
                                            (assert-type ,type
                                                         ,@(mapcar
                                                            binding-extractor
                                                            bindings))
                                            ',(car r))))
                   (cl-assert (equal should is)
                              nil "expected: %s\nactual  : %s"
                              should is)))
     ;; make sure that no bindings leaked after the letconstruct
     (assert-type! ,type ,@(mapcar binding-extractor bindings))))

;; alias
(assert-replacements lt-aliaslet listex:alias car
                     ((λ '/lambda)
                      (b '/beta)
                      (γ 'b)
                      (δ 'δ))
  (λ . /lambda)
  (γ . b)
  (δ . δ)
  ((λ a) . (/lambda a))
  ((λ (λ (λ (b)))) . (/lambda (/lambda (/lambda (/beta)))))
  ((-cmd γ) . (-cmd b)))

(assert-replacements lt-aliaslet* listex:alias car
                     ((λ '/lambda)
                      (b '/beta)
                      (γ 'b))
  (λ . /lambda)
  (γ . /beta)
  ((λ a) . (/lambda a))
  ((λ (λ (λ (γ)))) . (/lambda (/lambda (/lambda (/beta)))))
  ((-cmd γ) . (-cmd /beta)))

;; lt-macrolet test
(assert-replacements lt-macrolet listex:lisp-macro (lambda (x) (list (car x)))
                     ((λ () '(this and that))
                      (ι (n) `(+ ,@(cl-loop for i from 1 to n collect i)))
                      ;; keyword test
                      (circle (&key (radius 1) (diameter (* radius 2)))
                              `(-circle ,diameter))
                      (time (count &key m) `(,count ,(if m 'minutes 'seconds)))
                      ;; recursiveness test
                      (AB (n) `(AB 5 6 ,n))
                      (ABC (n) `(ABC (λ) 6 ,n)))
  ((λ) . (this and that))
  ((* (ι 3) (ι 2) (ι 1)) . (* (+ 1 2 3) (+ 1 2) (+ 1)))
  ;; keywords
  ((circle :radius 1) . (-circle 2))
  ((circle :diameter 1) . (-circle 1))
  ((time 1) . (1 seconds))
  ((time 1 :m t) . (1 minutes))
  ;; check recursiveness
  ((AB 1) . (AB 5 6 1))
  ((ABC 1) . (ABC (λ) 6 1)))

(assert-replacements lt-macrolet* listex:lisp-macro (lambda (x) (list (car x)))
                     ((λ () '(this and that))
                      (ABC (n) `(AB (λ) 6 ,n))
                      (ι (n) `(+ ,@(cl-loop for i from 1 to n collect i))))
  ((λ) . (this and that))
  ((* (ι 3) (ι 2) (ι 1)) . (* (+ 1 2 3) (+ 1 2) (+ 1)))
  ;; check recursiveness
  ((ABC 1) . (AB (this and that) 6 1)))

;; lt-macrolet test
(assert-replacements lt-cmdlet listex:lisp-macro (lambda (x) (list (car x)))
                     ((γ (u p) "\\lambda^{%s}_{%s}"))
  ((γ 1 (-frac 1 2)) . "\\lambda^{1}_{\\frac{1}{2}}"))


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
   ("\\left( \\i \\right)" . (lrp /i))))


;; lt-macrolet examples
(lt-macrolet ((Σ (up down) `(_ (^ (/sum /limits) ,up) ,down))
              (ω () `(5 6))
              (λ (a) `(-frac (-mathrm ,a)
                             (-mathbf ,a))))
  '(λ (+ (Σ (ω) (ω))
         (lrp (ω)))))
;; Tests:1 ends here
