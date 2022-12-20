(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defpackage aoc-2021-24
  (:use #:cl)
  (:local-nicknames (#:s #:split-sequence)
                    (#:a #:alexandria)))

(in-package :aoc-2021-24)

(defun read-from-input-file ()
  (with-open-file (in "24input" :direction :input)
    (loop
      for line = (read-line in nil)
      while line
      collect (mapcar #'read-from-string (s:split-sequence #\Space line)))))

(defparameter *reg-names* '(w x y z))

;; (defun extend-oper (a-val b-val oper)
;;   (cond
;;     ;; Extend a
;;     ((and (consp a-val)
;;           (eq (car a-val) oper))
;;      (cons oper (cons b-val (cdr a-val)))

;;      ;; Extend b
;;      ((and (consp b-val)
;;            (eq (car b-val) oper))
;;       (cons oper (cons a-val (cdr b-val))))

;;      (t (list oper a-val b-val)))))

(defun tree-depth (tree)
  (if (consp tree)
      (1+ (apply #'max (mapcar #'tree-depth tree)))
      1))

(defun range (expr env)
  (cond
    ((numberp expr) (cons expr expr))
    ((keywordp expr) '(1 . 9))
    ((consp expr)
     (destructuring-bind (l1 . u1) (range (second expr) env)
       (destructuring-bind (l2 . u2) (range (third expr) env)
         (case (first expr)
           (+ (cons (+ l1 l2) (+ u1 u2)))
           (* (loop
                for v in (list (* l1 l2) (* l1 u2) (* u1 l2) (* u1 u2))
                maximizing v into u
                minimizing v into l
                finally (return (cons l u))))
           (/ (cons (truncate u1 l2)
                    (truncate l1 u2)))
           (mod (cons 0 (min u1 u2)))
           (eql '(0 . 1))))))
    (t (range (cdr (assoc expr env)) env))))

(defun no-overlap (r1 r2)
  (destructuring-bind (l1 . u1) r1
    (destructuring-bind (l2 . u2) r2
      (or (> l2 u1) (> l1 u2)))))

(defun always-same (r1 r2)
  (destructuring-bind (l1 . u1) r1
    (destructuring-bind (l2 . u2) r2
      (and (eql l1 u1)
           (eql l1 l2)
           (eql l1 u2)))))

(defun treeify-input ()
  (loop
    with env = (pairlis *reg-names* (list 0 0 0 0))
    with input-vals = (loop for i from 0 to 14
                            collect (intern (format nil "D~a" i) :keyword))
    with var-num = 0
    for (op a-reg b-reg) in (read-from-input-file)
    do (let* ((b-val (a:if-let (reg-entry (assoc b-reg env))
                       (cdr reg-entry)
                       b-reg))
              (b (if (consp b-val)
                     (let ((b-sym (intern (format nil "~a~a" b-reg (incf var-num)))))
                       (push (cons b-sym b-val) env)
                       b-sym)
                     b-val))
              (a (cdr (assoc a-reg env))))
         (setf (cdr (assoc a-reg env))
               (case op
                 (inp (pop input-vals))
                 (add (cond
                        ((eql a 0) b) ; a + 0 = a
                        ((eql b 0) a) ; b + 0 = b
                        ((and (numberp a) (numberp b)) (+ a b)) ; add actual numbers
                        (t `(+ ,a ,b))))
                 (mul (cond
                        ((or (eql a 0) (eql b 0)) 0) ; 0 * a = 0
                        ((eql a 1) b) ; 1 * a = a
                        ((eql b 1) a) ; 1 * b = b
                        ((and (numberp a) (numberp b)) (* a b)) ; multiply actual numbers
                        (t `(* ,a ,b))))
                 (div (cond
                        ((eql a 0) 0) ; 0 / a = 0
                        ((eql b 1) a) ; a / 1 = a
                        (t `(/ ,a ,b))))
                 (mod (if (eql a 0) ; 0 mod b = 0
                          0
                          `(mod ,a ,b)))
                 (eql (let ((r1 (range a env))
                            (r2 (range b env)))
                        (cond
                         ((always-same r1 r2) 1)
                         ((no-overlap r1 r2) 0)
                         (t `(eql ,a ,b))))))))
    finally (return env)))
