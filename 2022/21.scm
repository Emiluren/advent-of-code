(use-modules (ice-9 rdelim)
             (ice-9 match)
             (ice-9 format))

(define (parse-line line)
  (let* ((sym-and-rule (string-split line #\:))
         (sym (string->symbol (list-ref sym-and-rule 0)))
         (rule (string-split (string-trim (list-ref sym-and-rule 1)) #\space)))
    (cons sym
          (match rule
            ((m1 op m2) (map string->symbol rule))
            ((num) (string->number num))))))

(define (parse-input)
  (let ((lines (string-split (string-trim-both (read-string)) #\newline)))
    (map parse-line lines)))

(define input (with-input-from-file "21input" parse-input))

(define (treeify-monkey monkey)
  (let ((m-val (cdr (assq monkey input))))
    (match m-val
      ((? number?) m-val)
      ((m1 op m2) (list op (treeify-monkey m1) (treeify-monkey m2))))))

(format #t "Part 1: ~a~%" (eval (treeify-monkey 'root) (interaction-environment)))

(define (treeify-humn monkey)
  (if (eq? monkey 'humn)
      'humn
      (let ((m-val (cdr (assq monkey input))))
        (match m-val
          ((? number?) m-val)
          ((m1 op m2) (list (if (eq? monkey 'root) '= op)
                            (treeify-humn m1)
                            (treeify-humn m2)))))))

(define (rev-op expr)
  (match expr
    (('= e1 e2) (list '= e2 e1))
    (('+ e1 e2) (list '+ e2 e1))
    (('* e1 e2) (list '* e2 e1))
    (('- e1 e2) (list '+ (- e2) e1))
    (('/ e1 e2) (list '* (/ 1 e2) e1))))

(define (simplify tree)
  (match tree
    ((? number?) tree)
    ('humn tree)
    ((op t1 t2) (let* ((t1-val (simplify t1))
                       (t2-val (simplify t2))
                       (expr (list op t1-val t2-val)))
                  (if (number? t1-val)
                      (if (number? t2-val)
                          (eval expr (interaction-environment))
                          expr)
                      (rev-op expr))))))

(define simplified (simplify (treeify-humn 'root)))

(define (solve res tree)
  (if (eq? tree 'humn)
      res
      (match tree
        (('+ n t2) (solve (- res n) t2))
        (('* n t2) (solve (/ res n) t2))
        (('- n t2) (solve (- n res) t2))
        (('/ n t2) (solve (/ n res) t2)))))

(format #t "Part 2: ~a~%" (solve (list-ref simplified 1) (list-ref simplified 2)))
