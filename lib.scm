
;
; lib.scm : micro Scheme 用簡易ライブラリ
;
;           Copyright (C) 2013 Makoto Hiroi
;
; 作成: 2017-11-30 ken3@nurs.or.jp
; 更新: 2017-12-01 ken3@nurs.or.jp

;; 2017-11-30  k_tsuka
;; Scheme的ではなく、古典LISPの書式を採用している。
;; ⇒ define の直後がシンボルかリストかで処理を分けて対応したい。

; 述語
(define not (lambda (x) (if x false true)))
(define null? (lambda (x) (eq? x '())))
(define eqv? eq?)
(define id (lambda (obj)           obj))
(define flip (lambda (func)        (lambda (arg1 arg2) (func arg2 arg1))))
(define curry (lambda (func arg1)  (lambda (arg) (func arg1 arg))))
(define compose (lambda (f g)      (lambda (arg) (f (g arg)))))

; 数
(define zero? (lambda (x) (= x 0)))
(define positive? (lambda (x) (< 0 x)))
(define negative? (lambda (x) (> 0 x)))
(define even? (lambda (x) (zero? (mod x 2))))
(define odd? (lambda (x) (not (even? x))))
(define abs (lambda (x) (if (negative? x) (- x) x)))
(define max
  (lambda (x . xs)
    (fold-left (lambda (a b) (if (< a b) b a)) x xs)))
(define min
  (lambda (x . xs)
    (fold-left (lambda (a b) (if (> a b) b a)) x xs)))

(define gcdi
  (lambda (a b)
    (if (zero? b)
        a
      (gcdi b (mod a b)))))
(define gcd
  (lambda xs
    (if (null? xs)
        0
      (fold-left (lambda (a b) (gcdi a b)) (car xs) (cdr xs)))))

(define lcmi (lambda (a b) (/ (* a b) (gcdi a b))))
(define lcm
  (lambda xs
    (if (null? xs)
        1
      (fold-left (lambda (a b) (lcmi a b)) (car xs) (cdr xs)))))

; cxxr
(define caar (lambda (xs) (car (car xs))))
(define cadr (lambda (xs) (car (cdr xs))))
(define cdar (lambda (xs) (cdr (car xs))))
(define cddr (lambda (xs) (cdr (cdr xs))))

; cxxxr
(define caaar (lambda (xs) (car (caar xs))))
(define caadr (lambda (xs) (car (cadr xs))))
(define cadar (lambda (xs) (car (cdar xs))))
(define caddr (lambda (xs) (car (cddr xs))))
(define cdaar (lambda (xs) (cdr (caar xs))))
(define cdadr (lambda (xs) (cdr (cadr xs))))
(define cddar (lambda (xs) (cdr (cdar xs))))
(define cdddr (lambda (xs) (cdr (cddr xs))))

; cxxxxr
(define caaaar (lambda (xs) (car (car (car (car xs))))))
(define caaadr (lambda (xs) (car (car (car (cdr xs))))))
(define caadar (lambda (xs) (car (car (cdr (car xs))))))
(define caaddr (lambda (xs) (car (car (cdr (cdr xs))))))
(define cadaar (lambda (xs) (car (cdr (car (car xs))))))
(define cadadr (lambda (xs) (car (cdr (car (cdr xs))))))
(define caddar (lambda (xs) (car (cdr (cdr (car xs))))))
(define cadddr (lambda (xs) (car (cdr (cdr (cdr xs))))))
(define cdaaar (lambda (xs) (cdr (car (car (car xs))))))
(define cdaadr (lambda (xs) (cdr (car (car (cdr xs))))))
(define cdadar (lambda (xs) (cdr (car (cdr (car xs))))))
(define cdaddr (lambda (xs) (cdr (car (cdr (cdr xs))))))
(define cddaar (lambda (xs) (cdr (cdr (car (car xs))))))
(define cddadr (lambda (xs) (cdr (cdr (car (cdr xs))))))
(define cdddar (lambda (xs) (cdr (cdr (cdr (car xs))))))
(define cddddr (lambda (xs) (cdr (cdr (cdr (cdr xs))))))

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth (lambda (xs) (car (cdddr xs))))
(define fifth  (lambda (xs) (cadr (cdddr xs))))

; リスト操作

(define list (lambda x x))

(define append-1
  (lambda (xs ys)
    (if (null? xs)
        ys
      (cons (car xs) (append-1 (cdr xs) ys)))))

(define append
  (lambda xs
    (if (null? xs)
        '()
      (if (null? (cdr xs))
          (car xs)
        (append-1 (car xs) (apply append (cdr xs)))))))

(define length
  (lambda (xs)
    (fold-left (lambda (a x) (+ a 1)) 0 xs)))

(define reverse
  (lambda (xs)
    (fold-left (lambda (a x) (cons x a)) '() xs)))

(define list-tail
  (lambda (xs k)
    (if (zero? k)
        xs
      (list-tail (cdr xs) (- k 1)))))

(define list-ref 
  (lambda (xs k)
    (if (zero? k)
        (car xs)
      (list-ref (cdr xs) (- k 1)))))

; リストの探索
(define memq
  (lambda (x ls)
    (if (null? ls)
        false
        (if (eq? x (car ls))
            ls
          (memq x (cdr ls))))))

(define memv
  (lambda (x ls)
    (if (null? ls)
        false
        (if (eqv? x (car ls))
            ls
          (memv x (cdr ls))))))

(define member
  (lambda (x ls)
    (if (null? ls)
        false
        (if (equal? x (car ls))
            ls
          (member x (cdr ls))))))

(define find
  (lambda (p xs)
    (if (null? xs)
        false
      (if (p (car xs))
          (car xs)
        (find p (cdr xs))))))

(define mem-helper (lambda (pred op)
  (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
(define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) false alist)))
(define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) false alist)))
(define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) false alist)))

; 高階関数
(define map-1
  (lambda (f xs)
    (if (null? xs)
        '()
      (cons (f (car xs)) (map f (cdr xs))))))

(define map
  (lambda (f . args)
    (if (memq '() args)
        '()
      (cons (apply f (map-1 car args))
            (apply map f (map-1 cdr args))))))

(define filter
  (lambda (p xs)
    (if (null? xs)
        '()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
        (filter p (cdr xs))))))

(define fold-left
  (lambda (f a xs)
    (if (null? xs)
        a
      (fold-left f (f a (car xs)) (cdr xs)))))
(define foldl  fold-left)
(define fold   fold-left)
(define reduce fold-left)

(define fold-right
  (lambda (f a xs)
    (if (null? xs)
        a
      (f (car xs) (fold-right f a (cdr xs))))))
(define foldr  fold-right)

(define unfold (lambda (func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred)))))

; any と every
(define any
  (lambda (p . xs)
    (if (memq '() xs)
        false
      (if (apply p (map car xs))
          true
        (apply any p (map cdr xs))))))
(define any? any)

(define every
  (lambda (p . xs)
    (if (memq '() xs)
        true
      (if (apply p (map car xs))
          (apply every p (map cdr xs))
        false))))
(define every? every)

(define sum (lambda(lst)
  (fold + 0 lst)))

(define product (lambda(lst)
  (fold * 1 lst)))

;;
;; マクロ
;;
(define unquote
  (lambda (x) (error "unquote appeared outside quasiquote")))

(define unquote-splicing
  (lambda (x) (error "unquote-splicing appeared outside quasiquote")))

(define translator-sub
  (lambda (sym ls n succ)
    (list 'list
          (list 'quote sym)
          (translator ls (+ n succ)))))

(define translator-unquote
  (lambda (ls n)
    (list 'cons
          (if (zero? n)
              (cadar ls)
            (translator-sub 'unquote (cadar ls) n -1))
          (translator (cdr ls) n))))

(define translator-unquote-splicing
  (lambda (ls n)
    (if (zero? n)
        (list 'append (cadar ls) (translator (cdr ls) n))
      (list 'cons
            (translator-sub 'unquote-splicing (cadar ls) n -1)
            (translator (cdr ls) n)))))

(define translator-quasiquote
  (lambda (ls n)
    (list 'cons
          (translator-sub 'quasiquote (cadar ls) n 1)
          (translator (cdr ls) n))))

(define translator-list
  (lambda (ls n)
    (if (eq? (caar ls) 'unquote)
        (translator-unquote ls n)
      (if (eq? (caar ls) 'unquote-splicing)
          (translator-unquote-splicing ls n)
        (if (eq? (caar ls) 'quasiquote)
            (translator-quasiquote ls n)
          (list 'cons
                (translator (car ls) n)
                (translator (cdr ls) n)))))))

(define translator-atom
  (lambda (ls n)
    (if (eq? (car ls) 'unquote)
        (if (zero? n)
            (cadr ls)
          (if (= n 1)
              (if (eq? (car (cadr ls)) 'unquote-splicing)
                  (list 'cons (list 'quote 'unquote) (cadr (cadr ls)))
                (translator-sub 'unquote (cadr ls) n -1))
            (translator-sub 'unquote (cadr ls) n -1)))
      (if (eq? (car ls) 'unquote-splicing)
          (if (zero? n)
              (error "invalid unquote-splicing form")
            (if (= n 1)
                (if (eq? (car (cadr ls)) 'unquote-splicing)
                    (list 'cons (list 'quote 'unquote-splicing) (cadr (cadr ls)))
                  (translator-sub 'unquote-splicing (cadr ls) n -1))
              (translator-sub 'unquote-splicing (cadr ls) n -1)))
        (if (eq? (car ls) 'quasiquote)
            (translator-sub 'quasiquote (cadr ls) n 1)
          (list 'cons 
                (list 'quote (car ls))
                (translator (cdr ls) n)))))))

(define translator
  (lambda (ls n)
    (if (pair? ls)
        (if (pair? (car ls))
            (translator-list ls n)
          (translator-atom ls n))
      (list 'quote ls))))

(define-macro quasiquote (lambda (x) (translator x 0)))

; let (named-let)
(define-macro let
  (lambda (args . body)
    (if (pair? args)
        `((lambda ,(map car args) ,@body) ,@(map cadr args))
      ; named-let
      `(letrec ((,args (lambda ,(map car (car body)) ,@(cdr body))))
        (,args ,@(map cadr (car body)))))))

; and
(define-macro and
  (lambda args
    (if (null? args)
        true
      (if (null? (cdr args))
          (car args)
        `(if ,(car args) (and ,@(cdr args)) false)))))

; or
(define-macro or
  (lambda args
    (if (null? args)
        false
      (if (null? (cdr args))
          (car args)
        `(let ((+value+ ,(car args)))
          (if +value+ +value+ (or ,@(cdr args))))))))

; let*
(define-macro let*
  (lambda (args . body) 
    (if (null? (cdr args))
        `(let (,(car args)) ,@body)
      `(let (,(car args)) (let* ,(cdr args) ,@body)))))

; letrec
(define-macro letrec
  (lambda (args . body)
    (let ((vars (map car args))
          (vals (map cadr args)))
      `(let ,(map (lambda (x) `(,x '*undef*)) vars)
            ,@(map (lambda (x y) `(set! ,x ,y)) vars vals)
            ,@body))))

; begin
(define-macro begin
  (lambda args
    (if (null? args)
        `((lambda () '*undef*))
      `((lambda () ,@args)))))

; cond
(define-macro cond
  (lambda args
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        (if (null? (cdar args))
            (caar args)
          `(if ,(caar args)
               (begin ,@(cdar args))
            (cond ,@(cdr args))))))))

; case
(define-macro case
  (lambda (key . args)
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        `(if (memv ,key ',(caar args))
             (begin ,@(cdar args))
           (case ,key ,@(cdr args)))))))

; do
(define-macro do
  (lambda (var-form test-form . args)
    (let ((vars (map car var-form))
          (vals (map cadr var-form))
          (step (map cddr var-form)))
      `(letrec ((loop (lambda ,vars
                        (if ,(car test-form)
                            (begin ,@(cdr test-form))
                          (begin
                            ,@args
                            (loop ,@(map (lambda (x y)
                                           (if (null? x) y (car x)))
                                           step
                                           vars)))))))
        (loop ,@vals)))))


