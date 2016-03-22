;;; forth.scm
;;; インタープリタにしましょう
;;; token 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEXER?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (space-char? char)
  (char-set-contains? char-set:whitespace char))

(define (skip-whitespaces)
  (let ([ch (peek-char)])
    (cond [(eof-object? ch) (read-char) #f]
          [(space-char? ch) (read-char) (skip-whitespaces)]
          [else 'done])))

(define (get-token #!optional (port (current-input-port)))
  (let ([next-char (peek-char)])
    (cond [(eof-object? next-char) #f]
          [(skip-whitespaces)           ; space 読み飛ばし
           (let loop ([first-char (peek-char)]
                      [str ""])
             (cond
              [(or (eof-object? first-char) (space-char? first-char)) str]
              [else
               (read-char)
               (loop (peek-char) (conc str first-char))]))]
          [else #f])))

(define-values (next-token current-token)
  (let ([tkn #f])
    (values (lambda () (set! tkn (get-token)) tkn)
            (lambda () tkn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STACK 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-stack depth)
  (let ([stk (make-vector depth 0)]
        [pointer 0])
    (define (push! val)
      (vector-set! stk pointer val)
      (set! pointer (add1 pointer)))
    (define (pop!)
      (set! pointer (sub1 pointer))
      (vector-ref stk pointer))
    (define (clear)
      (set! pointer 0))
    (define (show)
      (printf "<~A> " pointer)
      (do ([i 0 (add1 i)])
          ((<= pointer i) (newline))
        (printf "~A " (vector-ref stk i))))
    (values push! pop! clear show)))

(define-values (d-push! d-pop! d-clear! d-show) (make-stack 30000))
(define-values (r-push! r-pop! r-clear! _) (make-stack 128))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DICTIONARY 辞書の検索とか
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-entry name proc)
  (cons name proc))
(define (entry-name entry)
  (car entry))
(define (entry-body entry)
  (cdr entry))


(define (binary-op op)
  (lambda ()
    (let ([tos (d-pop!)]
          [sos (d-pop!)])
      (d-push! (op sos tos)))))
(define global-dictonary
  (list
   (make-entry ".s" d-show)
   (make-entry "."  (lambda ()
                      (display (d-pop!))
                      (newline)))
   (make-entry "dup" (lambda ()
                       (let ([val (d-pop!)])
                         (d-push! val)
                         (d-push! val))))
   (make-entry "clear" d-clear!)
   (make-entry "+" (binary-op +))
   (make-entry "*" (binary-op *))
   (make-entry "-" (binary-op -))
   (make-entry "/" (binary-op /))
   ))


(define (search-dictionary dict word-name)
  (cond [(assoc word-name dict) => (lambda (entry) ((entry-body entry)))]
        [(string->number word-name) => (lambda (num)
                                         (d-push! num))]
        [else (d-clear!)
              (display "ERROR: " word-name)
              (newline)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST DRIVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test str)
  (with-input-from-string str
    (lambda ()
      (let loop ([tkn (next-token)])
        (when tkn
          (pp tkn)
          (loop (next-token)))))))

(define (forth-load-file name)
  (with-input-from-file name
    (lambda ()
      (let loop ([tkn (next-token)])
        (when tkn
          (search-dictionary global-dictonary tkn)
          (loop (next-token)))))))
