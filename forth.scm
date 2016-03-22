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
(define-values (r-push! r-pop! r-clear! r-show) (make-stack 128))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DICTIONARY 辞書の検索とか
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-entry name proc)
  (cons name proc))
(define (entry-name entry)
  (car entry))
(define (entry-body entry)
  (cdr entry))

;;; 2項演算子用 -や/の順序に対応
(define (binary-op op)
  (lambda ()
    (let ([tos (d-pop!)]
          [sos (d-pop!)])
      (d-push! (op sos tos)))))

;;; (コメント 閉括弧はスペースいらず
(define (skip-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? #\) ch))
           'done]
          [else (skip-comment)])))
;;; .(コメント
(define (display-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? #\) ch))
           'done]
          [else (display ch) (display-comment)])))
;;; \コメント
(define (skip-online-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? #\newline ch))
           'done]
          [else (skip-online-comment)])))

(define-syntax make-dictionary
  (syntax-rules ()
    [(_ (name body) ...)
     (list (make-entry name body) ...)]))

(define global-dictonary  
  (make-dictionary
   ;; stack manipulate
   (".s" d-show)
   ("."  (lambda ()
           (display (d-pop!))
           (newline)))
   ("dup" (lambda ()
            (let ([val (d-pop!)])
              (d-push! val)
              (d-push! val))))
   ("clear" d-clear!)
   ;; arithmatic op
   ("+" (binary-op +))
   ("*" (binary-op *))
   ("-" (binary-op -))
   ("/" (binary-op /))
   ;; comment
   ("(" skip-comment)
   (".(" display-comment)
   ("\\" skip-online-comment)
   ))

(define (search-dictionary dict word-name)
  (assoc word-name dict string-ci=?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERPRETER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (forth-abort #!optional (msg ""))
  (d-clear!)
  (r-clear!)
  (fprintf (current-error-port) "FORTH ERROR: ~A\n" msg))

(define (forth-eval-token token dict)
  (cond [(search-dictionary dict token)
         => (lambda (entry) ((entry-body entry)))]
        [(string->number token)
         => (lambda (num)
              (d-push! num))]
        [else (d-clear!)
              (forth-abort token)
              (newline)]))

(define (forth-loop)
  (let loop ([tkn (next-token)])
    (when tkn
      (forth-eval-token tkn global-dictonary)
      (loop (next-token)))))


(define (lexer-test str)
  (with-input-from-string str
    (lambda ()
      (let loop ([tkn (next-token)])
        (when tkn
          (pp tkn)
          (loop (next-token)))))))


(define (forth-load-file name)
  (with-input-from-file name
    forth-loop))

(define (forth-eval-string str)
  (with-input-from-string str
    forth-loop))
