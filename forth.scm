;;; forth.scm
;;; インタープリタにしましょう
;;; token 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; state 
(define current-state (make-parameter 'interpret))
(define (compile?)
  (eq? (current-state) 'compile))
(define (interpret?)
  (eq? (current-state) 'interpret))

;;; for word definition
(define compile-stack (make-parameter '()))
(define (compile-stack-push! proc)
  (compile-stack (cons proc (compile-stack))))
(define (compile-stack-clear!)
  (compile-stack '()))

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

;;; 実行時のprocとcompile時のprocを指定
(define (make-entry name proc #!key
                    (immediate #f)
                    (compile-only #f))
  (list name proc immediate compile-only))

(define (entry-name entry)
  (first entry))
(define (entry-body entry)
  (second entry))
(define (entry-immediate? entry)
  (third entry))
(define (entry-compile-only? entry)
  (fourth entry))
(define (entry-execute entry)
  ((entry-body entry)))
(define-syntax make-dictionary
  (syntax-rules ()
    [(_ (name body ...) ...)
     (list (make-entry name body ...) ...)]))
(define-syntax dictionary-push!
  (syntax-rules ()
    [(_ entry dict)
     (set! dict (cons entry dict))]))


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

;;; compile word ------------------------------
;;; : colon
(define (colon-define)
  (let ([name (next-token)])
    (cond [name
           (compile-stack-clear!)
           (compile-stack-push! name)   ; word name
           (current-state 'compile)     ; start compile
           ]
          [else 'done])))
;;; convert compile stack to entry
;;;  (proc ... name) -> (make-entry name new-proc)
(define (compile-stack->entry)
  (let* ([stk (reverse! (compile-stack))]
         [name (car stk)]
         [proc-list (cdr stk)]
         [body (lambda ()
                 (for-each (lambda (proc)
                             (proc))
                           proc-list))]
         [entry (make-entry name body)])
    entry))
;;; ; end compile
(define (semicolon)
  (dictionary-push! (compile-stack->entry) global-dictonary)
  (current-state 'interpret)
  'done)

;;; control structure ------------------------------
;;; if

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
   ("(" skip-comment
    #:immediate #t)
   (".(" display-comment
    #:immediate #t)
   ("\\" skip-online-comment
    #:immediate #t)
   ;; compile word
   ;("create" )
   (":" colon-define)
   (";" semicolon
    #:immediate #t)
   ;; if compile時の意味を考えればよい
   ;; endifまでを一纏めにする
   ;; ("if" (lambda ()
   ;;         (let loop ([tkn (next-token)]
   ;;                    [true-clause '()]
   ;;                    [false-clause '()])
   ;;           (when tkn
   ;;             (cond [(string-ci=? tkn "else")]))))
   ;;  #:immediate #t
   ;;  #:compile-only #t)
   ))

(define (search-dictionary dict word-name)
  (assoc word-name dict string-ci=?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERPRETER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (forth-abort . msgs)
  (d-clear!)
  (r-clear!)
  (display  "FORTH ERROR:" (current-error-port))
  (for-each (cut fprintf (current-error-port) " ~A" <>)
            msgs)
  (newline (current-error-port)))

(define (forth-interpret token dict)
  (cond [(search-dictionary dict token)
         => (lambda (entry) (entry-execute entry))]
        [(string->number token)
         => (lambda (num)
              (d-push! num))]
        [else (d-clear!)
              (forth-abort token)]))

(define (forth-compile token dict)
  (cond [(search-dictionary dict token)
         => (lambda (entry)
              (if (entry-immediate? entry)
                  (entry-execute entry)
                  (compile-stack-push! (entry-body entry))))]
        [(string->number token)
         => (lambda (num)
              (compile-stack-push! (lambda ()
                                  (d-push! num))))]
        [else (d-clear!)
              (forth-abort "compile state error" token)
              (current-state 'interpret)]))


(define (forth-eval-token token dict)
  (if (compile?)
      (forth-compile token dict)
      (forth-interpret token dict)))

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
