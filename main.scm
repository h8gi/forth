;;; forth.scm
;;; インタープリタにしましょう
;;; token
(import-for-syntax matchable)
(use srfi-14 coops)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; state 
(define current-state (make-parameter 'interpret))
(define (compile?)
  (eq? (current-state) 'compile))
(define (interpret?)
  (eq? (current-state) 'interpret))
(define (compile-mode)
  (current-state 'compile))
(define (interpret-mode)
  (current-state 'interpret))

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
              [(or (eof-object? first-char) (space-char? first-char))
               (read-char) str]
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
      (cond [(> depth pointer)
             (vector-set! stk pointer val)
             (set! pointer (add1 pointer))
             val]
            [else
             (forth-abort "Error" "Stack overflow")]))
    (define (pop!)
      (cond [(> pointer 0)
             (set! pointer (sub1 pointer))
             (vector-ref stk pointer)]
            [else
             (forth-abort "Error" "Stack underflow")]))
    (define (clear)
      (set! pointer 0))
    (define (show)
      (printf "<~A> " pointer)
      (do ([i 0 (add1 i)])
          ((<= pointer i))
        (printf "~A " (vector-ref stk i))))
    (define (stack-ref i)
      (vector-ref stk i))
    (define (stack-set! i val)
      (vector-set! stk i val))
    (define p
      (lambda () pointer))
    (define (copy start end)
      (let ([vect (make-vector (- end start))])
        (do ([i start (add1 i)]
             [c 0 (add1 c)])
            ((<= end i) vect)
          (vector-set! vect c (vector-ref stk i)))))
    (values push! pop! clear show stack-ref stack-set! p copy)))

(define-syntax define-stack
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       [(_ name len)
        (let ([name-list (map (compose string->symbol (lambda (desc)
                                                        (conc (inject name) "-" desc)))
                              '(push! pop! clear! show ref set! pointer copy))])
          `(define-values ,name-list (make-stack ,len)))]))))

(define-stack d 30000)
(define-stack r 128)
(define-stack compile-stack 30000)
(define memory (make-vector 50000 0))
;;; for word definition
;;; convert compile stack to entry
;;;  (proc ... name) -> (make-entry name new-proc)
;;; 各ワード呼び出しで*pc*を使う
(define *pc* 0)
(define (compile-stack->entry)
  (let ([name (compile-stack-ref 0)]
        [vect (compile-stack-copy 1 (compile-stack-pointer))]
        [lim  (sub1 (compile-stack-pointer))])
    (make-entry name
                (lambda ()
                  (set! *pc* 0)
                  (let loop ()
                    (cond [(< *pc* lim)
                           ((vector-ref vect *pc*))
                           (set! *pc* (add1 *pc*))
                           (loop)]
                          [else 'done]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DICTIONARY 辞書の検索とか
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <entry> ()
  ([name #:reader entry-name]
   [body #:reader entry-body]
   [compilation #:accessor entry-compilation
		#:initform #f]
   [immediate #:accessor entry-immediate?
	      #:initform #f]
   [compile-only #:accessor entry-compile-only?
		 #:initform #f]))
;;; 実行時のprocとcompile時のprocを指定
(define (make-entry name body #!key
		    (immediate #f)
		    (compile-only #f)
		    (compilation #f))
  (make <entry>
    'name name
    'body body
    'compilation compilation
    'immediate immediate
    'compile-only compile-only))

(define (entry-execute entry)
  ((entry-body entry)))
(define (entry-set-immediate! entry)
  (set! (entry-immediate? entry) #t))
(define (entry-set-compile-only! entry)
  (set! (entry-compile-only? entry) #t))

(define-syntax make-dictionary
  (syntax-rules ()
    [(_ (name body ...) ...)
     (list (make-entry name body ...) ...)]))
(define-syntax dictionary-push!
  (syntax-rules ()
    [(_ entry)
     (set! global-dictionary (cons entry global-dictionary))]))

;;; 2項演算子用 -や/の順序に対応
(define (binary-op op)
  (lambda ()
    (let ([tos (d-pop!)]
          [nos (d-pop!)])
      (d-push! (op nos tos)))))

;;; (コメント 閉括弧はスペースいらず
(define (skip-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? ch #\)))
           'done]
          [else (skip-comment)])))
;;; .(コメント
(define (display-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? ch #\)))
           'done]
          [else (display ch) (display-comment)])))

;;; \コメント
(define (skip-online-comment)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? #\newline ch))
           'done]
          [else (skip-online-comment)])))
;;; ."
(define (display-string)
  (let ([ch (read-char)])
    (cond [(or (eof-object? ch)
               (char=? ch #\"))
           'done]
          [else (display ch) (display-string)])))

(define (compile-display-string)
  (let loop ([ch (read-char)]
             [str ""])
    (cond [(or (eof-object? ch)
               (char=? ch #\"))
           (compile-stack-push! (lambda () (display str)))]
          [else (loop (read-char) (conc str ch))])))

;;; compile word ------------------------------
;;; : colon
(define (colon-define)
  (let ([name (next-token)])
    (cond [name
           (compile-stack-clear!)
           (compile-stack-push! name)   ; word name
           (compile-mode)     ; start compile
           ]
          [else 'done])))

(define-syntax define-forth-word
  (syntax-rules ()
    [(_ name tkn ...)
     (begin
       (with-input-from-string name
         (lambda () (forth-eval-token ":")))
       (for-each (cut forth-eval-token <>) '(tkn ...))
       (forth-eval-token ";"))]))

;;; ; end compile
(define (semicolon)
  (dictionary-push! (compile-stack->entry))
  (interpret-mode)
  'done)

;;; for compile number
(define (compile-num num)
  (compile-stack-push!
   (lambda () (d-push! num))))

;;; literal
(define (literal)
  (compile-num (d-pop!)))

(define (forth-false?)
  (zero? (d-pop!)))
(define (forth-true?)
  (complement forth-false?))
;;; control structure ------------------------------
;;; if
(define global-dictionary  
  (make-dictionary
   ;; stack manipulate
   (".s" d-show)
   ("."  (lambda ()
           (printf "~A " (d-pop!))))
   ("drop" (lambda ()
             (d-pop!)))
   ("dup" (lambda ()
            (let ([val (d-pop!)])
              (d-push! val)
              (d-push! val))))
   ("swap" (lambda ()
             (let ([tos (d-pop!)]
                   [nos (d-pop!)])
               (d-push! tos)
               (d-push! nos))))
   ("clear" d-clear!)
   ;; arithmatic op
   ("+" (binary-op +))
   ("*" (binary-op *))
   ("-" (binary-op -))
   ("/" (binary-op /))
   ("<" (binary-op (compose (lambda (b) (if b -1 0))
                            <)))
   ("<=" (binary-op (compose (lambda (b) (if b -1 0))
                             <=)))
   (">" (binary-op (compose (lambda (b) (if b -1 0))
                            >)))
   (">=" (binary-op (compose (lambda (b) (if b -1 0))
                             >=)))
   ("=" (binary-op (compose (lambda (b) (if b -1 0))
                            =)))    
   ;; comment
   ("(" skip-comment
    #:immediate #t)
   (".(" display-comment
    #:immediate #t)
   ("\\" skip-online-comment
    #:immediate #t)
   (".\"" display-string
    #:compilation compile-display-string)
   ;; words
   ("words" (lambda ()
   	      (for-each (compose (cut printf "~A " <>) entry-name)
			global-dictionary)))
   ;; compile word   
   (":" colon-define)
   (";" semicolon
    #:immediate #t
    #:compile-only #t)
   ;; (n -- )
   (">r" (lambda ()
           (r-push! (d-pop!))))
   ;; ( -- n)
   ("r>" (lambda ()
           (d-push! (r-pop!))))
   ;; (-- n)
   ("r@" (lambda ()
           (d-push! (r-push! (r-pop!)))))
   ;; if compile時の意味を考えればよい
   ;; 「0であれば次のelse or thenまでジャンプ」をコンパイルする
   ("if" (lambda ()
           (r-push! (compile-stack-pointer))
           (compile-stack-push!
            (lambda (jump-to)
              (lambda ()
                (when (forth-false?)
                  (set! *pc* jump-to))))))
    #:immediate #t
    #:compile-only #t)
   ;; return stack の位置にある 関数へ 飛び先(ココ)を教える
   ("then" (lambda ()
             (let ([if-or-else-pos (r-pop!)]
                   [then-pos (- (compile-stack-pointer) 2)]) ; 直後のadd1と、nameの分?
               (compile-stack-set! if-or-else-pos ((compile-stack-ref if-or-else-pos) then-pos))
               'done))
    #:immediate #t
    #:compile-only #t)
   ;; return stak の位置にある 関数へ 飛び先を教えつつ、
   ("else" (lambda ()
	     (let ([if-pos (r-pop!)]
		   [else-pos (- (compile-stack-pointer) 1)] ; else直後に そこからはジャンプしない
		   )
	       (r-push! (compile-stack-pointer))
	       (compile-stack-set! if-pos ((compile-stack-ref if-pos) else-pos))
	       (compile-stack-push!
		(lambda (jump-to)
		  (lambda ()
		    (set! *pc* jump-to))))))
    #:immediate #t
    #:compile-only #t)
   ;; do
   ("do" (lambda ()
           (r-push! (compile-stack-pointer))
           (compile-stack-push!
            (lambda (jump-to)
              (lambda ()
                (r-push! (d-pop!))
                (set! *pc* jump-to))))))
   ;; loop
   ("loop" (lambda ()
             (let ([do-pos (r-pop!)]
                   [loop-pos (- (compile-stack-pointer) 2)])
               (compile-stack-set! do-pos ((compile-stack-ref do-pos) loop-pos))
               (compile-stack-push!
                (lambda ()
                  (let [(start (d-pop!))
                        (limit (d-pop!))]
                    (unless (< limit start)
                      (set! *pc* (sub1 do-pos))
                      (r-push! limit)
                      (r-push! (add1 start)))))))))

   ;; 10 0 do code .. loop hoge ;
   ("[" interpret-mode
    #:immediate #t)
   ("]" compile-mode)
   ("literal" literal
    #:immediate #t
    #:compile-only #t)

   ("immediate" (lambda ()
                  (entry-set-immediate! (car global-dictionary))))
   ("compile-only" (lambda ()
		     (entry-set-compile-only! (car global-dictionary))))
   ;; 実行時の挙動
   ("postpone" (lambda ()
                 (let ([tkn (next-token)])
                   (if tkn
                       (compile-stack-push!
                        ;; コンパイル時の挙動を定義にcompile
                        (lambda () (forth-compile tkn)))
                       'done)))
    #:immediate #t)
   ("constant" (lambda ()
                 (let ([name (next-token)])
                   (dictionary-push!
                    (make-entry name (let ([val (d-pop!)])
                                       (lambda () (d-push! val))))))))))

(define (search-dictionary word-name)
  (find (lambda (x) (string-ci=? word-name (entry-name x)))
	global-dictionary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERPRETER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (forth-abort . msgs)
  (d-clear!)
  (r-clear!)
  (interpret-mode)
  (display  "FORTH ERROR:" (current-error-port))
  (for-each (cut fprintf (current-error-port) " ~A" <>)
            msgs)
  (newline (current-error-port)))

(define (forth-abort . msgs)
  (d-clear!)
  (r-clear!)
  (interpret-mode)
  (apply error msgs))

(define (forth-interpret token)
  (cond [(search-dictionary token)
         => (lambda (entry)
	      (if (entry-compile-only? entry)
		  (forth-abort "Compile only word" token)
		  (entry-execute entry)))]
        [(string->number token)
         => (lambda (num)
              (d-push! num))]
        [else (d-clear!)
              (forth-abort "Undefined word" token)]))

(define (forth-compile token)
  (cond [(search-dictionary token)
         => (lambda (entry)
              (cond [(entry-immediate? entry)
                     (entry-execute entry)]
                    [(entry-compilation entry) => (lambda (xt) (xt))]
                    [else (compile-stack-push! (entry-body entry))]))]
        [(string->number token)
         => compile-num]
        [else (d-clear!)
              (forth-abort "Compile error" token)]))

(define (forth-eval-token token)
  (if (compile?)
      (forth-compile token)
      (forth-interpret token)))

(define (forth-loop)
  (let loop ([tkn (next-token)])
    (when tkn
      (forth-eval-token tkn)
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
    forth-loop)
  (display " ok")
  (newline))

(forth-eval-string "0 constant false")
(forth-eval-string "-1 constant true")
(forth-eval-string ": endif postpone then ; immediate compile-only")
