;;; A Little Scheme in Common Lisp, v0.4 R01.12.08/R02.04.12 by SUZUKI Hisao

(defconstant +command-line-args+
  (or #+CLISP *args*			; clisp scm.l
      #+SBCL (cdr *posix-argv*)		; sbcl --script scm.l
      #+ECL (cdddr (ext:command-args))	; ecl --shell scm.l
      #+GCL (cdr si::*command-args*)	; gcl -f scm.l
      #+ABCL extensions:*command-line-argument-list*
					; abcl --batch --noinform --load scm.l
      nil))

;; Environment: ((symbol . value)...)
;; (look-up 'v env) returns (v . value)

(defun look-up (KEY LIST)
  (or (assoc KEY LIST :test #'eq)
      (error "~:A not found" KEY)))

(defstruct Continuation
  "Scheme's contiuation"
  stack)

(defstruct SStep
  "Scheme's step in a continuation"
  op val)

(defun push-Step (OP VAL K)
  "Push a step of OP and VAL to a continuation K."
  (push (make-SStep :op OP :val VAL) (Continuation-stack K)))

(defun pop-Step (K)
  "Pop a step from a continuation K."
  (pop (Continuation-stack K)))

(defun push-RestoreEnv (ENV K)
  "Push :RestoreEnv to a continuation K unless on a tail call."
  (let ((stack (Continuation-stack K)))
    (if (or (null stack)
	    (not (eq (SStep-op (car stack)) :RestoreEnv)))
	(push-Step :RestoreEnv ENV K))))

;;----------------------------------------------------------------------

(defstruct Expr
  "Expression, i.e., Lambda expression with its environment"
  params body env)

(defstruct Subr
  "Subroutine, i.e. Intrinsic function"
  name arity fun)

(defun __ (NAME ARITY FUN)
  (cons NAME (make-Subr :name NAME :arity ARITY :fun FUN)))

(defun __bool (X)
  (if X :True :False))

(defvar *global-env* nil)

(defun globals ()
  "Return a list of symbols of the global environment."
  (reverse (mapcar #'car (cdr *global-env*)))) ; take cdr to skip the marker.

(setq *global-env*
      (list
       nil				; marker of the frame top
       (__ 'car 1 #'car)
       (__ 'cdr 1 #'cdr)
       (__ 'cons 2 #'cons)
       (__ 'eq? 2 (lambda (x y) (__bool (eq x y))))
       (__ 'pair? 1 (lambda (x) (__bool (consp x))))
       (__ 'null? 1 (lambda (x) (__bool (null x))))
       (__ 'not 1 (lambda (x) (__bool (eq x :False))))
       (__ 'list -1 #'list)
       (__ 'display 1 (lambda (x) (princ x)
			:None))
       (__ 'newline 0 (lambda () (terpri)
			:None))
       (__ 'read 0 (lambda () (read-expression "")))
       (__ 'eof-object? 1 (lambda (x) (__bool (eq x :EOF))))
       (__ 'symbol? 1 (lambda (x) (__bool (and (symbolp x)
					       (not (null x))
					       (not (keywordp x))))))
       (cons 'call/cc :<call/cc>)
       (cons 'apply :<apply>)
       (__ '+ 2 #'+)
       (__ '- 2 #'-)
       (__ '* 2 #'*)
       (__ '< 2 (lambda (x y) (__bool (< x y))))
       (__ '= 2 (lambda (x y) (__bool (= x y))))
       (__ 'number? 1 (lambda (x) (__bool (numberp x))))
       (__ 'error 2 (lambda (x y) (error (format nil "Error: ~:A: ~:S" x y))))
       (__ 'globals 0 #'globals)))

;;----------------------------------------------------------------------

(defun evaluate (EXP ENV)
  "Evaluate an expression EXP in an environment ENV."
  (let ((k (make-Continuation)))
    (loop
     (loop named Loop1 do
      (cond ((consp EXP)
	     (let ((kar (car EXP))
		   (kdr (cdr EXP)))
	       (case kar
		 ((quote)		; (quote e)
		  (setq EXP (car kdr))
		  (return-from Loop1))
		 ((if)			; (if e1 e2 e3) or (if e1 e2)
		  (setq EXP (car kdr))
		  (push-Step :Then (cdr kdr) k))
		 ((begin)		; (begin e...)
		  (setq EXP (car kdr))
		  (unless (null (cdr kdr))
		    (push-Step :Begin (cdr kdr) k)))
		 ((lambda)		; (lambda (v...) e...)
		  (setq EXP (make-Expr :params (car kdr)
				       :body (cdr kdr)
				       :env ENV))
		  (return-from Loop1))
		 ((define)		; (define v e)
		  (setq EXP (cadr kdr))
		  (push-Step :Define (car kdr) k))
		 ((set!)		; (set! v e)
		  (setq EXP (cadr kdr))
		  (push-Step :Setq (look-up (car kdr) ENV) k))
		 (otherwise		; (fun arg...)
		  (setq EXP kar)
		  (push-Step :Apply kdr k)))))
	    ((and (symbolp EXP)
		  (not (keywordp EXP)))
	     (setq EXP (cdr (look-up EXP ENV)))
	     (return-from Loop1))
	    (t				; a number, #t, #f etc.
	     (return-from Loop1))))
     (loop named Loop2 do
	   ;; (princ "_") (princ (length (Continuation-stack k)))
	   (if (null (Continuation-stack k))
	       (return EXP))
	   (let* ((step (pop-Step k))
		  (op (SStep-op step))
		  (x (SStep-val step)))
	     (case op
	       ((:Then)			; x = (e2 e3)
		(if (eq EXP :False)
		    (if (null (cdr x))
			(setq EXP :None)
		      (progn
			(setq EXP (cadr x)) ; e3
			(return-from Loop2)))
		  (progn
		    (setq EXP (car x))	; e2
		    (return-from Loop2))))
	       ((:Begin)		; x = (e...)
		(unless (null (cdr x))
		  (push-Step :Begin (cdr x) k)) ; unless on a tail call.
		(setq EXP (car x))
		(return-from Loop2))
	       ((:Define)		; x = v
		;; (car ENV) should be nil here.
		(setf (cdr ENV)
		      (cons (cons x EXP) (cdr ENV)))
		(setq EXP :None))
	       ((:Setq)			; x = (v . e)
		(setf (cdr x) EXP)
		(setq EXP :None))
	       ((:Apply)		; x = arg...; EXP = function
		(if (null x)
		    (let ((pair (apply-function EXP nil k ENV)))
		      (setq EXP (car pair)
			    ENV (cdr pair)))
		  (progn
		    (push-Step :ApplyFun EXP k)
		    (loop until (null (cdr x)) do
			  (push-Step :EvalArg (car x) k)
			  (setq x (cdr x)))
		    (setq EXP (car x))
		    (push-Step :PushArg nil k)
		    (return-from Loop2))))
	       ((:PushArg)		; x = evaluated arg...
		(let ((args (cons EXP x))
		      (step (pop-Step k)))
		  (setq op (SStep-op step)
			EXP (SStep-val step))
		  (case op
		    ((:EvalArg)		; EXP = the next arg
		     (push-Step :PushArg args k)
		     (return-from Loop2))
		    ((:ApplyFun)	; EXP = evaluated function
		     (let ((pair (apply-function EXP args k ENV)))
		       (setq EXP (car pair)
			     ENV (cdr pair))))
		    (otherwise
		     (error "unexpected op: ~:S" op)))))
	       ((:RestoreEnv)		; x = environment
		(setq ENV x))
	       (otherwise
		(error "bad op: ~:S" op))))))))


(defun apply-function (FUN ARG K ENV)
  "Apply a function FUN to arguments ARG with a continuation K.
   ENV will be referred to push RestoreEnvOp to the continuation."
  (loop named L1 do
	(cond ((eq FUN :<call/cc>)
	       (push-RestoreEnv ENV K)
	       (setq FUN (car ARG))
	       (setq ARG (list (make-Continuation
				:stack (Continuation-stack K)))))
	      ((eq FUN :<apply>)
	       (setq FUN (car ARG))
	       (setq ARG (cadr ARG)))
	      (t
	       (return-from L1))))
  (cond ((Subr-p FUN)
	   (if (and (>= (Subr-arity FUN) 0)
		    (/= (Subr-arity FUN) (length ARG)))
	       (error "arity not matched ~:S ~:S" FUN ARG))
	   (cons (apply (Subr-fun FUN) ARG)
		 ENV))
	((Expr-p FUN)
	 (push-RestoreEnv ENV K)
	 (push-Step :Begin (Expr-body FUN) K)
	 (cons :None
	       (cons nil		; marker of the frame top
		     (pairlis (Expr-params FUN) ARG
			      (Expr-env FUN)))))
	((Continuation-p FUN)
	 (setf (Continuation-stack K) (copy-list (Continuation-stack FUN)))
	 (cons (car ARG)
	       ENV))
	(t
	 (error "not a function ~:S ~:S" FUN ARG))))

;;----------------------------------------------------------------------

;; #t will be read as :True; #f will be read as :False.
(set-dispatch-macro-character #\# #\t
			      #'(lambda (stream ch n) :True))
(set-dispatch-macro-character #\# #\f
			      #'(lambda (stream ch n) :False))

(defun read-expression (prompt)
  "Read an expression from the standard input."
  (princ prompt)
  (force-output)
  (read t nil :EOF))

(defun read-eval-print-loop ()
  "Repeat Read-Eval-Print until End-Of-File."
  (loop named L1 do
	(handler-case
	    (let ((exp (read-expression "> ")))
	      (when (eq exp :EOF)
		(format t "Goodbye~%")
		(return-from L1))
	      (let ((result (evaluate exp *global-env*)))
		(unless (eq result :None)
		  (format t "~:S~%" result))))
	  (error (c)
		 (format t "* ~:A~%" c)))))

(defun load-scheme-script (FILENAME)
  "Load a Scheme script from the file named FILENAME."
  (with-open-file (rf FILENAME)
		  (loop named L1 do
			(let ((exp (read rf nil :EOF)))
			  (if (eq exp :EOF)
			      (return-from L1))
			  (evaluate exp *global-env*)))))

;; The main routine
(if (null +command-line-args+)
    (read-eval-print-loop)
  (progn
    (load-scheme-script (car +command-line-args+))
    (if (equal "-" (cadr +command-line-args+))
	(read-eval-print-loop))))
