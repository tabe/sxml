(library (sxml ssax)
  (export ssax:xml->sxml)
  (import (rnrs (6))
          (rnrs mutable-strings)
          (only (srfi :1) fold)
          (only (srfi :13)
                string-concatenate-reverse
                string-concatenate-reverse/shared
                string-concatenate/shared
                string-index
                string-null?))

(define (call-with-input-string str proc)
  (call-with-port (open-string-input-port str) proc))

(define (inc i) (+ i 1))
(define (dec i) (- i 1))

(define ascii->char integer->char)
(define char-newline #\linefeed)
(define char-return #\return)
(define ucscode->char integer->char)

(define-syntax begin0
  (syntax-rules ()
    ((_ e0 e1 ...)
     (let ((r e0)) e1 ... r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; output.scm

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
		  (x (current-error-port))
		  (display x (current-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ssax:warn port msg . other-msg)
  (apply cerr (cons (string-append (string #\newline) "Warning: ")
		    (cons msg
			  other-msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implementation of the structure parser-errors-vanilla
; for systems other than Scheme48 and PLT Scheme
; $Id: parser-errors-vanilla.scm,v 1.1 2004/08/06 23:01:09 oleg Exp $

(define (parser-error port message . rest)
  (apply error message rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -- Function: find-string-from-port? STR IN-PORT MAX-NO-CHARS
;    Looks for a string STR within the first MAX-NO-CHARS chars of the
;    input port IN-PORT
;    MAX-NO-CHARS may be omitted: in that case, the search span would be
;    limited only by the end of the input stream.
;    When the STR is found, the function returns the number of
;    characters it has read from the port, and the port is set
;    to read the first char after that (that is, after the STR)
;    The function returns #f when the string wasn't found
; Note the function reads the port *STRICTLY* sequentially, and does not
; perform any buffering. So the function can be used even if the port is open
; on a pipe or other communication channel.
;
; Probably can be classified as misc-io.
;
; Notes on the algorithm.
; A special care should be taken in a situation when one had achieved a partial
; match with (a head of) STR, and then some unexpected character appeared in
; the stream. It'll be rash to discard all already read characters. Consider
; an example of string "acab" and the stream "bacacab...", specifically when
;    a  c  a _b_
; b  a  c  a  c  a  b ...
; that is, when 'aca' had matched, but then 'c' showed up in the stream
; while we were looking for 'b'. In that case, discarding all already read
; characters and starting the matching process from scratch, that is,
; from 'c a b ...', would miss a certain match.
; Note, we don't actually need to keep already read characters, or at least
; strlen(str) characters in some kind of buffer. If there has been no match,
; we can safely discard read characters. If there was some partial match,
; we already know the characters before, they are in the STR itself, so
; we don't need a special buffer for that.

;;; "MISCIO" Search for string from port.
; Written 1995 by Oleg Kiselyov (oleg@ponder.csci.unt.edu)
; Modified 1996 by A. Jaffer (jaffer@ai.mit.edu)
;
; This code is in the public domain.

(define (MISCIO:find-string-from-port? str <input-port> . max-no-char)
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (my-peek-char			; Return a peeked char or #f
	(lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
			(let ((c (peek-char <input-port>)))
			  (if (eof-object? c) #f c)))))
       (next-char (lambda () (read-char <input-port>)
			  (set! no-chars-read  (inc no-chars-read))))
       (match-1st-char			; of the string str
	(lambda ()
	  (let ((c (my-peek-char)))
	    (if (not c) #f
		(begin (next-char)
		       (if (char=? c (string-ref str 0))
			   (match-other-chars 1)
			   (match-1st-char)))))))
       ;; There has been a partial match, up to the point pos-to-match
       ;; (for example, str[0] has been found in the stream)
       ;; Now look to see if str[pos-to-match] for would be found, too
       (match-other-chars
	(lambda (pos-to-match)
	  (if (>= pos-to-match (string-length str))
	      no-chars-read		; the entire string has matched
	      (let ((c (my-peek-char)))
		(and c
		     (if (not (char=? c (string-ref str pos-to-match)))
			 (backtrack 1 pos-to-match)
			 (begin (next-char)
				(match-other-chars (inc pos-to-match)))))))))

       ;; There had been a partial match, but then a wrong char showed up.
       ;; Before discarding previously read (and matched) characters, we check
       ;; to see if there was some smaller partial match. Note, characters read
       ;; so far (which matter) are those of str[0..matched-substr-len - 1]
       ;; In other words, we will check to see if there is such i>0 that
       ;; substr(str,0,j) = substr(str,i,matched-substr-len)
       ;; where j=matched-substr-len - i
       (backtrack
	(lambda (i matched-substr-len)
	  (let ((j (- matched-substr-len i)))
	    (if (<= j 0)
	      (match-1st-char)	; backed off completely to the begining of str
	      (let loop ((k 0))
	        (if (>= k j)
	           (match-other-chars j) ; there was indeed a shorter match
	           (if (char=? (string-ref str k)
	           	       (string-ref str (+ i k)))
	             (loop (inc k))
	             (backtrack (inc i) matched-substr-len))))))))
       )
    (match-1st-char)))

(define find-string-from-port? MISCIO:find-string-from-port?)


;-----------------------------------------------------------------------------
;   This is a test driver for miscio:find-string-from-port?, to make sure it
;			really works as intended

; moved to vinput-parse.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;****************************************************************************
;			Simple Parsing of input
;
; The following simple functions surprisingly often suffice to parse
; an input stream. They either skip, or build and return tokens,
; according to inclusion or delimiting semantics. The list of
; characters to expect, include, or to break at may vary from one
; invocation of a function to another. This allows the functions to
; easily parse even context-sensitive languages.
;
; EOF is generally frowned on, and thrown up upon if encountered.
; Exceptions are mentioned specifically. The list of expected characters 
; (characters to skip until, or break-characters) may include an EOF
; "character", which is to be coded as symbol *eof*
;
; The input stream to parse is specified as a PORT, which is usually
; the last (and optional) argument. It defaults to the current input
; port if omitted.
;
; IMPORT
; This package relies on a function parser-error, which must be defined
; by a user of the package. The function has the following signature:
;	parser-error PORT MESSAGE SPECIALISING-MSG*
; Many procedures of this package call parser-error to report a parsing
; error.  The first argument is a port, which typically points to the
; offending character or its neighborhood. Most of the Scheme systems
; let the user query a PORT for the current position. MESSAGE is the
; description of the error. Other arguments supply more details about
; the problem.
; myenv.scm, myenv-bigloo.scm or a similar prelude is assumed.
; From SRFI-13, string-concatenate-reverse
; If a particular implementation lacks SRFI-13 support, please
; include the file srfi-13-local.scm
;
; $Id: input-parse.scm,v 1.7 2004/07/07 16:02:31 sperber Exp $

;------------------------------------------------------------------------

; -- procedure+: peek-next-char [PORT]
; 	advances to the next character in the PORT and peeks at it.
; 	This function is useful when parsing LR(1)-type languages
; 	(one-char-read-ahead).
;	The optional argument PORT defaults to the current input port.

(define-opt (peek-next-char (optional (port (current-input-port))))
  (read-char port) 
  (peek-char port)) 


;------------------------------------------------------------------------

; -- procedure+: assert-curr-char CHAR-LIST STRING [PORT]
;	Reads a character from the PORT and looks it up
;	in the CHAR-LIST of expected characters
;	If the read character was found among expected, it is returned
;	Otherwise, the procedure writes a nasty message using STRING
;	as a comment, and quits.
;	The optional argument PORT defaults to the current input port.
;
(define-opt (assert-curr-char expected-chars comment
			      (optional (port (current-input-port))))
  (let ((c (read-char port)))
    (if (memv c expected-chars) c
    (parser-error port "Wrong character " c
    	   " (0x" (if (eof-object? c) "*eof*"
    	   	    (number->string (char->integer c) 16)) ") "
    	   comment ". " expected-chars " expected"))))
    	   

; -- procedure+: skip-until CHAR-LIST [PORT]
;	Reads and skips characters from the PORT until one of the break
;	characters is encountered. This break character is returned.
;	The break characters are specified as the CHAR-LIST. This list
;	may include EOF, which is to be coded as a symbol *eof*
;
; -- procedure+: skip-until NUMBER [PORT]
;	Skips the specified NUMBER of characters from the PORT and returns #f
;
;	The optional argument PORT defaults to the current input port.


(define-opt (skip-until arg (optional (port (current-input-port))) )
  (cond
   ((number? arg)		; skip 'arg' characters
      (do ((i arg (dec i)))
      	  ((not (positive? i)) #f)
      	  (if (eof-object? (read-char port))
      	    (parser-error port "Unexpected EOF while skipping "
			 arg " characters"))))
   (else			; skip until break-chars (=arg)
     (let loop ((c (read-char port)))
       (cond
         ((memv c arg) c)
         ((eof-object? c)
           (if (memq '*eof* arg) c
             (parser-error port "Unexpected EOF while skipping until " arg)))
         (else (loop (read-char port))))))))


; -- procedure+: skip-while CHAR-LIST [PORT]
;	Reads characters from the PORT and disregards them,
;	as long as they are mentioned in the CHAR-LIST.
;	The first character (which may be EOF) peeked from the stream
;	that is NOT a member of the CHAR-LIST is returned. This character
;	is left on the stream.
;	The optional argument PORT defaults to the current input port.

(define-opt (skip-while skip-chars (optional (port (current-input-port))) )
  (do ((c (peek-char port) (peek-char port)))
      ((not (memv c skip-chars)) c)
      (read-char port)))
 
; whitespace const

;------------------------------------------------------------------------
;				Stream tokenizers


; -- procedure+: 
;    next-token PREFIX-CHAR-LIST BREAK-CHAR-LIST [COMMENT-STRING] [PORT]
;	skips any number of the prefix characters (members of the
;	PREFIX-CHAR-LIST), if any, and reads the sequence of characters
;	up to (but not including) a break character, one of the
;	BREAK-CHAR-LIST.
;	The string of characters thus read is returned.
;	The break character is left on the input stream
;	The list of break characters may include EOF, which is to be coded as
;	a symbol *eof*. Otherwise, EOF is fatal, generating an error message
;	including a specified COMMENT-STRING (if any)
;
;	The optional argument PORT defaults to the current input port.
;
; Note: since we can't tell offhand how large the token being read is
; going to be, we make a guess, pre-allocate a string, and grow it by
; quanta if necessary. The quantum is always the length of the string
; before it was extended the last time. Thus the algorithm does
; a Fibonacci-type extension, which has been proven optimal.
; Note, explicit port specification in read-char, peek-char helps.

; Procedure: input-parse:init-buffer
; returns an initial buffer for next-token* procedures.
; The input-parse:init-buffer may allocate a new buffer per each invocation:
;	(define (input-parse:init-buffer) (make-string 32))
; Size 32 turns out to be fairly good, on average.
; That policy is good only when a Scheme system is multi-threaded with
; preemptive scheduling, or when a Scheme system supports shared substrings.
; In all the other cases, it's better for input-parse:init-buffer to
; return the same static buffer. next-token* functions return a copy
; (a substring) of accumulated data, so the same buffer can be reused.
; We shouldn't worry about an incoming token being too large:
; next-token will use another chunk automatically. Still, 
; the best size for the static buffer is to allow most of the tokens to fit in.
; Using a static buffer _dramatically_ reduces the amount of produced garbage
; (e.g., during XML parsing).

(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))
  

		; See a better version below
(define-opt (next-token-old prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer))
	 (quantum curr-buf-len))
    (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
      (cond
        ((memv c break-chars) (substring buffer 0 i))
    	((eof-object? c)
    	  (if (memq '*eof* break-chars)
    	    (substring buffer 0 i)		; was EOF expected?
    	    (parser-error port "EOF while reading a token " comment)))
    	(else
    	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
    	    (begin			; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
    	  (string-set! buffer i c)
    	  (read-char port)			; move to the next char
    	  (loop (inc i) (peek-char port))
    	  )))))


; A better version of next-token, which accumulates the characters
; in chunks, and later on reverse-concatenates them, using
; SRFI-13 if available.
; The overhead of copying characters is only 100% (or even smaller: bulk
; string copying might be well-optimised), compared to the (hypothetical)
; circumstance if we had known the size of the token beforehand.
; For small tokens, the code performs just as above. For large
; tokens, we expect an improvement. Note, the code also has no
; assignments. 
; See next-token-comp.scm

(define-opt (next-token prefix-skipped-chars break-chars
		  (optional (comment "") (port (current-input-port))) )
  (let outer ((buffer (input-parse:init-buffer)) (filled-buffer-l '())
	      (c (skip-while prefix-skipped-chars port)))
    (let ((curr-buf-len (string-length buffer)))
      (let loop ((i 0) (c c))
	(cond
	  ((memv c break-chars)
	    (if (null? filled-buffer-l) (substring buffer 0 i)
	      (string-concatenate-reverse filled-buffer-l buffer i)))
	  ((eof-object? c)
	    (if (memq '*eof* break-chars)	; was EOF expected?
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i))
	      (parser-error port "EOF while reading a token " comment)))
	  ((>= i curr-buf-len)
	    (outer (make-string curr-buf-len)
	      (cons buffer filled-buffer-l) c))
	  (else
	    (string-set! buffer i c)
	    (read-char port)			; move to the next char
	    (loop (inc i) (peek-char port))))))))

; -- procedure+: next-token-of INC-CHARSET [PORT]
;	Reads characters from the PORT that belong to the list of characters
;	INC-CHARSET. The reading stops at the first character which is not
;	a member of the set. This character is left on the stream.
;	All the read characters are returned in a string.
;
; -- procedure+: next-token-of PRED [PORT]
;	Reads characters from the PORT for which PRED (a procedure of one
;	argument) returns non-#f. The reading stops at the first character
;	for which PRED returns #f. That character is left on the stream.
;	All the results of evaluating of PRED up to #f are returned in a
;	string.
;
;	PRED is a procedure that takes one argument (a character
;	or the EOF object) and returns a character or #f. The returned
;	character does not have to be the same as the input argument
;	to the PRED. For example,
;	(next-token-of (lambda (c)
;			  (cond ((eof-object? c) #f)
;				((char-alphabetic? c) (char-downcase c))
;				(else #f))))
;	will try to read an alphabetic token from the current
;	input port, and return it in lower case.
; 
;	The optional argument PORT defaults to the current input port.
;
; This procedure is similar to next-token but only it implements
; an inclusion rather than delimiting semantics.

(define-opt (next-token-of incl-list/pred
			   (optional (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)))
  (if (procedure? incl-list/pred)
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (incl-list/pred (peek-char port))))
	    (if c
	      (begin
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i)))
	      ; incl-list/pred decided it had had enough
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i)))))))

    ; incl-list/pred is a list of allowed characters
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (peek-char port)))
	    (cond
	      ((not (memv c incl-list/pred))
		(if (null? filled-buffer-l) (substring buffer 0 i)
		  (string-concatenate-reverse filled-buffer-l buffer i)))
	      (else
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i))))))))
    )))


; -- procedure+: read-text-line [PORT]
;	Reads one line of text from the PORT, and returns it as a string.
;	A line is a (possibly empty) sequence of characters terminated
;	by CR, CRLF or LF (or even the end of file).
;	The terminating character (or CRLF combination) is removed from
;	the input stream. The terminating character(s) is not a part
;	of the return string either.
;	If EOF is encountered before any character is read, the return
;	value is EOF.
; 
;	The optional argument PORT defaults to the current input port.

(define *read-line-breaks* (list char-newline char-return '*eof*))

(define-opt (read-text-line (optional (port (current-input-port))) )
  (if (eof-object? (peek-char port)) (peek-char port)
    (let* ((line
             (next-token '() *read-line-breaks*
			 "reading a line" port))
           (c (read-char port)))	; must be either \n or \r or EOF
       (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
         (read-char port))			; skip \n that follows \r
       line)))


; -- procedure+: read-string N [PORT]
;	Reads N characters from the PORT, and  returns them in a string.
;	If EOF is encountered before N characters are read, a shorter string
;	will be returned.
;	If N is not positive, an empty string will be returned.
;	The optional argument PORT defaults to the current input port.

(define-opt (read-string n (optional (port (current-input-port))) )
  (if (not (positive? n)) ""
    (let ((buffer (make-string n)))
      (let loop ((i 0) (c (read-char port)))
        (if (eof-object? c) (substring buffer 0 i)
          (let ((i1 (inc i)))
            (string-set! buffer i c)
            (if (= i1 n) buffer
              (loop i1 (read-char port)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;	Functional XML parsing framework: SAX/DOM and SXML parsers
;	      with support for XML Namespaces and validation
;
; This is a package of low-to-high level lexing and parsing procedures
; that can be combined to yield a SAX, a DOM, a validating parsers, or
; a parser intended for a particular document type. The procedures in
; the package can be used separately to tokenize or parse various
; pieces of XML documents. The package supports XML Namespaces,
; internal and external parsed entities, user-controlled handling of
; whitespace, and validation. This module therefore is intended to be
; a framework, a set of "Lego blocks" you can use to build a parser
; following any discipline and performing validation to any degree. As
; an example of the parser construction, this file includes a
; semi-validating SXML parser.

; The present XML framework has a "sequential" feel of SAX yet a
; "functional style" of DOM. Like a SAX parser, the framework scans
; the document only once and permits incremental processing. An
; application that handles document elements in order can run as
; efficiently as possible. _Unlike_ a SAX parser, the framework does
; not require an application register stateful callbacks and surrender
; control to the parser. Rather, it is the application that can drive
; the framework -- calling its functions to get the current lexical or
; syntax element. These functions do not maintain or mutate any state
; save the input port. Therefore, the framework permits parsing of XML
; in a pure functional style, with the input port being a monad (or a
; linear, read-once parameter).

; Besides the PORT, there is another monad -- SEED. Most of the
; middle- and high-level parsers are single-threaded through the
; seed. The functions of this framework do not process or affect the
; SEED in any way: they simply pass it around as an instance of an
; opaque datatype.  User functions, on the other hand, can use the
; seed to maintain user's state, to accumulate parsing results, etc. A
; user can freely mix his own functions with those of the
; framework. On the other hand, the user may wish to instantiate a
; high-level parser: ssax:make-elem-parser or ssax:make-parser.  In
; the latter case, the user must provide functions of specific
; signatures, which are called at predictable moments during the
; parsing: to handle character data, element data, or processing
; instructions (PI). The functions are always given the SEED, among
; other parameters, and must return the new SEED.

; From a functional point of view, XML parsing is a combined
; pre-post-order traversal of a "tree" that is the XML document
; itself. This down-and-up traversal tells the user about an element
; when its start tag is encountered. The user is notified about the
; element once more, after all element's children have been
; handled. The process of XML parsing therefore is a fold over the
; raw XML document. Unlike a fold over trees defined in [1], the
; parser is necessarily single-threaded -- obviously as elements
; in a text XML document are laid down sequentially. The parser
; therefore is a tree fold that has been transformed to accept an
; accumulating parameter [1,2].

; Formally, the denotational semantics of the parser can be expressed
; as
; parser:: (Start-tag -> Seed -> Seed) ->
;	   (Start-tag -> Seed -> Seed -> Seed) ->
;	   (Char-Data -> Seed -> Seed) ->
;	   XML-text-fragment -> Seed -> Seed
; parser fdown fup fchar "<elem attrs> content </elem>" seed
;  = fup "<elem attrs>" seed
;	(parser fdown fup fchar "content" (fdown "<elem attrs>" seed))
;
; parser fdown fup fchar "char-data content" seed
;  = parser fdown fup fchar "content" (fchar "char-data" seed)
;
; parser fdown fup fchar "elem-content content" seed
;  = parser fdown fup fchar "content" (
;	parser fdown fup fchar "elem-content" seed)

; Compare the last two equations with the left fold
; fold-left kons elem:list seed = fold-left kons list (kons elem seed)

; The real parser created my ssax:make-parser is slightly more complicated,
; to account for processing instructions, entity references, namespaces,
; processing of document type declaration, etc.


; The XML standard document referred to in this module is
;	http://www.w3.org/TR/1998/REC-xml-19980210.html
;
; The present file also defines a procedure that parses the text of an
; XML document or of a separate element into SXML, an
; S-expression-based model of an XML Information Set. SXML is also an
; Abstract Syntax Tree of an XML document. SXML is similar
; but not identical to DOM; SXML is particularly suitable for
; Scheme-based XML/HTML authoring, SXPath queries, and tree
; transformations. See SXML.html for more details.
; SXML is a term implementation of evaluation of the XML document [3].
; The other implementation is context-passing.

; The present frameworks fully supports the XML Namespaces Recommendation:
;	http://www.w3.org/TR/REC-xml-names/
; Other links:
; [1] Jeremy Gibbons, Geraint Jones, "The Under-appreciated Unfold,"
; Proc. ICFP'98, 1998, pp. 273-279.
; [2] Richard S. Bird, The promotion and accumulation strategies in
; transformational programming, ACM Trans. Progr. Lang. Systems,
; 6(4):487-504, October 1984.
; [3] Ralf Hinze, "Deriving Backtracking Monad Transformers,"
; Functional Pearl. Proc ICFP'00, pp. 186-197.

; IMPORT
; parser-error ssax:warn, see Handling of errors, below
; functions declared in files util.scm, input-parse.scm and look-for-str.scm
; char-encoding.scm for various platform-specific character-encoding functions.
; From SRFI-13: string-concatenate/shared and string-concatenate-reverse/shared
; If a particular implementation lacks SRFI-13 support, please
; include the file srfi-13-local.scm

; Handling of errors
; This package relies on a function parser-error, which must be defined
; by a user of the package. The function has the following signature:
;	parser-error PORT MESSAGE SPECIALISING-MSG*
; Many procedures of this package call 'parser-error' whenever a
; parsing, well-formedness or validation error is encountered. The
; first argument is a port, which typically points to the offending
; character or its neighborhood. Most of the Scheme systems let the
; user query a PORT for the current position. The MESSAGE argument
; indicates a failed XML production or a failed XML constraint. The
; latter is referred to by its anchor name in the XML Recommendation
; or XML Namespaces Recommendation. The parsing library (e.g.,
; next-token, assert-curr-char) invoke 'parser-error' as well, in
; exactly the same way.  See input-parse.scm for more details.
; See
;	http://pair.com/lisovsky/download/parse-error.scm
; for an excellent example of such a redefined parser-error function.
;
; In addition, the present code invokes a function ssax:warn
;   ssax:warn PORT MESSAGE SPECIALISING-MSG*
; to notify the user about warnings that are NOT errors but still
; may alert the user.
;
; Again, parser-error and ssax:warn are supposed to be defined by the
; user. However, if a run-test macro below is set to include
; self-tests, this present code does provide the definitions for these
; functions to allow tests to run.

; Misc notes
; It seems it is highly desirable to separate tests out in a dedicated
; file.
;
; Jim Bender wrote on Mon, 9 Sep 2002 20:03:42 EDT on the SSAX-SXML
; mailing list (message A fine-grained "lego")
; The task was to record precise source location information, as PLT
; does with its current XML parser. That parser records the start and
; end location (filepos, line#, column#) for pi, elements, attributes,
; chuncks of "pcdata".
; As suggested above, though, in some cases I needed to be able force
; open an interface that did not yet exist. For instance, I added an
; "end-char-data-hook", which would be called at the end of char-data
; fragment. This returns a function of type (seed -> seed) which is
; invoked on the current seed only if read-char-data has indeed reached
; the end of a block of char data (after reading a new token.
; But the deepest interface that I needed to expose was that of reading
; attributes. In the official distribution, this is not even a separate
; function. Instead, it is embedded within SSAX:read-attributes.  This
; required some small re-structuring as well.
; This definitely will not be to everyone's taste (nor needed by most).
; Certainly, the existing make-parser interface addresses most custom
; needs. And likely 80-90 lines of a "link specification" to create a
; parser from many tiny little lego blocks may please only a few, while
; appalling others.
; The code is available at http://celtic.benderweb.net/ssax-lego.plt or 
; http://celtic.benderweb.net/ssax-lego.tar.gz
; In the examples directory, I provide:
; - a unit version of the make-parser interface,
; - a simple SXML parser using that interface, 
; - an SXML parser which directly uses the "new lego",
; - a pseudo-SXML parser, which records source location information
; - and lastly a parser which returns the structures used in PLT's xml 
; collection, with source location information

; $Id: SSAX.scm,v 5.1 2004/07/07 16:02:30 sperber Exp $
;^^^^^^^^^


	; See the Makefile in the ../tests directory
	; (in particular, the rule vSSAX) for an example of how
	; to run this code on various Scheme systems.
	; See SSAX examples for many samples of using this code,
	; again, on a variety of Scheme systems.
	; See http://ssax.sf.net/


; The following macro runs built-in test cases -- or does not run,
; depending on which of the two cases below you commented out
; Case 1: no tests:
;(define-macro run-test (lambda body '(begin #f)))
;(define-syntax run-test (syntax-rules () ((run-test . args) (begin #f))))

; Case 2: with tests.
; The following macro could've been defined just as
; (define-macro run-test (lambda body `(begin (display "\n-->Test\n") ,@body)))
;
; Instead, it's more involved, to make up for case-insensitivity of
; symbols on some Scheme systems. In Gambit, symbols are case
; sensitive: (eq? 'A 'a) is #f and (eq? 'Aa (string->symbol "Aa")) is
; #t.  On some systems, symbols are case-insensitive and just the
; opposite is true.  Therefore, we introduce a notation '"ASymbol" (a
; quoted string) that stands for a case-_sensitive_ ASymbol -- on any
; R5RS Scheme system. This notation is valid only within the body of
; run-test.
; The notation is implemented by scanning the run-test's
; body and replacing every occurrence of (quote "str") with the result
; of (string->symbol "str"). We can do such a replacement at macro-expand
; time (rather than at run time).

; Here's the previous version of run-test, implemented as a low-level
; macro. 
; (define-macro run-test
;   (lambda body
;     (define (re-write body)
;       (cond
;        ((vector? body)
; 	(list->vector (re-write (vector->list body))))
;        ((not (pair? body)) body)
;        ((and (eq? 'quote (car body)) (pair? (cdr body))
; 	     (string? (cadr body)))
; 	(string->symbol (cadr body)))
;        (else (cons (re-write (car body)) (re-write (cdr body))))))
;     (cons 'begin (re-write body))))
;
; For portability, it is re-written as syntax-rules. The syntax-rules
; version is less powerful: for example, it can't handle
; (case x (('"Foo") (do-on-Foo))) whereas the low-level macro
; could correctly place a case-sensitive symbol at the right place.
; We also do not scan vectors (because we don't use them here).
; Twice-deep quasiquotes aren't handled either.
; Still, the syntax-rules version satisfies our immediate needs.
; Incidentally, I originally didn't believe that the macro below
; was at all possible.
; 
; The macro is written in a continuation-passing style. A continuation
; typically has the following structure: (k-head ! . args)
; When the continuation is invoked, we expand into
; (k-head <computed-result> . arg). That is, the dedicated symbol !
; is the placeholder for the result.
;
; It seems that the most modular way to write the run-test macro would
; be the following
;
; (define-syntax run-test
;  (syntax-rules ()
;   ((run-test . ?body)
;     (letrec-syntax
;       ((scan-exp			; (scan-exp body k)
; 	 (syntax-rules (quote quasiquote !)
; 	   ((scan-exp (quote (hd . tl)) k)
; 	     (scan-lit-lst (hd . tl) (do-wrap ! quasiquote k)))
; 	   ((scan-exp (quote x) (k-head ! . args))
; 	     (k-head 
; 	       (if (string? (quote x)) (string->symbol (quote x)) (quote x))
; 	       . args))
; 	   ((scan-exp (hd . tl) k)
; 	     (scan-exp hd (do-tl ! scan-exp tl k)))
; 	   ((scan-exp x (k-head ! . args))
; 	     (k-head x . args))))
; 	(do-tl
; 	  (syntax-rules (!)
; 	    ((do-tl processed-hd fn () (k-head ! . args))
; 	      (k-head (processed-hd) . args))
; 	    ((do-tl processed-hd fn old-tl k)
; 	      (fn old-tl (do-cons ! processed-hd k)))))
; 	...
; 	(do-finish
; 	  (syntax-rules ()
; 	    ((do-finish (new-body)) new-body)
; 	    ((do-finish new-body) (begin . new-body))))
; 	...
;       (scan-exp ?body (do-finish !))
; ))))
;
; Alas, that doesn't work on all systems. We hit yet another dark
; corner of the R5RS macros. The reason is that run-test is used in
; the code below to introduce definitions. For example:
; (run-test
;  (define (ssax:warn port msg . other-msg)
;    (apply cerr (cons* nl "Warning: " msg other-msg)))
; )
; This code expands to
; (begin
;    (define (ssax:warn port msg . other-msg) ...))
; so the definition gets spliced in into the top level. Right?
; Well, On Petite Chez Scheme it is so. However, many other systems
; don't like this approach. The reason is that the invocation of
; (run-test (define (ssax:warn port msg . other-msg) ...))
; first expands into
; (letrec-syntax (...) 
;   (scan-exp ((define (ssax:warn port msg . other-msg) ...)) ...))
; because of the presence of (letrec-syntax ...), the begin form that
; is generated eventually is no longer at the top level! The begin
; form in Scheme is an overloading of two distinct forms: top-level
; begin and the other begin. The forms have different rules: for example,
; (begin (define x 1)) is OK for a top-level begin but not OK for
; the other begin. Some Scheme systems see the that the macro
; (run-test ...) expands into (letrec-syntax ...) and decide right there
; that any further (begin ...) forms are NOT top-level begin forms.
; The only way out is to make sure all our macros are top-level.
; The best approach <sigh> seems to be to make run-test one huge
; top-level macro.


(define-syntax run-test
 (syntax-rules (define)
   ((run-test "scan-exp" (define vars body))
    (define vars (run-test "scan-exp" body)))
   ((run-test "scan-exp" ?body)
    (letrec-syntax
      ((scan-exp			; (scan-exp body k)
	 (syntax-rules (quote quasiquote !)
	   ((scan-exp '() (k-head ! . args))
	     (k-head '() . args))
	   ((scan-exp (quote (hd . tl)) k)
	     (scan-lit-lst (hd . tl) (do-wrap ! quasiquote k)))
	   ((scan-exp (quasiquote (hd . tl)) k)
	     (scan-lit-lst (hd . tl) (do-wrap ! quasiquote k)))
	   ((scan-exp (quote x) (k-head ! . args))
	     (k-head 
	       (if (string? (quote x)) (string->symbol (quote x)) (quote x))
	       . args))
	   ((scan-exp (hd . tl) k)
	     (scan-exp hd (do-tl ! scan-exp tl k)))
	   ((scan-exp x (k-head ! . args))
	     (k-head x . args))))
	(do-tl
	  (syntax-rules (!)
	    ((do-tl processed-hd fn () (k-head ! . args))
	      (k-head (processed-hd) . args))
	    ((do-tl processed-hd fn old-tl k)
	      (fn old-tl (do-cons ! processed-hd k)))))
	(do-cons
	  (syntax-rules (!)
	    ((do-cons processed-tl processed-hd (k-head ! . args))
	      (k-head (processed-hd . processed-tl) . args))))
	(do-wrap
	  (syntax-rules (!)
	    ((do-wrap val fn (k-head ! . args))
	      (k-head (fn val) . args))))
	(do-finish
	  (syntax-rules ()
	    ((do-finish new-body) new-body)))

	(scan-lit-lst			; scan literal list
	  (syntax-rules (quote unquote unquote-splicing !)
	   ((scan-lit-lst '() (k-head ! . args))
	     (k-head '() . args))
	   ((scan-lit-lst (quote (hd . tl)) k)
	     (do-tl quote scan-lit-lst ((hd . tl)) k))
	   ((scan-lit-lst (unquote x) k)
	     (scan-exp x (do-wrap ! unquote k)))
	   ((scan-lit-lst (unquote-splicing x) k)
	     (scan-exp x (do-wrap ! unquote-splicing k)))
	   ((scan-lit-lst (quote x) (k-head ! . args))
	     (k-head 
	       ,(if (string? (quote x)) (string->symbol (quote x)) (quote x))
	       . args))
	    ((scan-lit-lst (hd . tl) k)
	      (scan-lit-lst hd (do-tl ! scan-lit-lst tl k)))
	    ((scan-lit-lst x (k-head ! . args))
	      (k-head x . args))))
	)
      (scan-exp ?body (do-finish !))))
  ((run-test body ...)
   (begin
     (run-test "scan-exp" body) ...))
))

;========================================================================
;				Data Types

; TAG-KIND
;	a symbol 'START, 'END, 'PI, 'DECL, 'COMMENT, 'CDSECT
;		or 'ENTITY-REF that identifies a markup token

; UNRES-NAME
;	a name (called GI in the XML Recommendation) as given in an xml
;	document for a markup token: start-tag, PI target, attribute name.
;	If a GI is an NCName, UNRES-NAME is this NCName converted into
;	a Scheme symbol. If a GI is a QName, UNRES-NAME is a pair of
;	symbols: (PREFIX . LOCALPART)

; RES-NAME
;	An expanded name, a resolved version of an UNRES-NAME.
;	For an element or an attribute name with a non-empty namespace URI,
;	RES-NAME is a pair of symbols, (URI-SYMB . LOCALPART).
;	Otherwise, it's a single symbol.

; ELEM-CONTENT-MODEL
;	A symbol:
;	ANY	  - anything goes, expect an END tag.
;	EMPTY-TAG - no content, and no END-tag is coming
;	EMPTY	  - no content, expect the END-tag as the next token
;	PCDATA    - expect character data only, and no children elements
;	MIXED
;	ELEM-CONTENT

; URI-SYMB
;	A symbol representing a namespace URI -- or other symbol chosen
;	by the user to represent URI. In the former case,
;	URI-SYMB is created by %-quoting of bad URI characters and
;	converting the resulting string into a symbol.

; NAMESPACES
;	A list representing namespaces in effect. An element of the list
;	has one of the following forms:
;	(PREFIX URI-SYMB . URI-SYMB) or
;	(PREFIX USER-PREFIX . URI-SYMB)
;		USER-PREFIX is a symbol chosen by the user
;		to represent the URI.
;	(#f USER-PREFIX . URI-SYMB)
;		Specification of the user-chosen prefix and a URI-SYMBOL.
;	(*DEFAULT* USER-PREFIX . URI-SYMB)
;		Declaration of the default namespace
;	(*DEFAULT* #f . #f)
;		Un-declaration of the default namespace. This notation
;		represents overriding of the previous declaration
;	A NAMESPACES list may contain several elements for the same PREFIX.
;	The one closest to the beginning of the list takes effect.

; ATTLIST
;	An ordered collection of (NAME . VALUE) pairs, where NAME is
;	a RES-NAME or an UNRES-NAME. The collection is an ADT

; STR-HANDLER
;	A procedure of three arguments: STRING1 STRING2 SEED
;	returning a new SEED
;	The procedure is supposed to handle a chunk of character data
;	STRING1 followed by a chunk of character data STRING2.
;	STRING2 is a short string, often "\n" and even ""

; ENTITIES
;	An assoc list of pairs:
;	   (named-entity-name . named-entity-body)
;	where named-entity-name is a symbol under which the entity was
;	declared, named-entity-body is either a string, or
;	(for an external entity) a thunk that will return an
;	input port (from which the entity can be read).
;	named-entity-body may also be #f. This is an indication that a
;	named-entity-name is currently being expanded. A reference to
;	this named-entity-name will be an error: violation of the
;	WFC nonrecursion.

; XML-TOKEN -- a record

; In Gambit, you can use the following declaration:
; (define-structure xml-token kind head)
; The following declaration is "standard" as it follows SRFI-9:
;;(define-record-type  xml-token  (make-xml-token kind head)  xml-token?
;;  (kind  xml-token-kind)
;;  (head  xml-token-head) )
; No field mutators are declared as SSAX is a pure functional parser
;
; But to make the code more portable, we define xml-token simply as
; a pair. It suffices for us. Furthermore, xml-token-kind and xml-token-head
; can be defined as simple procedures. However, they are declared as
; macros below for efficiency.

(define (make-xml-token kind head) (cons kind head))
(define xml-token? pair?)
(define-syntax xml-token-kind 
  (syntax-rules () ((xml-token-kind token) (car token))))
(define-syntax xml-token-head 
  (syntax-rules () ((xml-token-head token) (cdr token))))

; (define-macro xml-token-kind (lambda (token) `(car ,token)))
; (define-macro xml-token-head (lambda (token) `(cdr ,token)))

; This record represents a markup, which is, according to the XML
; Recommendation, "takes the form of start-tags, end-tags, empty-element tags,
; entity references, character references, comments, CDATA section delimiters,
; document type declarations, and processing instructions."
;
;	kind -- a TAG-KIND
;	head -- an UNRES-NAME. For xml-tokens of kinds 'COMMENT and
;		'CDSECT, the head is #f
;
; For example,
;	<P>  => kind='START, head='P
;	</P> => kind='END, head='P
;	<BR/> => kind='EMPTY-EL, head='BR
;	<!DOCTYPE OMF ...> => kind='DECL, head='DOCTYPE
;	<?xml version="1.0"?> => kind='PI, head='xml
;	&my-ent; => kind = 'ENTITY-REF, head='my-ent
; 
; Character references are not represented by xml-tokens as these references
; are transparently resolved into the corresponding characters.
;



; XML-DECL -- a record

; The following is Gambit-specific, see below for a portable declaration
;(define-structure xml-decl elems entities notations)

; The record represents a datatype of an XML document: the list of
; declared elements and their attributes, declared notations, list of
; replacement strings or loading procedures for parsed general
; entities, etc. Normally an xml-decl record is created from a DTD or
; an XML Schema, although it can be created and filled in in many other
; ways (e.g., loaded from a file).
;
; elems: an (assoc) list of decl-elem or #f. The latter instructs
;	the parser to do no validation of elements and attributes.
;
; decl-elem: declaration of one element:
;	(elem-name elem-content decl-attrs)
;	elem-name is an UNRES-NAME for the element.
;	elem-content is an ELEM-CONTENT-MODEL.
;	decl-attrs is an ATTLIST, of (ATTR-NAME . VALUE) associations
; !!!This element can declare a user procedure to handle parsing of an
; element (e.g., to do a custom validation, or to build a hash of
; IDs as they're encountered).
;
; decl-attr: an element of an ATTLIST, declaration of one attribute
;	(attr-name content-type use-type default-value)
;	attr-name is an UNRES-NAME for the declared attribute
;	content-type is a symbol: CDATA, NMTOKEN, NMTOKENS, ...
;		or a list of strings for the enumerated type.
;	use-type is a symbol: REQUIRED, IMPLIED, FIXED
;	default-value is a string for the default value, or #f if not given.
;
;

; see a function make-empty-xml-decl to make a XML declaration entry
; suitable for a non-validating parsing.


;-------------------------
; Utilities

;   ssax:warn PORT MESSAGE SPECIALISING-MSG*
; to notify the user about warnings that are NOT errors but still
; may alert the user.
; Result is unspecified.
; We need to define the function to allow the self-tests to run.
; Normally the definition of ssax:warn is to be provided by the user.


;   parser-error PORT MESSAGE SPECIALISING-MSG*
; to let the user know of a syntax error or a violation of a
; well-formedness or validation constraint.
; Result is unspecified.
; We need to define the function to allow the self-tests to run.
; Normally the definition of parser-error is to be provided by the user.
;; (run-test
;;  (define (parser-error port msg . specializing-msgs)
;;    (apply error (cons msg specializing-msgs)))
;; )

; The following is a function that is often used in validation tests,
; to make sure that the computed result matches the expected one.
; This function is a standard equal? predicate with one exception.
; On Scheme systems where (string->symbol "A") and a symbol A
; are the same, equal_? is precisely equal?
; On other Scheme systems, we compare symbols disregarding their case.
; Since this function is used only in tests, we don't have to
; strive to make it efficient.
;; (run-test
;;  (define (equal_? e1 e2)
;;    (if (eq? 'A (string->symbol "A")) (equal? e1 e2)
;;        (cond
;; 	((symbol? e1)
;; 	 (and (symbol? e2) 
;; 	      (string-ci=? (symbol->string e1) (symbol->string e2))))
;; 	((pair? e1)
;; 	 (and (pair? e2)
;; 	      (equal_? (car e1) (car e2)) (equal_? (cdr e1) (cdr e2))))
;; 	((vector? e1)
;; 	 (and (vector? e2) (equal_? (vector->list e1) (vector->list e2))))
;; 	(else
;; 	 (equal? e1 e2)))))
;; )

; The following function, which is often used in validation tests,
; lets us conveniently enter newline, CR and tab characters in a character
; string.
;	unesc-string: ESC-STRING -> STRING
; where ESC-STRING is a character string that may contain
;    %n  -- for #\newline
;    %r  -- for #\return
;    %t  -- for #\tab
;    %%  -- for #\%
;
; The result of unesc-string is a character string with all %-combinations
; above replaced with their character equivalents

;; (run-test
;;  (define (unesc-string str)
;;    (call-with-input-string str
;;      (lambda (port)
;;        (let loop ((frags '()))
;; 	 (let* ((token (next-token '() '(#\% *eof*) "unesc-string" port))
;; 		(cterm (read-char port))
;; 		(frags (cons token frags)))
;; 	   (if (eof-object? cterm) (string-concatenate-reverse/shared frags)
;; 	     (let ((cchar (read-char port)))  ; char after #\%
;; 	       (if (eof-object? cchar)
;; 		 (error "unexpected EOF after reading % in unesc-string:" str)
;; 		 (loop
;; 		   (cons
;; 		     (case cchar
;; 		       ((#\n) (string #\newline))
;; 		       ((#\r) (string char-return))
;; 		       ((#\t) (string char-tab))
;; 		       ((#\%) "%")
;; 		       (else (error "bad %-char in unesc-string:" cchar)))
;; 		     frags))))))))))
;; )
	     

; Test if a string is made of only whitespace
; An empty string is considered made of whitespace as well
(define (string-whitespace? str)
  (let ((len (string-length str)))
    (cond
     ((zero? len) #t)
     ((= 1 len) (char-whitespace? (string-ref str 0)))
     ((= 2 len) (and (char-whitespace? (string-ref str 0))
		     (char-whitespace? (string-ref str 1))))
     (else
      (let loop ((i 0))
	(or (>= i len)
	    (and (char-whitespace? (string-ref str i))
		 (loop (inc i)))))))))

; Find val in alist
; Return (values found-el remaining-alist) or
;	 (values #f alist)

(define (assq-values val alist)
  (let loop ((alist alist) (scanned '()))
    (cond
     ((null? alist) (values #f scanned))
     ((equal? val (caar alist))
      (values (car alist) (append scanned (cdr alist))))
     (else
      (loop (cdr alist) (cons (car alist) scanned))))))


;========================================================================
;		Lower-level parsers and scanners
;
; They deal with primitive lexical units (Names, whitespaces, tags)
; and with pieces of more generic productions. Most of these parsers
; must be called in appropriate context. For example, ssax:complete-start-tag
; must be called only when the start-tag has been detected and its GI
; has been read.

;------------------------------------------------------------------------
;			Low-level parsing code

; Skip the S (whitespace) production as defined by
; [3] S ::= (#x20 | #x9 | #xD | #xA)
; The procedure returns the first not-whitespace character it
; encounters while scanning the PORT. This character is left
; on the input stream.

(define ssax:S-chars (map ascii->char '(32 10 9 13)))

(define (ssax:skip-S port)
  (skip-while ssax:S-chars port))


; Read a Name lexem and return it as string
; [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
;                  | CombiningChar | Extender
; [5] Name ::= (Letter | '_' | ':') (NameChar)*
;
; This code supports the XML Namespace Recommendation REC-xml-names,
; which modifies the above productions as follows:
;
; [4] NCNameChar ::= Letter | Digit | '.' | '-' | '_'
;                       | CombiningChar | Extender
; [5] NCName ::= (Letter | '_') (NCNameChar)*
; As the Rec-xml-names says,
; "An XML document conforms to this specification if all other tokens
; [other than element types and attribute names] in the document which
; are required, for XML conformance, to match the XML production for
; Name, match this specification's production for NCName."
; Element types and attribute names must match the production QName,
; defined below.

; Check to see if a-char may start a NCName
(define (ssax:ncname-starting-char? a-char)
  (and (char? a-char)
    (or
      (char-alphabetic? a-char)
      (char=? #\_ a-char))))


; Read a NCName starting from the current position in the PORT and
; return it as a symbol.
(define (ssax:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (ssax:ncname-starting-char? first-char)
      (parser-error port "XMLNS [4] for '" first-char "'")))
  (string->symbol
    (next-token-of
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          ((string-index "0123456789.-_" c) c)
          (else #f)))
      port)))

; Read a (namespace-) Qualified Name, QName, from the current
; position in the PORT.
; From REC-xml-names:
;	[6] QName ::= (Prefix ':')? LocalPart
;	[7] Prefix ::= NCName
;	[8] LocalPart ::= NCName
; Return: an UNRES-NAME
(define (ssax:read-QName port)
  (let ((prefix-or-localpart (ssax:read-NCName port)))
    (case (peek-char port)
      ((#\:)			; prefix was given after all
       (read-char port)		; consume the colon
       (cons prefix-or-localpart (ssax:read-NCName port)))
      (else prefix-or-localpart) ; Prefix was omitted
      )))

; The prefix of the pre-defined XML namespace
(define ssax:Prefix-XML (string->symbol "xml"))

; Compare one RES-NAME or an UNRES-NAME with the other.
; Return a symbol '<, '>, or '= depending on the result of
; the comparison.
; Names without PREFIX are always smaller than those with the PREFIX.
(define name-compare
  (letrec ((symbol-compare
	    (lambda (symb1 symb2)
	      (cond 
	       ((eq? symb1 symb2) '=)
	       ((string<? (symbol->string symb1) (symbol->string symb2))
		'<)
	       (else '>)))))
    (lambda (name1 name2)
      (cond
       ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2)
			    '<))
       ((symbol? name2) '>)
       ((eq? name2 ssax:largest-unres-name) '<)
       ((eq? name1 ssax:largest-unres-name) '>)
       ((eq? (car name1) (car name2))	; prefixes the same
	(symbol-compare (cdr name1) (cdr name2)))
       (else (symbol-compare (car name1) (car name2)))))))

; An UNRES-NAME that is postulated to be larger than anything that can occur in
; a well-formed XML document.
; name-compare enforces this postulate.
(define ssax:largest-unres-name (cons 
				  (string->symbol "#LARGEST-SYMBOL")
				  (string->symbol "#LARGEST-SYMBOL")))



; procedure:	ssax:read-markup-token PORT
; This procedure starts parsing of a markup token. The current position
; in the stream must be #\<. This procedure scans enough of the input stream
; to figure out what kind of a markup token it is seeing. The procedure returns
; an xml-token structure describing the token. Note, generally reading
; of the current markup is not finished! In particular, no attributes of
; the start-tag token are scanned.
;
; Here's a detailed break out of the return values and the position in the PORT
; when that particular value is returned:
;	PI-token:	only PI-target is read.
;			To finish the Processing Instruction and disregard it,
;			call ssax:skip-pi. ssax:read-attributes may be useful
;			as well (for PIs whose content is attribute-value
;			pairs)
;	END-token:	The end tag is read completely; the current position
;			is right after the terminating #\> character.	
;	COMMENT		is read and skipped completely. The current position
;			is right after "-->" that terminates the comment.
;	CDSECT		The current position is right after "<!CDATA["
;			Use ssax:read-cdata-body to read the rest.
;	DECL		We have read the keyword (the one that follows "<!")
;			identifying this declaration markup. The current
;			position is after the keyword (usually a
;			whitespace character)
;
;	START-token	We have read the keyword (GI) of this start tag.
;			No attributes are scanned yet. We don't know if this
;			tag has an empty content either.
;			Use ssax:complete-start-tag to finish parsing of
;			the token.

(define ssax:read-markup-token ; procedure ssax:read-markup-token port
 (let ()
  		; we have read "<!-". Skip through the rest of the comment
		; Return the 'COMMENT token as an indication we saw a comment
		; and skipped it.
  (define (skip-comment port)
    (assert-curr-char '(#\-) "XML [15], second dash" port)
    (if (not (find-string-from-port? "-->" port))
      (parser-error port "XML [15], no -->"))
    (make-xml-token 'COMMENT #f))

  		; we have read "<![" that must begin a CDATA section
  (define (read-cdata port)
    (assert (string=? "CDATA[" (read-string 6 port)))
    (make-xml-token 'CDSECT #f))

  (lambda (port)
    (assert-curr-char '(#\<) "start of the token" port)
    (case (peek-char port)
      ((#\/) (read-char port)
       (begin0 (make-xml-token 'END (ssax:read-QName port))
	       (ssax:skip-S port)
	       (assert-curr-char '(#\>) "XML [42]" port)))
      ((#\?) (read-char port) (make-xml-token 'PI (ssax:read-NCName port)))
      ((#\!)
       (case (peek-next-char port)
	 ((#\-) (read-char port) (skip-comment port))
	 ((#\[) (read-char port) (read-cdata port))
	 (else (make-xml-token 'DECL (ssax:read-NCName port)))))
      (else (make-xml-token 'START (ssax:read-QName port)))))
))


; The current position is inside a PI. Skip till the rest of the PI
(define (ssax:skip-pi port)      
  (if (not (find-string-from-port? "?>" port))
    (parser-error port "Failed to find ?> terminating the PI")))


; The current position is right after reading the PITarget. We read the
; body of PI and return is as a string. The port will point to the
; character right after '?>' combination that terminates PI.
; [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'

(define (ssax:read-pi-body-as-string port)
  (ssax:skip-S port)		; skip WS after the PI target name
  (string-concatenate/shared
    (let loop ()
      (let ((pi-fragment
	     (next-token '() '(#\?) "reading PI content" port)))
	(if (eqv? #\> (peek-next-char port))
	    (begin
	      (read-char port)
	      (cons pi-fragment '()))
	    (cons* pi-fragment "?" (loop)))))))

;(define (ssax:read-pi-body-as-name-values port)

; The current pos in the port is inside an internal DTD subset
; (e.g., after reading #\[ that begins an internal DTD subset)
; Skip until the "]>" combination that terminates this DTD
(define (ssax:skip-internal-dtd port)      
  (if (not (find-string-from-port? "]>" port))
    (parser-error port
		  "Failed to find ]> terminating the internal DTD subset")))


; procedure+: 	ssax:read-cdata-body PORT STR-HANDLER SEED
;
; This procedure must be called after we have read a string "<![CDATA["
; that begins a CDATA section. The current position must be the first
; position of the CDATA body. This function reads _lines_ of the CDATA
; body and passes them to a STR-HANDLER, a character data consumer.
;
; The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
; The first STRING1 argument to STR-HANDLER never contains a newline.
; The second STRING2 argument often will. On the first invocation of
; the STR-HANDLER, the seed is the one passed to ssax:read-cdata-body
; as the third argument. The result of this first invocation will be
; passed as the seed argument to the second invocation of the line
; consumer, and so on. The result of the last invocation of the
; STR-HANDLER is returned by the ssax:read-cdata-body.  Note a
; similarity to the fundamental 'fold' iterator.
;
; Within a CDATA section all characters are taken at their face value,
; with only three exceptions:
;	CR, LF, and CRLF are treated as line delimiters, and passed
;	as a single #\newline to the STR-HANDLER
;	"]]>" combination is the end of the CDATA section.
;	&gt; is treated as an embedded #\> character
; Note, &lt; and &amp; are not specially recognized (and are not expanded)!

(define ssax:read-cdata-body 
  (let ((cdata-delimiters (list char-return #\newline #\] #\&)))

    (lambda (port str-handler seed)
      (let loop ((seed seed))
	(let ((fragment (next-token '() cdata-delimiters
				    "reading CDATA" port)))
			; that is, we're reading the char after the 'fragment'
     (case (read-char port)	
       ((#\newline) (loop (str-handler fragment nl seed)))
       ((#\])
	(if (not (eqv? (peek-char port) #\]))
	    (loop (str-handler fragment "]" seed))
	    (let check-after-second-braket
		((seed (if (string-null? fragment) seed
			   (str-handler fragment "" seed))))
	      (case (peek-next-char port)	; after the second bracket
		((#\>) (read-char port)	seed)	; we have read "]]>"
		((#\]) (check-after-second-braket
			(str-handler "]" "" seed)))
		(else (loop (str-handler "]]" "" seed)))))))
       ((#\&)		; Note that #\& within CDATA may stand for itself
	(let ((ent-ref 	; it does not have to start an entity ref
               (next-token-of (lambda (c) 
		 (and (not (eof-object? c)) (char-alphabetic? c) c)) port)))
	  (cond		; "&gt;" is to be replaced with #\>
	   ((and (string=? "gt" ent-ref) (eqv? (peek-char port) #\;))
	    (read-char port)
	    (loop (str-handler fragment ">" seed)))
	   (else
	    (loop 
	     (str-handler ent-ref ""
			  (str-handler fragment "&" seed)))))))
       (else		; Must be CR: if the next char is #\newline, skip it
         (if (eqv? (peek-char port) #\newline) (read-char port))
         (loop (str-handler fragment nl seed)))
       ))))))

; procedure+:	ssax:read-char-ref PORT
;
; [66]  CharRef ::=  '&#' [0-9]+ ';' 
;                  | '&#x' [0-9a-fA-F]+ ';'
;
; This procedure must be called after we we have read "&#" 
; that introduces a char reference.
; The procedure reads this reference and returns the corresponding char
; The current position in PORT will be after ";" that terminates
; the char reference
; Faults detected:
;	WFC: XML-Spec.html#wf-Legalchar
;
; According to Section "4.1 Character and Entity References"
; of the XML Recommendation:
;  "[Definition: A character reference refers to a specific character
;   in the ISO/IEC 10646 character set, for example one not directly
;   accessible from available input devices.]"
; Therefore, we use a ucscode->char function to convert a character
; code into the character -- *regardless* of the current character
; encoding of the input stream.

(define (ssax:read-char-ref port)
  (let* ((base
           (cond ((eqv? (peek-char port) #\x) (read-char port) 16)
                 (else 10)))
         (name (next-token '() '(#\;) "XML [66]" port))
         (char-code (string->number name base)))
    (read-char port)	; read the terminating #\; char
    (if (integer? char-code) (ucscode->char char-code)
      (parser-error port "[wf-Legalchar] broken for '" name "'"))))


; procedure+:	ssax:handle-parsed-entity PORT NAME ENTITIES 
;		CONTENT-HANDLER STR-HANDLER SEED
;
; Expand and handle a parsed-entity reference
; port - a PORT
; name - the name of the parsed entity to expand, a symbol
; entities - see ENTITIES
; content-handler -- procedure PORT ENTITIES SEED
;	that is supposed to return a SEED
; str-handler - a STR-HANDLER. It is called if the entity in question
; turns out to be a pre-declared entity
;
; The result is the one returned by CONTENT-HANDLER or STR-HANDLER
; Faults detected:
;	WFC: XML-Spec.html#wf-entdeclared
;	WFC: XML-Spec.html#norecursion

(define ssax:predefined-parsed-entities
  `(
    (,(string->symbol "amp") . "&")
    (,(string->symbol "lt") . "<")
    (,(string->symbol "gt") . ">")
    (,(string->symbol "apos") . "'")
    (,(string->symbol "quot") . "\"")))

(define (ssax:handle-parsed-entity port name entities
				   content-handler str-handler seed)
  (cond	  ; First we check the list of the declared entities
   ((assq name entities) =>
    (lambda (decl-entity)
      (let ((ent-body (cdr decl-entity)) ; mark the list to prevent recursion
	    (new-entities (cons (cons name #f) entities)))
	(cond
	 ((string? ent-body)
	  (call-with-input-string ent-body
	     (lambda (port) (content-handler port new-entities seed))))
	 ((procedure? ent-body)
	  (let ((port (ent-body)))
	    (begin0
	     (content-handler port new-entities seed)
	     (close-input-port port))))
	 (else
	  (parser-error port "[norecursion] broken for " name))))))
    ((assq name ssax:predefined-parsed-entities)
     => (lambda (decl-entity)
	  (str-handler (cdr decl-entity) "" seed)))
    (else (parser-error port "[wf-entdeclared] broken for " name))))



; The ATTLIST Abstract Data Type
; Currently is implemented as an assoc list sorted in the ascending
; order of NAMES.

(define (make-empty-attlist) '())

; Add a name-value pair to the existing attlist preserving the order
; Return the new list, in the sorted ascending order.
; Return #f if a pair with the same name already exists in the attlist

(define (attlist-add attlist name-value)
  (if (null? attlist) (cons name-value attlist)
      (case (name-compare (car name-value) (caar attlist))
	((=) #f)
	((<) (cons name-value attlist))
	(else (cons (car attlist) (attlist-add (cdr attlist) name-value)))
	)))

(define attlist-null? null?)

; Given an non-null attlist, return a pair of values: the top and the rest
(define (attlist-remove-top attlist)
  (values (car attlist) (cdr attlist)))

(define (attlist->alist attlist) attlist)
(define attlist-fold fold)

; procedure+:	ssax:read-attributes PORT ENTITIES
;
; This procedure reads and parses a production Attribute*
; [41] Attribute ::= Name Eq AttValue
; [10] AttValue ::=  '"' ([^<&"] | Reference)* '"' 
;                 | "'" ([^<&'] | Reference)* "'"
; [25] Eq ::= S? '=' S?
;
;
; The procedure returns an ATTLIST, of Name (as UNRES-NAME), Value (as string)
; pairs. The current character on the PORT is a non-whitespace character
; that is not an ncname-starting character.
;
; Note the following rules to keep in mind when reading an 'AttValue'
; "Before the value of an attribute is passed to the application
; or checked for validity, the XML processor must normalize it as follows: 
; - a character reference is processed by appending the referenced
;   character to the attribute value 
; - an entity reference is processed by recursively processing the
;   replacement text of the entity [see ENTITIES]
;   [named entities amp lt gt quot apos are assumed pre-declared]
; - a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20
;   to the normalized value, except that only a single #x20 is appended for a
;   "#xD#xA" sequence that is part of an external parsed entity or the
;   literal entity value of an internal parsed entity 
; - other characters are processed by appending them to the normalized value "
;
;
; Faults detected:
;	WFC: XML-Spec.html#CleanAttrVals
;	WFC: XML-Spec.html#uniqattspec

(define ssax:read-attributes  ; ssax:read-attributes port entities
 (let ((value-delimeters (append ssax:S-chars '(#\< #\&))))
		; Read the AttValue from the PORT up to the delimiter
		; (which can be a single or double-quote character,
		; or even a symbol *eof*)
		; 'prev-fragments' is the list of string fragments, accumulated
		; so far, in reverse order.
		; Return the list of fragments with newly read fragments
		; prepended.
  (define (read-attrib-value delimiter port entities prev-fragments)
    (let* ((new-fragments
	    (cons (next-token '() (cons delimiter value-delimeters)
		              "XML [10]" port)
	     prev-fragments))
	   (cterm (read-char port)))
      (cond
	((or (eof-object? cterm) (eqv? cterm delimiter))
	  new-fragments)
	((eqv? cterm char-return)	; treat a CR and CRLF as a LF
	  (if (eqv? (peek-char port) #\newline) (read-char port))
	  (read-attrib-value delimiter port entities
	                     (cons " " new-fragments)))
	((memv cterm ssax:S-chars)
	  (read-attrib-value delimiter port entities
	                     (cons " " new-fragments)))
	((eqv? cterm #\&)
	  (cond
	    ((eqv? (peek-char port) #\#)
	      (read-char port)
	      (read-attrib-value delimiter port entities
		(cons (string (ssax:read-char-ref port)) new-fragments)))
	    (else
	      (read-attrib-value delimiter port entities
		(read-named-entity port entities new-fragments)))))
	(else (parser-error port "[CleanAttrVals] broken")))))

		; we have read "&" that introduces a named entity reference.
		; read this reference and return the result of
		; normalizing of the corresponding string
		; (that is, read-attrib-value is applied to the replacement
		; text of the entity)
		; The current position will be after ";" that terminates
		; the entity reference
  (define (read-named-entity port entities fragments)
    (let ((name (ssax:read-NCName port)))
      (assert-curr-char '(#\;) "XML [68]" port)
      (ssax:handle-parsed-entity port name entities
	(lambda (port entities fragments)
	  (read-attrib-value '*eof* port entities fragments))
	(lambda (str1 str2 fragments)
	  (if (equal? "" str2) (cons str1 fragments)
	      (cons* str2 str1 fragments)))
	fragments)))

  (lambda (port entities)
    (let loop ((attr-list (make-empty-attlist)))
      (if (not (ssax:ncname-starting-char? (ssax:skip-S port))) attr-list
	  (let ((name (ssax:read-QName port)))
	    (ssax:skip-S port)
	    (assert-curr-char '(#\=) "XML [25]" port)
	    (ssax:skip-S port)
	    (let ((delimiter 
		   (assert-curr-char '(#\' #\" ) "XML [10]" port)))
	      (loop 
	       (or (attlist-add attr-list 
		     (cons name 
			   (string-concatenate-reverse/shared
			     (read-attrib-value delimiter port entities
						      '()))))
		   (parser-error port "[uniqattspec] broken for " name))))))))
))


; ssax:resolve-name PORT UNRES-NAME NAMESPACES apply-default-ns?
;
; Convert an UNRES-NAME to a RES-NAME given the appropriate NAMESPACES
; declarations.
; the last parameter apply-default-ns? determines if the default
; namespace applies (for instance, it does not for attribute names)
;
; Per REC-xml-names/#nsc-NSDeclared, "xml" prefix is considered pre-declared
; and bound to the namespace name "http://www.w3.org/XML/1998/namespace".
;
; This procedure tests for the namespace constraints:
; http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared

(define (ssax:resolve-name port unres-name namespaces apply-default-ns?)
  (cond
   ((pair? unres-name)		; it's a QNAME
    (cons 
     (cond
     ((assq (car unres-name) namespaces) => cadr)
     ((eq? (car unres-name) ssax:Prefix-XML) ssax:Prefix-XML)
     (else
      (parser-error port "[nsc-NSDeclared] broken; prefix " (car unres-name))))
     (cdr unres-name)))
   (apply-default-ns?		; Do apply the default namespace, if any
    (let ((default-ns (assq '*DEFAULT* namespaces)))
      (if (and default-ns (cadr default-ns))
	  (cons (cadr default-ns) unres-name)
	  unres-name)))		; no default namespace declared
   (else unres-name)))		; no prefix, don't apply the default-ns
	   
	  


; procedure+:	ssax:uri-string->symbol URI-STR
; Convert a URI-STR to an appropriate symbol
(define (ssax:uri-string->symbol uri-str)
  (string->symbol uri-str))

; procedure+:	ssax:complete-start-tag TAG PORT ELEMS ENTITIES NAMESPACES
;
; This procedure is to complete parsing of a start-tag markup. The
; procedure must be called after the start tag token has been
; read. TAG is an UNRES-NAME. ELEMS is an instance of xml-decl::elems;
; it can be #f to tell the function to do _no_ validation of elements
; and their attributes.
;
; This procedure returns several values:
;  ELEM-GI: a RES-NAME.
;  ATTRIBUTES: element's attributes, an ATTLIST of (RES-NAME . STRING)
;	pairs. The list does NOT include xmlns attributes.
;  NAMESPACES: the input list of namespaces amended with namespace
;	(re-)declarations contained within the start-tag under parsing
;  ELEM-CONTENT-MODEL

; On exit, the current position in PORT will be the first character after
; #\> that terminates the start-tag markup.
;
; Faults detected:
;	VC: XML-Spec.html#enum 
;	VC: XML-Spec.html#RequiredAttr
;	VC: XML-Spec.html#FixedAttr
;	VC: XML-Spec.html#ValueType
;	WFC: XML-Spec.html#uniqattspec (after namespaces prefixes are resolved)
;	VC: XML-Spec.html#elementvalid 
;	WFC: REC-xml-names/#dt-NSName

; Note, although XML Recommendation does not explicitly say it,
; xmlns and xmlns: attributes don't have to be declared (although they
; can be declared, to specify their default value)

; Procedure:  ssax:complete-start-tag tag-head port elems entities namespaces
(define ssax:complete-start-tag

 (let ((xmlns (string->symbol "xmlns"))
       (largest-dummy-decl-attr (list ssax:largest-unres-name #f #f #f)))

  ; Scan through the attlist and validate it, against decl-attrs
  ; Return an assoc list with added fixed or implied attrs.
  ; Note that both attlist and decl-attrs are ATTLISTs, and therefore,
  ; sorted
  (define (validate-attrs port attlist decl-attrs)

    ; Check to see decl-attr is not of use type REQUIRED. Add
    ; the association with the default value, if any declared
    (define (add-default-decl decl-attr result)
      (let*-values
	 (((attr-name content-type use-type default-value)
	   (apply values decl-attr)))
	 (and (eq? use-type 'REQUIRED)
	      (parser-error port "[RequiredAttr] broken for" attr-name))
	 (if default-value
	     (cons (cons attr-name default-value) result)
	     result)))

    (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
      (if (attlist-null? attlist)
	  (attlist-fold add-default-decl result decl-attrs)
	  (let*-values
	   (((attr attr-others)
	     (attlist-remove-top attlist))
	    ((decl-attr other-decls)
	     (if (attlist-null? decl-attrs)
		 (values largest-dummy-decl-attr decl-attrs)
		 (attlist-remove-top decl-attrs)))
	    )
	   (case (name-compare (car attr) (car decl-attr))
	     ((<) 
	      (if (or (eq? xmlns (car attr))
		      (and (pair? (car attr)) (eq? xmlns (caar attr))))
		  (loop attr-others decl-attrs (cons attr result))
		  (parser-error port "[ValueType] broken for " attr)))
	     ((>) 
	      (loop attlist other-decls 
		    (add-default-decl decl-attr result)))
	     (else	; matched occurrence of an attr with its declaration
	      (let*-values
	       (((attr-name content-type use-type default-value)
		 (apply values decl-attr)))
	       ; Run some tests on the content of the attribute
	       (cond
		((eq? use-type 'FIXED)
		 (or (equal? (cdr attr) default-value)
		     (parser-error port "[FixedAttr] broken for " attr-name)))
		((eq? content-type 'CDATA) #t) ; everything goes
		((pair? content-type)
		 (or (member (cdr attr) content-type)
		     (parser-error port "[enum] broken for " attr-name "="
			    (cdr attr))))
		(else
		 (ssax:warn port "declared content type " content-type
		       " not verified yet")))
	       (loop attr-others other-decls (cons attr result)))))
	   ))))
	    

  ; Add a new namespace declaration to namespaces.
  ; First we convert the uri-str to a uri-symbol and search namespaces for
  ; an association (_ user-prefix . uri-symbol).
  ; If found, we return the argument namespaces with an association
  ; (prefix user-prefix . uri-symbol) prepended.
  ; Otherwise, we prepend (prefix uri-symbol . uri-symbol)
  (define (add-ns port prefix uri-str namespaces)
    (and (equal? "" uri-str)
	 (parser-error port "[dt-NSName] broken for " prefix))
    (let ((uri-symbol (ssax:uri-string->symbol uri-str)))
      (let loop ((nss namespaces))
	(cond 
	 ((null? nss)
	  (cons (cons* prefix uri-symbol uri-symbol) namespaces))
	 ((eq? uri-symbol (cddar nss))
	  (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
	 (else (loop (cdr nss)))))))
      
  ; partition attrs into proper attrs and new namespace declarations
  ; return two values: proper attrs and the updated namespace declarations
  (define (adjust-namespace-decl port attrs namespaces)
    (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
      (cond
       ((null? attrs) (values proper-attrs namespaces))
       ((eq? xmlns (caar attrs))	; re-decl of the default namespace
	(loop (cdr attrs) proper-attrs 
	      (if (equal? "" (cdar attrs))	; un-decl of the default ns
		  (cons (cons* '*DEFAULT* #f #f) namespaces)
		  (add-ns port '*DEFAULT* (cdar attrs) namespaces))))
       ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
	(loop (cdr attrs) proper-attrs
	      (add-ns port (cdaar attrs) (cdar attrs) namespaces)))
       (else
	(loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))

    ; The body of the function
 (lambda (tag-head port elems entities namespaces)
  (let*-values
   (((attlist) (ssax:read-attributes port entities))
    ((empty-el-tag?)
     (begin
       (ssax:skip-S port)
       (and
	(eqv? #\/ 
	      (assert-curr-char '(#\> #\/) "XML [40], XML [44], no '>'" port))
	(assert-curr-char '(#\>) "XML [44], no '>'" port))))
    ((elem-content decl-attrs)	; see xml-decl for their type
     (if elems			; elements declared: validate!
	 (cond
	  ((assoc tag-head elems) =>
	   (lambda (decl-elem)		; of type xml-decl::decl-elem
	     (values
	      (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem))
	      (caddr decl-elem))))
	  (else
	   (parser-error port "[elementvalid] broken, no decl for " tag-head)))
	 (values		; non-validating parsing
	  (if empty-el-tag? 'EMPTY-TAG 'ANY)
	  #f)			; no attributes declared
	 ))
    ((merged-attrs) (if decl-attrs (validate-attrs port attlist decl-attrs)
		      (attlist->alist attlist)))
    ((proper-attrs namespaces)
     (adjust-namespace-decl port merged-attrs namespaces))
    )
   ;(cerr "proper attrs: " proper-attrs nl)
   ; build the return value
   (values
    (ssax:resolve-name port tag-head namespaces #t)
    (fold-right
     (lambda (name-value attlist)
       (or
	(attlist-add attlist
	   (cons (ssax:resolve-name port (car name-value) namespaces #f)
		 (cdr name-value)))
	(parser-error port "[uniqattspec] after NS expansion broken for " 
	       name-value)))
     (make-empty-attlist)
     proper-attrs)
    namespaces
    elem-content)))))


; procedure+:	ssax:read-external-id PORT
;
; This procedure parses an ExternalID production:
; [75] ExternalID ::= 'SYSTEM' S SystemLiteral
;		| 'PUBLIC' S PubidLiteral S SystemLiteral
; [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'") 
; [12] PubidLiteral ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
; [13] PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9]
;                         | [-'()+,./:=?;!*#@$_%]
;
; This procedure is supposed to be called when an ExternalID is expected;
; that is, the current character must be either #\S or #\P that start
; correspondingly a SYSTEM or PUBLIC token. This procedure returns the
; SystemLiteral as a string. A PubidLiteral is disregarded if present.
 
(define (ssax:read-external-id port)
  (let ((discriminator (ssax:read-NCName port)))
    (assert-curr-char ssax:S-chars "space after SYSTEM or PUBLIC" port)
    (ssax:skip-S port)
    (let ((delimiter 
          (assert-curr-char '(#\' #\" ) "XML [11], XML [12]" port)))
      (cond
        ((eq? discriminator (string->symbol "SYSTEM"))
          (begin0
            (next-token '() (list delimiter) "XML [11]" port)
            (read-char port)	; reading the closing delim
            ))
         ((eq? discriminator (string->symbol "PUBLIC"))
           (skip-until (list delimiter) port)
           (assert-curr-char ssax:S-chars "space after PubidLiteral" port)
           (ssax:skip-S port)
           (let* ((delimiter 
                  (assert-curr-char '(#\' #\" ) "XML [11]" port))
                  (systemid
                    (next-token '() (list delimiter) "XML [11]" port)))
                (read-char port)	; reading the closing delim
                systemid))
         (else
           (parser-error port "XML [75], " discriminator 
		  " rather than SYSTEM or PUBLIC"))))))


;-----------------------------------------------------------------------------
;			Higher-level parsers and scanners
;
; They parse productions corresponding to the whole (document) entity
; or its higher-level pieces (prolog, root element, etc).


; Scan the Misc production in the context
; [1]  document ::=  prolog element Misc*
; [22] prolog ::= XMLDecl? Misc* (doctypedec l Misc*)?
; [27] Misc ::= Comment | PI |  S
;
; The following function should be called in the prolog or epilog contexts.
; In these contexts, whitespaces are completely ignored.
; The return value from ssax:scan-Misc is either a PI-token,
; a DECL-token, a START token, or EOF.
; Comments are ignored and not reported.

(define (ssax:scan-Misc port)
  (let loop ((c (ssax:skip-S port)))
    (cond
      ((eof-object? c) c)
      ((not (char=? c #\<))
        (parser-error port "XML [22], char '" c "' unexpected"))
      (else
        (let ((token (ssax:read-markup-token port)))
          (case (xml-token-kind token)
            ((COMMENT) (loop (ssax:skip-S port)))
            ((PI DECL START) token)
            (else 
              (parser-error port "XML [22], unexpected token of kind "
		     (xml-token-kind token)
		     ))))))))

; procedure+:	ssax:read-char-data PORT EXPECT-EOF? STR-HANDLER SEED
;
; This procedure is to read the character content of an XML document
; or an XML element.
; [43] content ::= 
;	(element | CharData | Reference | CDSect | PI
; 	| Comment)*
; To be more precise, the procedure reads CharData, expands CDSect
; and character entities, and skips comments. The procedure stops
; at a named reference, EOF, at the beginning of a PI or a start/end tag.
;
; port
;	a PORT to read
; expect-eof?
;	a boolean indicating if EOF is normal, i.e., the character
;	data may be terminated by the EOF. EOF is normal
;	while processing a parsed entity.
; str-handler
;	a STR-HANDLER
; seed
;	an argument passed to the first invocation of STR-HANDLER.
;
; The procedure returns two results: SEED and TOKEN.
; The SEED is the result of the last invocation of STR-HANDLER, or the
; original seed if STR-HANDLER was never called.
;
; TOKEN can be either an eof-object (this can happen only if
; expect-eof? was #t), or:
;     - an xml-token describing a START tag or an END-tag;
;	For a start token, the caller has to finish reading it.
;     - an xml-token describing the beginning of a PI. It's up to an
;	application to read or skip through the rest of this PI;
;     - an xml-token describing a named entity reference.
;
; CDATA sections and character references are expanded inline and
; never returned. Comments are silently disregarded.
;
; As the XML Recommendation requires, all whitespace in character data
; must be preserved. However, a CR character (#xD) must be disregarded
; if it appears before a LF character (#xA), or replaced by a #xA character
; otherwise. See Secs. 2.10 and 2.11 of the XML Recommendation. See also
; the canonical XML Recommendation.

	; ssax:read-char-data port expect-eof? str-handler seed
(define ssax:read-char-data
 (let
     ((terminators-usual (list #\< #\& char-return))
      (terminators-usual-eof (list #\< '*eof* #\& char-return))

      (handle-fragment
       (lambda (fragment str-handler seed)
	 (if (string-null? fragment) seed
	     (str-handler fragment "" seed))))
      )

   (lambda (port expect-eof? str-handler seed)

     ; Very often, the first character we encounter is #\<
     ; Therefore, we handle this case in a special, fast path
     (if (eqv? #\< (peek-char port))

         ; The fast path
	 (let ((token (ssax:read-markup-token port)))
	   (case (xml-token-kind token)
	     ((START END)	; The most common case
	      (values seed token))
	     ((CDSECT)
	      (let ((seed (ssax:read-cdata-body port str-handler seed)))
		(ssax:read-char-data port expect-eof? str-handler seed)))
	     ((COMMENT) (ssax:read-char-data port expect-eof?
					     str-handler seed))
	     (else
	      (values seed token))))


         ; The slow path
	 (let ((char-data-terminators
		(if expect-eof? terminators-usual-eof terminators-usual)))

	   (let loop ((seed seed))
	     (let* ((fragment
		     (next-token '() char-data-terminators 
				 "reading char data" port))
		    (term-char (peek-char port)) ; one of char-data-terminators
		    )
	       (if (eof-object? term-char)
		   (values
		    (handle-fragment fragment str-handler seed)
		    term-char)
		   (case term-char
		     ((#\<)
		      (let ((token (ssax:read-markup-token port)))
			(case (xml-token-kind token)
			  ((CDSECT)
			   (loop
			    (ssax:read-cdata-body port str-handler
			        (handle-fragment fragment str-handler seed))))
			  ((COMMENT)
			   (loop (handle-fragment fragment str-handler seed)))
			  (else
			   (values
			    (handle-fragment fragment str-handler seed)
			    token)))))
		     ((#\&)
		      (case (peek-next-char port)
			((#\#) (read-char port) 
			 (loop (str-handler fragment
				       (string (ssax:read-char-ref port))
				       seed)))
			(else
			 (let ((name (ssax:read-NCName port)))
			   (assert-curr-char '(#\;) "XML [68]" port)
			   (values
			    (handle-fragment fragment str-handler seed)
			    (make-xml-token 'ENTITY-REF name))))))
		     (else		; This must be a CR character
		      (if (eqv? (peek-next-char port) #\newline)
			  (read-char port))
		      (loop (str-handler fragment (string #\newline) seed))))
		   ))))))))





; procedure+:	ssax:assert-token TOKEN KIND GI
; Make sure that TOKEN is of anticipated KIND and has anticipated GI
; Note GI argument may actually be a pair of two symbols, Namespace
; URI or the prefix, and of the localname.
; If the assertion fails, error-cont is evaluated by passing it
; three arguments: token kind gi. The result of error-cont is returned.
(define (ssax:assert-token token kind gi error-cont)
  (or
    (and (xml-token? token)
      (eq? kind (xml-token-kind token))
      (equal? gi (xml-token-head token)))
    (error-cont token kind gi)))

;========================================================================
;		Highest-level parsers: XML to SXML

; These parsers are a set of syntactic forms to instantiate a SSAX parser.
; A user can instantiate the parser to do the full validation, or
; no validation, or any particular validation. The user specifies
; which PI he wants to be notified about. The user tells what to do
; with the parsed character and element data. The latter handlers
; determine if the parsing follows a SAX or a DOM model.

; syntax: ssax:make-pi-parser my-pi-handlers
; Create a parser to parse and process one Processing Element (PI).

; my-pi-handlers
;	An assoc list of pairs (PI-TAG . PI-HANDLER)
;	where PI-TAG is an NCName symbol, the PI target, and
;	PI-HANDLER is a procedure PORT PI-TAG SEED
;	where PORT points to the first symbol after the PI target.
;	The handler should read the rest of the PI up to and including
;	the combination '?>' that terminates the PI. The handler should
;	return a new seed.
;	One of the PI-TAGs may be the symbol *DEFAULT*. The corresponding
;	handler will handle PIs that no other handler will. If the
;	*DEFAULT* PI-TAG is not specified, ssax:make-pi-parser will assume
;	the default handler that skips the body of the PI
;	
; The output of the ssax:make-pi-parser is a procedure
;	PORT PI-TAG SEED
; that will parse the current PI according to the user-specified handlers.
;
; The previous version of ssax:make-pi-parser was a low-level macro:
; (define-macro ssax:make-pi-parser
;   (lambda (my-pi-handlers)
;   `(lambda (port target seed)
;     (case target
; 	; Generate the body of the case statement
;       ,@(let loop ((pi-handlers my-pi-handlers) (default #f))
; 	 (cond
; 	  ((null? pi-handlers)
; 	   (if default `((else (,default port target seed)))
; 	       '((else
; 		  (ssax:warn port "Skipping PI: " target nl)
; 		  (ssax:skip-pi port)
; 		  seed))))
; 	  ((eq? '*DEFAULT* (caar pi-handlers))
; 	   (loop (cdr pi-handlers) (cdar pi-handlers)))
; 	  (else
; 	   (cons
; 	    `((,(caar pi-handlers)) (,(cdar pi-handlers) port target seed))
; 	    (loop (cdr pi-handlers) default)))))))))

(define-syntax ssax:make-pi-parser
  (syntax-rules ()
    ((ssax:make-pi-parser orig-handlers)
     (letrec-syntax 
	; Generate the clauses of the case statement
      ((loop
	 (syntax-rules (*DEFAULT*)
	   ((loop () #f accum port target seed) 	; no default
	    (make-case 
	      ((else
		 (ssax:warn port "Skipping PI: " target nl)
		 (ssax:skip-pi port)
		 seed)
		. accum)
	      () target))
	   ((loop () default accum port target seed)
	    (make-case 
	      ((else (default port target seed)) . accum)
	      () target))
	   ((loop ((*DEFAULT* . default) . handlers) old-def accum
	      port target seed)
	    (loop handlers default accum port target seed))
	   ((loop ((tag . handler) . handlers) default accum port target seed)
	    (loop handlers default
	      (((tag) (handler port target seed)) . accum)
	      port target seed))
	   ))
	(make-case 			; Reverse the clauses, make the 'case'
	  (syntax-rules ()
	    ((make-case () clauses target)
	     (case target . clauses))
	    ((make-case (clause . clauses) accum target)
	     (make-case clauses (clause . accum) target)))
	  ))
      (lambda (port target seed)
	(loop orig-handlers #f () port target seed))
       ))))


; syntax: ssax:make-elem-parser my-new-level-seed my-finish-element
;				my-char-data-handler my-pi-handlers

; Create a parser to parse and process one element, including its
; character content or children elements. The parser is typically
; applied to the root element of a document.

; my-new-level-seed
;	procedure ELEM-GI ATTRIBUTES NAMESPACES EXPECTED-CONTENT SEED
;		where ELEM-GI is a RES-NAME of the element
;		about to be processed.
;	This procedure is to generate the seed to be passed
;	to handlers that process the content of the element.
;	This is the function identified as 'fdown' in the denotational
;	semantics of the XML parser given in the title comments to this
;	file.
;
; my-finish-element
;	procedure ELEM-GI ATTRIBUTES NAMESPACES PARENT-SEED SEED
;	This procedure is called when parsing of ELEM-GI is finished.
;	The SEED is the result from the last content parser (or
;	from my-new-level-seed if the element has the empty content).
;	PARENT-SEED is the same seed as was passed to my-new-level-seed.
;	The procedure is to generate a seed that will be the result
;	of the element parser.
;	This is the function identified as 'fup' in the denotational
;	semantics of the XML parser given in the title comments to this
;	file.
;
; my-char-data-handler
;	A STR-HANDLER
;
; my-pi-handlers
;	See ssax:make-pi-handler above
;

; The generated parser is a
;	procedure START-TAG-HEAD PORT ELEMS ENTITIES
;	NAMESPACES PRESERVE-WS? SEED
; The procedure must be called after the start tag token has been
; read. START-TAG-HEAD is an UNRES-NAME from the start-element tag.
; ELEMS is an instance of xml-decl::elems.
; See ssax:complete-start-tag::preserve-ws?

; Faults detected:
;	VC: XML-Spec.html#elementvalid 
;	WFC: XML-Spec.html#GIMatch


(define-syntax ssax:make-elem-parser
  (syntax-rules ()
    ((ssax:make-elem-parser my-new-level-seed my-finish-element
                            my-char-data-handler my-pi-handlers)
  
   (lambda (start-tag-head port elems entities namespaces
			   preserve-ws? seed)

     (define xml-space-gi (cons ssax:Prefix-XML
				(string->symbol "space")))

     (let handle-start-tag ((start-tag-head start-tag-head)
			    (port port) (entities entities)
			    (namespaces namespaces)
			    (preserve-ws? preserve-ws?) (parent-seed seed))
       (let*-values
	(((elem-gi attributes namespaces expected-content)
	  (ssax:complete-start-tag start-tag-head port elems
				   entities namespaces))
	 ((seed)
	  (my-new-level-seed elem-gi attributes
			      namespaces expected-content parent-seed)))
	(case expected-content
	  ((EMPTY-TAG)
	   (my-finish-element
	    elem-gi attributes namespaces parent-seed seed))
	  ((EMPTY)		; The end tag must immediately follow
	   (ssax:assert-token 
	    (and (eqv? #\< (ssax:skip-S port)) (ssax:read-markup-token port))
	    'END  start-tag-head
	    (lambda (token exp-kind exp-head)
	      (parser-error port "[elementvalid] broken for " token 
		     " while expecting "
		     exp-kind exp-head)))
	   (my-finish-element
	    elem-gi attributes namespaces parent-seed seed))
	  (else		; reading the content...
	   (let ((preserve-ws?  ; inherit or set the preserve-ws? flag
		  (cond
		   ((assoc xml-space-gi attributes) =>
		    (lambda (name-value)
		      (equal? "preserve" (cdr name-value))))
		   (else preserve-ws?))))
	     (let loop ((port port) (entities entities)
			(expect-eof? #f) (seed seed))
	       (let*-values
		(((seed term-token)
		  (ssax:read-char-data port expect-eof?
				       my-char-data-handler seed)))
		(if (eof-object? term-token)
		    seed
		    (case (xml-token-kind term-token)
		      ((END)
		       (ssax:assert-token term-token 'END  start-tag-head
			  (lambda (token exp-kind exp-head)
			    (parser-error port "[GIMatch] broken for "
				   term-token " while expecting "
				   exp-kind exp-head)))
		       (my-finish-element
			elem-gi attributes namespaces parent-seed seed))
		      ((PI)
		       (let ((seed 
			  ((ssax:make-pi-parser my-pi-handlers)
			   port (xml-token-head term-token) seed)))
			 (loop port entities expect-eof? seed)))
		      ((ENTITY-REF)
		       (let ((seed
			      (ssax:handle-parsed-entity
			       port (xml-token-head term-token)
			       entities
			       (lambda (port entities seed)
				 (loop port entities #t seed))
			       my-char-data-handler
			       seed))) ; keep on reading the content after ent
			 (loop port entities expect-eof? seed)))
		      ((START)		; Start of a child element
		       (if (eq? expected-content 'PCDATA)
			   (parser-error port "[elementvalid] broken for "
				  elem-gi
				  " with char content only; unexpected token "
				  term-token))
			   ; Do other validation of the element content
			   (let ((seed
				  (handle-start-tag
				     (xml-token-head term-token)
				     port entities namespaces
				     preserve-ws? seed)))
			     (loop port entities expect-eof? seed)))
		      (else
		       (parser-error port "XML [43] broken for "
				     term-token))))))))
	  )))
))))


; syntax: ssax:make-parser user-handler-tag user-handler-proc ...
;
; Create an XML parser, an instance of the XML parsing framework.
; This will be a SAX, a DOM, or a specialized parser depending
; on the supplied user-handlers.

; user-handler-tag is a symbol that identifies a procedural expression
; that follows the tag. Given below are tags and signatures of the
; corresponding procedures. Not all tags have to be specified. If some
; are omitted, reasonable defaults will apply.
;

; tag: DOCTYPE
; handler-procedure: PORT DOCNAME SYSTEMID INTERNAL-SUBSET? SEED
; If internal-subset? is #t, the current position in the port
; is right after we have read #\[ that begins the internal DTD subset.
; We must finish reading of this subset before we return
; (or must call skip-internal-subset if we aren't interested in reading it).
; The port at exit must be at the first symbol after the whole
; DOCTYPE declaration.
; The handler-procedure must generate four values:
;	ELEMS ENTITIES NAMESPACES SEED
; See xml-decl::elems for ELEMS. It may be #f to switch off the validation.
; NAMESPACES will typically contain USER-PREFIXes for selected URI-SYMBs.
; The default handler-procedure skips the internal subset,
; if any, and returns (values #f '() '() seed)

; tag: UNDECL-ROOT
; handler-procedure: ELEM-GI SEED
; where ELEM-GI is an UNRES-NAME of the root element. This procedure
; is called when an XML document under parsing contains _no_ DOCTYPE
; declaration.
; The handler-procedure, as a DOCTYPE handler procedure above,
; must generate four values:
;	ELEMS ENTITIES NAMESPACES SEED
; The default handler-procedure returns (values #f '() '() seed)

; tag: DECL-ROOT
; handler-procedure: ELEM-GI SEED
; where ELEM-GI is an UNRES-NAME of the root element. This procedure
; is called when an XML document under parsing does contains the DOCTYPE
; declaration.
; The handler-procedure must generate a new SEED (and verify
; that the name of the root element matches the doctype, if the handler
; so wishes). 
; The default handler-procedure is the identity function.

; tag: NEW-LEVEL-SEED
; handler-procedure: see ssax:make-elem-parser, my-new-level-seed

; tag: FINISH-ELEMENT
; handler-procedure: see ssax:make-elem-parser, my-finish-element

; tag: CHAR-DATA-HANDLER
; handler-procedure: see ssax:make-elem-parser, my-char-data-handler

; tag: PI
; handler-procedure: see ssax:make-pi-parser
; The default value is '()
 
; The generated parser is a
;	procedure PORT SEED

; This procedure parses the document prolog and then exits to
; an element parser (created by ssax:make-elem-parser) to handle
; the rest.
;
; [1]  document ::=  prolog element Misc*
; [22] prolog ::= XMLDecl? Misc* (doctypedec | Misc*)?
; [27] Misc ::= Comment | PI |  S
;
; [28] doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S? 
;			('[' (markupdecl | PEReference | S)* ']' S?)? '>'
; [29] markupdecl ::= elementdecl | AttlistDecl
;                      | EntityDecl
;                      | NotationDecl | PI
;                      | Comment 
;


; This is ssax:make-parser with all the (specialization) handlers given
; as positional arguments. It is called by ssax:make-parser, see below
(define-syntax ssax:make-parser/positional-args
  (syntax-rules ()
    ((ssax:make-parser/positional-args
       *handler-DOCTYPE
       *handler-UNDECL-ROOT
       *handler-DECL-ROOT
       *handler-NEW-LEVEL-SEED
       *handler-FINISH-ELEMENT
       *handler-CHAR-DATA-HANDLER
       *handler-PI)
  (lambda (port seed)

     ; We must've just scanned the DOCTYPE token 
     ; Handle the doctype declaration and exit to
     ; scan-for-significant-prolog-token-2, and eventually, to the
     ; element parser.
     (define (handle-decl port token-head seed)
       (or (eq? (string->symbol "DOCTYPE") token-head)
	   (parser-error port "XML [22], expected DOCTYPE declaration, found "
		  token-head))
       (assert-curr-char ssax:S-chars "XML [28], space after DOCTYPE" port)
       (ssax:skip-S port)
       (let*-values
	(((docname) (ssax:read-QName port))
	 ((systemid)
	  (and (ssax:ncname-starting-char? (ssax:skip-S port))
	       (ssax:read-external-id port)))
	 ((internal-subset?)
	  (begin (ssax:skip-S port)
	    (eqv? #\[ (assert-curr-char '(#\> #\[)
					"XML [28], end-of-DOCTYPE" port))))
	 ((elems entities namespaces seed)
	  (*handler-DOCTYPE port docname systemid
			    internal-subset? seed))
	 )
	(scan-for-significant-prolog-token-2 port elems entities namespaces
					     seed)))


     ; Scan the leading PIs until we encounter either a doctype declaration
     ; or a start token (of the root element)
     ; In the latter two cases, we exit to the appropriate continuation
     (define (scan-for-significant-prolog-token-1 port seed)
       (let ((token (ssax:scan-Misc port)))
	 (if (eof-object? token)
	     (parser-error port "XML [22], unexpected EOF")
	     (case (xml-token-kind token)
	       ((PI)
		(let ((seed 
		       ((ssax:make-pi-parser *handler-PI)
			port (xml-token-head token) seed)))
		  (scan-for-significant-prolog-token-1 port seed)))
	       ((DECL) (handle-decl port (xml-token-head token) seed))
	       ((START)
		(let*-values
		 (((elems entities namespaces seed)
		   (*handler-UNDECL-ROOT (xml-token-head token) seed)))
		 (element-parser (xml-token-head token) port elems
				 entities namespaces #f seed)))
	       (else (parser-error port "XML [22], unexpected markup "
				   token))))))


     ; Scan PIs after the doctype declaration, till we encounter
     ; the start tag of the root element. After that we exit
     ; to the element parser
     (define (scan-for-significant-prolog-token-2 port elems entities
						  namespaces seed)
       (let ((token (ssax:scan-Misc port)))
	 (if (eof-object? token)
	     (parser-error port "XML [22], unexpected EOF")
	     (case (xml-token-kind token)
	       ((PI)
		(let ((seed 
		       ((ssax:make-pi-parser *handler-PI)
			port (xml-token-head token) seed)))
		  (scan-for-significant-prolog-token-2 port elems entities
						       namespaces seed)))
	       ((START)
		(element-parser (xml-token-head token) port elems
		  entities namespaces #f
		  (*handler-DECL-ROOT (xml-token-head token) seed)))
	       (else (parser-error port "XML [22], unexpected markup "
				   token))))))


     ; A procedure start-tag-head port elems entities namespaces
     ;		 preserve-ws? seed
     (define element-parser
       (ssax:make-elem-parser *handler-NEW-LEVEL-SEED
			      *handler-FINISH-ELEMENT
			      *handler-CHAR-DATA-HANDLER
			      *handler-PI))

     ; Get the ball rolling ...
     (scan-for-significant-prolog-token-1 port seed)
))))



; The following meta-macro turns a regular macro (with positional
; arguments) into a form with keyword (labeled) arguments.  We later
; use the meta-macro to convert ssax:make-parser/positional-args into
; ssax:make-parser. The latter provides a prettier (with labeled
; arguments and defaults) interface to
; ssax:make-parser/positional-args
;
; ssax:define-labeled-arg-macro LABELED-ARG-MACRO-NAME 
;		(POS-MACRO-NAME ARG-DESCRIPTOR ...)
; expands into the definition of a macro
;	LABELED-ARG-MACRO-NAME KW-NAME KW-VALUE KW-NAME1 KW-VALUE1 ...
; which, in turn, expands into
;	POS-MACRO-NAME ARG1 ARG2 ...
; where each ARG1 etc. comes either from KW-VALUE or from
; the deafult part of ARG-DESCRIPTOR. ARG1 corresponds to the first
; ARG-DESCRIPTOR, ARG2 corresponds to the second descriptor, etc.
; Here ARG-DESCRIPTOR describes one argument of the positional macro.
; It has the form 
;	(ARG-NAME DEFAULT-VALUE)
; or
;	(ARG-NAME)
; In the latter form, the default value is not given, so that the invocation of
; LABELED-ARG-MACRO-NAME must mention the corresponding parameter.
; ARG-NAME can be anything: an identifier, a string, or even a number.


(define-syntax ssax:define-labeled-arg-macro
  (syntax-rules ()
    ((ssax:define-labeled-arg-macro
       labeled-arg-macro-name
       (positional-macro-name
	 (arg-name . arg-def) ...))
      (define-syntax labeled-arg-macro-name
	(syntax-rules ()
	  ((labeled-arg-macro-name . kw-val-pairs)
	    (letrec-syntax
	      ((find 
		 (syntax-rules (arg-name ...)
		   ((find k-args (arg-name . default) arg-name
		      val . others)	   ; found arg-name among kw-val-pairs
		    (next val . k-args)) ...
		   ((find k-args key arg-no-match-name val . others)
		     (find k-args key . others))
		   ((find k-args (arg-name default)) ; default must be here
		     (next default . k-args)) ...
		   ))
		(next			; pack the continuation to find
		  (syntax-rules ()
		    ((next val vals key . keys)
		      (find ((val . vals) . keys) key . kw-val-pairs))
		    ((next val vals)	; processed all arg-descriptors
		      (rev-apply (val) vals))))
		(rev-apply
		  (syntax-rules ()
		    ((rev-apply form (x . xs))
		      (rev-apply (x . form) xs))
		    ((rev-apply form ()) form))))
	      (next positional-macro-name () 
		(arg-name . arg-def) ...))))))))


; The definition of ssax:make-parser
(ssax:define-labeled-arg-macro ssax:make-parser
  (ssax:make-parser/positional-args
    (DOCTYPE
      (lambda (port docname systemid internal-subset? seed)
	(when internal-subset?
	  (ssax:warn port "Internal DTD subset is not currently handled ")
	  (ssax:skip-internal-dtd port))
	(ssax:warn port "DOCTYPE DECL " docname " " 
	  systemid " found and skipped")
	(values #f '() '() seed)
	))
    (UNDECL-ROOT
      (lambda (elem-gi seed) (values #f '() '() seed)))
    (DECL-ROOT
      (lambda (elem-gi seed) seed))
    (NEW-LEVEL-SEED)		; required
    (FINISH-ELEMENT)		; required
    (CHAR-DATA-HANDLER)		; required
    (PI ())
    ))


   

;========================================================================
;		Highest-level parsers: XML to SXML
;

; First, a few utility procedures that turned out useful

;     ssax:reverse-collect-str LIST-OF-FRAGS -> LIST-OF-FRAGS
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings.
; We can prove from the general case below that if LIST-OF-FRAGS
; has zero or one element, the result of the procedure is equal?
; to its argument. This fact justifies the shortcut evaluation below.
(define (ssax:reverse-collect-str fragments)
  (cond
    ((null? fragments) '())	; a shortcut
    ((null? (cdr fragments)) fragments) ; see the comment above
    (else
      (let loop ((fragments fragments) (result '()) (strs '()))
	(cond
	  ((null? fragments)
	    (if (null? strs) result
	      (cons (string-concatenate/shared strs) result)))
	  ((string? (car fragments))
	    (loop (cdr fragments) result (cons (car fragments) strs)))
	  (else
	    (loop (cdr fragments)
	      (cons
		(car fragments)
		(if (null? strs) result
		  (cons (string-concatenate/shared strs) result)))
	      '())))))))


;     ssax:reverse-collect-str-drop-ws LIST-OF-FRAGS -> LIST-OF-FRAGS
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings.
; We also drop "unsignificant" whitespace, that is, whitespace
; in front, behind and between elements. The whitespace that
; is included in character data is not affected.
; We use this procedure to "intelligently" drop "insignificant"
; whitespace in the parsed SXML. If the strict compliance with
; the XML Recommendation regarding the whitespace is desired, please
; use the ssax:reverse-collect-str procedure instead.

(define (ssax:reverse-collect-str-drop-ws fragments)
  (cond 
    ((null? fragments) '())		; a shortcut
    ((null? (cdr fragments))		; another shortcut
     (if (and (string? (car fragments)) (string-whitespace? (car fragments)))
       '() fragments))			; remove trailing ws
    (else
      (let loop ((fragments fragments) (result '()) (strs '())
		  (all-whitespace? #t))
	(cond
	  ((null? fragments)
	    (if all-whitespace? result	; remove leading ws
	      (cons (string-concatenate/shared strs) result)))
	  ((string? (car fragments))
	    (loop (cdr fragments) result (cons (car fragments) strs)
	      (and all-whitespace?
		(string-whitespace? (car fragments)))))
	  (else
	    (loop (cdr fragments)
	      (cons
		(car fragments)
		(if all-whitespace? result
		  (cons (string-concatenate/shared strs) result)))
	      '() #t)))))))


; procedure: ssax:xml->sxml PORT NAMESPACE-PREFIX-ASSIG
;
; This is an instance of a SSAX parser above that returns an SXML
; representation of the XML document to be read from PORT.
; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
; that assigns USER-PREFIXes to certain namespaces identified by
; particular URI-STRINGs. It may be an empty list.
; The procedure returns an SXML tree. The port points out to the
; first character after the root element.

(define (ssax:xml->sxml port namespace-prefix-assig)
  (letrec
      ((namespaces
	(map (lambda (el)
	       (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
	     namespace-prefix-assig))

       (RES-NAME->SXML
	(lambda (res-name)
	  (string->symbol
	   (string-append
	    (symbol->string (car res-name))
	    ":"
	    (symbol->string (cdr res-name))))))

       )
    (let ((result
	   (reverse
	    ((ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       '())
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let ((seed (ssax:reverse-collect-str-drop-ws seed))
		     (attrs
		      (attlist-fold
		       (lambda (attr accum)
			 (cons (list 
				(if (symbol? (car attr)) (car attr)
				    (RES-NAME->SXML (car attr)))
				(cdr attr)) accum))
		       '() attributes)))
		 (cons
		  (cons 
		   (if (symbol? elem-gi) elem-gi
		       (RES-NAME->SXML elem-gi))
		   (if (null? attrs) seed
		       (cons (cons '^ attrs) seed)))
		  parent-seed)))

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (if (string-null? string2) (cons string1 seed)
		   (cons* string2 string1 seed)))

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (ssax:warn port
			   "Internal DTD subset is not currently handled ")
		     (ssax:skip-internal-dtd port))
	       (ssax:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f '() namespaces seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() namespaces seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
		  (cons
		   (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
		   seed))))
	     )
	    port '()))))
      (cons '*TOP*
	    (if (null? namespace-prefix-assig) result
		(cons
		 (list '^ (cons '*NAMESPACES* 
				 (map (lambda (ns) (list (car ns) (cdr ns)))
				      namespace-prefix-assig)))
		      result)))
)))

; For backwards compatibility
(define SSAX:XML->SXML ssax:xml->sxml)

)
