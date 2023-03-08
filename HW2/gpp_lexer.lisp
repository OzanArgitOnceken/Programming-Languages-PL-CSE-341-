

(defun getTokens ()

	(let((key 
					'("equal" "or" "and" "not" "less" 
					"nil" "list" "append" "concat" "set"
					"deffun" "for" "if" "exit" "load"
					"disp" "false" "true" "-" "+"  
	 	 			 "*" "/" "(" ")" "," "**" )				;"="
			)
		 (value 
		 
		 				'("KW_EQUAL" "KW_OR" " KW_AND" "KW_ NOT"  "KW_LESS" 
					   "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" 
					   "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" 
					   "KW_DISP" "KW_FALSE" "KW_TRUE" "OP_MINUS" "OP_PLUS" 
					   "OP_MULT" "OP_DIV"  "OP_OP" "OP_CP"  "OP_COMMA" "OP_DBLMULT")		;"OP_EQ"
			)
		)
		(pairlis key value)
	)
)	
(defun isAlpha (chr);;is it on alphabet 

	(let (
			(c (char-int (coerce chr 'character)))
		)
		(and (<= c (char-int #\z)) (>= c (char-int #\A))  )
	)
)
(defun isNumeric (chr);;is it numerical
	(let ((c (char-int (coerce chr 'character))))
		(and (>= c (char-int #\0)) (<= c (char-int #\9)) )
	)
)
(defun isQuomarkp (chr)
	(
		eq (char-code (coerce chr 'character)) 
		34
	)
)

(defun OPtokenize (token myList)
	;;Operator state of DFA.

	 (let ((value (assoc token myList :test #'string=)))
		(if value (format nil "~a" (cdr value)) nil))
)
(defun isZero (chr)
	(
		eq (char-code (coerce chr 'character))
			48
	)
)
(defun valueTokenizer (token)

	(assert (isNumeric (string (char token 0))))

	(if 
		(and (> (length token) 1) (isZero (substring token 0 1))) 
		(return-from valueTokenizer (format t " \"SYNTAX ERROR: '~a' : '~a' \"" token (substring token 0 1)))
	)
	(
		loop for c across token 
		do (if 
			(
				not (isNumeric c)
			)
			(return-from valueTokenizer (format t " \"SYNTAX ERROR: '~a' : '~a' \"" token c))
		)
	)
	(format nil "VALUE")
)
(defun IdentifierTokenize (token myList)

	(assert (
			isAlpha (string (char token 0))
			)
	)

	;; scan for each one
	(loop for c across token 
		do (
			if (not (or (isAlpha c) (isNumeric c))) 
				(return-from IdentifierTokenize (format t " \"SYNTAX ERROR: '~a' : '~a' \"" token c))
		)
	)

	(let ((kw (kwTokenizer token myList)))
		(if (null kw) (format nil "IDENTIFIER") kw);;If null return "IDENTIFIER" else return the kw
	)
)
	
(defun kwTokenizer (token myList)
	 (let 
	 		(
				(
					value (assoc token myList :test #'string=)
				)
	 		)
		(if value (format nil "~a" (cdr value)) nil)
	)
)


(defun tokenize (token myList)
	(let ((c (string (char token 0)))) 
	(cond (	(isAlpha c) (IdentifierTokenize token myList))  
	 	   		(
				    (isNumeric c) (valueTokenizer token)
				)			
	 	   		(
					(isQuomarkp c) (stringTokenize token)
				)
	 	   	(	t 	
				(if (OpTokenize token myList) 					;; operator
	 	   			(OpTokenize token myList) (format t " \"SYNTAX ERROR: '~a' : '~a' \"" token c)
				)
			)
	)
	)
)	

(defun interpretgpp (&optional NameOfFile)
	(if NameOfFile (interpretFile NameOfFile) (shellinterpret));;file interpret returns nil if file does not exists
        ;;if it does not existst it returns the shellinterpret function
)
(defun Shellinterpret()
    ;;(format t "~a" myLine)
	(loop 
        (format t "~%>>> ") 
        (defparameter myLine 0)
        (setq myLine(read-line))
        
        (let ((myList (inputFormyList myline)))
            ;;(format t "~a" myList)
            (format t "(")
            (if (string= (car myList) ";;") (format t "(\";;\" \"COMMENT\")");;if it is a comment line do not read the rest.
                (map nil #'(lambda (token) (format t "(\"~a\"" token)(format t " \"~a\")" (tokenize token (getTokens))) ) myList)
                ;;each token taken by map and written with lambda 
            )
            (format t ")")
        )

    )
)
(defun ListForString (myList);; this function makes argument an array
    	
        ;;(format t "~{~a~}" myList)
        (format nil "~{~A~}" myList) 
)

(defun inputFormyList (str)

	(setq str 	(ListForString (map 'list #'(lambda (c) 
				(if (isBrackets c) (concatenate 'string " " (string c) " ") ;;if it's elements are character then make it concatenate
				(string c))) ;;else do not concatenate
                (cleanUp str)
			)
		)
	)

	(let (
			(myList (loop for idx from 0 to (- (length str) 1)
					when (char= (aref str idx) #\") collect idx
				)
			)
			(i1 '()) (i2 '()) (space_i '())
		)
			(loop while myList do (setq i1 (car myList)) 
							do (setq i2 (car (cdr myList)))
							do (setq myList (cdr (cdr myList)))
							do (setq space_i
					(loop for idx from 0 to (- (length str) 1)
					when (and (< idx i2)  (> idx i1) (char= (aref str idx) #\Space)) 
					collect idx))
			)

		(splitSpaceSeq (ListForString
			(loop for idx from 0 to (- (length str) 1)
			if (member idx space_i) collect #\. else collect (aref str idx)
			))
		)
	)
)
(defun isSpace (charVariable) 
	(char= Charvariable #\Space)
)

(defun splitSpaceSeq;; some string ->('some' 'string')
                    (
						string &key (isSpace #'isSpace)
					)

  	(
		loop :for start = (position-if-not isSpace string)
		:then (position-if-not isSpace string :start (1+ end))
		:for end = (and start (position-if isSpace string :start start))
		:when start :collect (subseq string start end)
		:while end
	)
)
(defun isBrackets (chr)
	"If chr := ( or ) returns true, nil otherwise."
	(let (
			(
				c (char-int (coerce chr 'character))
            )
		)
		(or (= c 40) (= c 41))
	)
) 
(defun cleanUp (str)
	;;Cleans str
	(let (
        (trim-myList '(#\Space #\Newline #\Backspace #\Tab #\Return ))
        )
	(string-trim trim-myList str)
    )
)

(defun fileInterpreter(seq)

	(let ((myList (inputFormyList seq)))

		(format t "(")
		(if (string= (car myList) ";;") (format t "(\";;\" \"COMMENT\")")
			(map nil #'(lambda (token) (format t "(\"~a\"" token)(format t " \"~a\")" (tokenize token (getTokens))) ) myList)
			;;each token taken by map and written with lambda 
		)
		(format t ")")
	)
)


(defun stringtokenize (token)
	(
		assert (
			isQuomarkp (string (char token 0))
			)
	)
	(format nil "STRING")
)

(defun interpretFile (NameOfFile)
	(let (
			(in (open NameOfFile :if-does-not-exist nil));;if file does not exists return nil
			)
	  	(
		when in (loop for line = (read-line in nil)
	         	while line do (fileInterpreter line)
				) (close in)
		)
	  	(unless in 
	  		(format t "ERROR: No such file: '~a'" NameOfFile)
	  	)
	)
)
(if *args* (interpretgpp (car *args*)) (interpretgpp))
