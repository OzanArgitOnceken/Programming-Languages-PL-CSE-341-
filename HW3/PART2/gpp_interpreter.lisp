(setq previousCharacter nil)(setq isItIdentifier nil)(defvar tokens (list))(setq breakLoop nil)(setq stringLine nil)(defvar oc nil)(setq temp3 nil)
(defvar foundedDOt nil)(setq counter 0)(defvar starFoundDouble nil)(defvar subStr nil)(defvar classPrevious nil)(defvar isNumber nil)(setq glblTemp 0)
(defvar classCurrent)(defvar done nil)
(setq d 0)(defconstant LETTER  1)(defconstant DIGIT   2)(defconstant OTHER 3)(defconstant OPERATOR 4)(defconstant UNKNOWN 5)
(defconstant operatorList '("'" "+" "-" "/" "*" "(" ")" "**" "\"" ","))
(defconstant keywordList '("defvar" "and" "or" "not" "equal" "gt" "nil" "list" "append" "concat" "set" "deffun" "while" "if" "exit" "load" "disp" "true" "false"))
(defconstant othersList '(" " "\n" "\t" "\0"))

(defun isItOperator(readCharacter)(loop for key in operatorList do(cond ((equal  (string readCharacter) key)(return-from isItOperator t)))  )(return-from isItOperator nil))

(defun isOth(readCharacter)(loop for key in othersList do
    (cond ((equal  (string readCharacter) key)	(return-from isOth t))))(return-from isOth nil)
)

(defun isKeyw(readCharacter)(loop for key in keywordList do(cond ((equal  (string readCharacter) key)(return-from isKeyw t))))(return-from isKeyw nil))

(defun detectChar(c)(cond((alpha-char-p c)(setq classCurrent LETTER))((not (null (digit-char-p c)))(setq classCurrent DIGIT))((equal (isItOperator c) t)(setq classCurrent OPERATOR))
((equal (isOth c) t)(setq classCurrent OTHER))(t(setq classCurrent UNKNOWN))))

(defun wordCheck (string1)(cond((equal (isKeyw string1) t)(setq tokens (append tokens(list(list "KEYWORD"(identifierDetermine (string string1)))))))
  	(t (setq tokens (append tokens (list(list "ID" (string string1))))))))

(defun operatorCheck(op)(cond((equal (isItOperator op) t)(setq tokens (append tokens (list(list "OPERATOR" (operatorDetermine (string op))))))(setq subStr nil))))

(defun identifierDetermine (string)(cond((equal string "defvar")(return-from identifierDetermine "DEFV"))((equal string "and")
(return-from identifierDetermine "OP_AND"))((equal string "or")
(return-from identifierDetermine "OP_OR"))
((equal string "not")(return-from identifierDetermine "OP_NOT"))((equal string "equal")(return-from identifierDetermine "OP_EQ")
)((equal string "gt")(return-from identifierDetermine "OP_GT"))((equal string "nil")(return-from identifierDetermine "KW_NIL"))(
(equal string "list")(return-from identifierDetermine "KW_LIST"))((equal string "append")(return-from identifierDetermine "KW_APPEND")
)((equal string "concat")(return-from identifierDetermine "KW_CONCAT"))((equal string "set")(return-from identifierDetermine "OP_SET"))(
  (equal string "deffun")(return-from identifierDetermine "DEFF"))((equal string "while")(return-from identifierDetermine "KW_WHILE"
  
  ))((equal string "if")(return-from identifierDetermine "KW_IF"))((equal string "exit")(return-from identifierDetermine "KW_EXIT"
  ))((equal string "load")(return-from identifierDetermine "KW_LOAD"))((equal string "disp")(return-from identifierDetermine "KW_DISP"))
  ((equal string "true")(return-from identifierDetermine "KW_TRUE"))((equal string "false")(return-from identifierDetermine "KW_FALSE"))))

(defun operatorDetermine (string)(cond((equal string "+")(return-from operatorDetermine "OP_PLUS"))(
  (equal string "-")(return-from operatorDetermine "OP_MINUS"))((equal string "/")(return-from operatorDetermine "OP_DIV"))((equal string "*")
  (return-from operatorDetermine "OP_MULT"))((equal string "(")(return-from operatorDetermine "OP"))((equal string ")")
  (return-from operatorDetermine "CP"))((equal string "**")(return-from operatorDetermine "OP_DBLMULT"))((equal string "\"")
  (return-from operatorDetermine "OP_OC"))((equal string "\"")(return-from operatorDetermine "OP_CC"))((equal string ",")
  (return-from operatorDetermine "OP_COMMA"))((equal string "'")(return-from operatorDetermine "'"))))


(defun gppLex (str)(loop for c across str do(detectChar c)(cond((equal breakLoop nil)(cond((and (equal (string c) ";"))
(setq isNumber nil)(setq isItIdentifier nil)
(cond((< (+ counter 1) (length str))(setq temp3 (elt str (+ 1 counter)))(cond((equal (string temp3) ";")
(setq tokens (append tokens (list(list "comment"))))
(setq breakLoop t))(t(format t "~d ~%" "syntax error")(setq breakLoop t))))(t(format t "~d ~%" "syntax error")(setq breakLoop t))))
((and (equal classPrevious nil) (equal classCurrent DIGIT))(setq isNumber t)(setq isItIdentifier nil))
((and (equal classPrevious OTHER) (equal classCurrent DIGIT))
 (setq isNumber t)(setq isItIdentifier nil)(setq subStr nil))
 ((and (equal classPrevious OPERATOR) (equal classCurrent DIGIT))
 (setq isNumber t)(setq isItIdentifier nil)
  (setq subStr nil))((and (equal classPrevious DIGIT) (equal classCurrent DIGIT))(setq isNumber t)(setq isItIdentifier nil))
  ((and (equal classPrevious LETTER) 
  (equal classCurrent DIGIT))(setq isNumber nil)(setq isItIdentifier nil)(setq breakLoop t)(format t "~d ~%" "syntax error"))
  ((and (equal isNumber t) 
  (equal classCurrent OTHER))(setq isNumber nil)(setq tokens (append tokens (list(list "VALUE" (string subStr)))))(setq subStr nil))
  ((and (equal isNumber t) (equal classCurrent LETTER))(setq isNumber nil)(setq breakLoop t)(format t "~d ~%" "syntax error"))
  (
    (and (and(equal isNumber t)(equal (string c) "."))(equal (+ 1 counter)(length str)))(
      setq isNumber nil)(setq breakLoop t)(format t "~d ~%" "syntax error"))
  ((and (and (equal isNumber t)(equal foundedDOt nil)) (equal (string c) "."))(setq isNumber t)(setq foundedDOt t)(setq isItIdentifier nil))
  ((and (equal foundedDOt t) (equal (string c) "."))(setq isNumber nil)(setq foundedDOt nil)(setq breakLoop t)(format t "~d ~%" "syntax error"))
  ((and (equal classPrevious nil) (equal classCurrent LETTER))(setq isItIdentifier t)(setq isNumber nil))((and (equal classPrevious OTHER) 
  (equal classCurrent LETTER))(setq isItIdentifier t)(setq isNumber nil)
  (setq subStr nil))((and (equal classPrevious OPERATOR) 
  (equal classCurrent LETTER))(setq subStr nil)(setq isItIdentifier t)(setq isNumber nil))((and (equal isItIdentifier t)(equal classCurrent OTHER))
      (setq isItIdentifier nil)(wordCheck subStr)(setq subStr nil))((and (equal isItIdentifier t)(equal (string c) "."))(setq isItIdentifier nil)
      (setq isNumber nil)(setq breakLoop t)(wordCheck subStr)(format t "~d ~%" "syntax error"))((and(equal classCurrent UNKNOWN)
      (not(equal (string c) "\0")))(setq isItIdentifier nil)(setq isNumber nil)(setq breakLoop t)(format t "~d ~%" "syntax error"))

( (equal classCurrent OPERATOR)(if (equal isItIdentifier t)(wordCheck subStr))(if (equal isNumber t)(setq tokens (append tokens
(list(list "VALUE" (string subStr)))))(setq subStr nil))( if(equal isNumber t) (setq isNumber nil))( if(equal isItIdentifier t) 
(setq isItIdentifier nil))(cond((equal (string c) "-")(cond((< (+ counter 1) (length str))(setq temp3 nil)(setq temp3 (elt str (+ 1 counter)))
  (cond((not(null (digit-char-p temp3)))(setq done t))(t(operatorCheck "-")(setq subStr nil))))(t(operatorCheck "-")(setq subStr nil)))
  )((and (equal(string c) "\"")(equal oc nil))(setq oc t)(format t "~d ~%" "OP_OC")(setq tokens (append tokens (list(list "OPERATOR" "OP_OC")))
  )(setq subStr nil))((and (equal(string c) "\"")(equal oc t))(setq oc nil)(setq tokens (append tokens (list(list "OPERATOR" "OP_CC")))
  )(setq subStr nil))((equal (string c) "*")(cond((equal starFoundDouble t)(setq starFoundDouble nil))((< (+ counter 1) (length str))
  (setq temp3 (elt str (+ 1 counter)))(cond((and(equal (string c) "*")(equal (string temp3) "*"))(setq starFoundDouble t)(operatorCheck "**")
  (setq subStr nil))(t(operatorCheck "*")(setq subStr nil))))(t(operatorCheck "*")(setq subStr nil))))(t(operatorCheck c)(setq subStr nil)))))
)
)
  (setq subStr (concatenate 'string subStr (string c)))(cond((and (= (- (length str) 1)  counter)(equal isNumber t))(setq tokens 
  (append tokens (list(list "VALUE" (string subStr)))))(setq isNumber nil))((and (= (- (length str) 1)  counter)(equal isItIdentifier t))
    (wordCheck subStr)(setq isItIdentifier nil)))(setq classPrevious classCurrent)(setq previousCharacter c)(setf counter (+ counter 1))))
(setq tree nil)(setq tokens nil)(setq isCorrect t)(setq currentLayer 0)(setq counterList (list 0))

(defun interpreter(tokenList)(parserMain))
(defun parserMain ()(cond((null  tokens))( ( equal t (  or (equal (string(getFirstTokenName)) "'") (equal (string (getFirstTokenName)) "null")  ) ) 
  ( parseEXPLISTI )(parserMain))((equal t (or (equal (string(getSecondTokenName)) "KW_APPEND")(equal (string (getSecondTokenName)) "KW_CONCAT")
  (equal (string (getSecondTokenName)) "KW_LIST")))( parseEXPLISTI)(parserMain))((equal t(equal(string   (getFirstTokenType) ) "comment"))(parserMain))
      (t (parseEXPI)(parserMain)))
)

(defun parseEXPI()(cond((null tokens) nil)((equal (string(getFirstTokenType))"ID")(parseIdentifier))((and(equal     
  (string(getSecondTokenType))"ID")(equal(string(getFirstTokenName))"OP"))(isOP)(parseIdentifier)(parseEXPLISTI)(isCP))
  ((equal (string (getFirstTokenType)  ) "VALUE")(parseValue))((and(equal(string(getSecondTokenType))  "KEYWORD")
  (not (equal(string(getFirstTokenName))"KW_FALSE")) 
  (not(equal (string (getFirstTokenName)) "KW_TRUE"))(not(equal (string (getFirstTokenName)) "KW_NIL")))(parseKeyword))
  ((equal (string (getSecondTokenType)) "OPERATOR") 
  (parseOperator))(t(throwError "in parseEXPI"))))

(defun parseEXPB ()(cond((or (equal (string (getFirstTokenName)) "KW_FALSE")(equal (string (getFirstTokenName)) "KW_TRUE")
  (equal (string (getFirstTokenName))"KW_NIL"))
  (parseBinaryValue))((equal (string (getFirstTokenType)) "ID")(parseIdentifier))
  ((or (equal (string (getSecondTokenName)) "KW_FALSE")(equal (string (getSecondTokenName)) "KW_TRUE")
  (equal (string (getSecondTokenName)) "KW_NIL"))(parseBinaryValue))
  ((equal (string (getSecondTokenType)) "KEYWORD")(parseKeyword))((equal (string (getSecondTokenType)) "KEYWORD")
  (parseKeyword))(t(throwError "in parseEXPB"))))

(defun parseEXPLISTI ()(cond((equal(string (cadr(nth 0 tokens))) "OP")(cond((equal (string (getSecondTokenName)) "KW_CONCAT") 
  (parseConcat))((equal (string (getSecondTokenName)) "KW_APPEND")(parseAppend))((equal (string (getSecondTokenName)) "KW_LIST")  
  (parseList))((equal (string (getSecondTokenType)) "ID")(parseIdentifier))))((equal (string (cadr(nth 0 tokens))) "'")(parseListValue))
  ((equal (string (getFirstTokenType)) "null")(setq tokens (cdr tokens)))
  (t(throwError "in parseEXPLISTI"))))

(defun parseBinaryValue ()(cond((or(equal (string (getFirstTokenName)) "KW_FALSE")(equal (string (getFirstTokenName)) "KW_TRUE")
  (equal (string (getFirstTokenName)) "KW_NIL"))
  (setq tokens (cdr tokens)))(t(throwError "in Binary Value"))))

(defun parseIdentifier ()(cond((equal (string (getFirstTokenType)) "ID")(setq tokens (cdr tokens)))(t(throwError "in parseId"))))

(defun parseIDList ()(cond((equal (string (getFirstTokenName)) "OP")(isOP)(cond((equal (string (getFirstTokenName)) "CP")(isCP))(t(parseIDList))))(
  (equal (string (getFirstTokenType)) "ID")(parseIdentifier)(cond((equal (string (getFirstTokenName)) "CP")(isCP))(t(parseIDList))))
  (t(throwError "in parseIdList"))))

(defun parseValues()(cond((equal t (and (equal (string (getSecondTokenType)) "VALUE")(equal (string (getFirstTokenType)) "VALUE")))(parseValue)
  (parseValues))((equal (string (getFirstTokenType)) "VALUE")(parseValue))))

(defun parseListValue()(cond((equal (string (getFirstTokenName)) "'")(setq tokens (cdr tokens))(isOP)
  (cond((not (equal (string (getFirstTokenName)) "CP"))(parseValues)))(isCP))))

(defun parseValue()(setq tokens(cdr tokens)))

(defun parseOperator ()(cond((or (string= "OP_PLUS" (getSecondTokenName))(string= "OP_MINUS" (getSecondTokenName))(string= "OP_DIV" (getSecondTokenName)) 
  (string= "OP_MULT" (getSecondTokenName))(string= "OP_DBLMULT" (getSecondTokenName)))(isOP)(setq tokens (cdr tokens))(parseEXPI)(parseEXPI)(isCP))
  (t(throwError "parseOperator"))))

(defun parseKeyword ()(cond((equal (string (getSecondTokenName))  "OP_AND")(parseAnd))((equal (string (getSecondTokenName))  "DEFV")(parseDefvar))
  ((equal (string (getSecondTokenName)) "OP_OR")(parseOr))((equal (string (getSecondTokenName)) "OP_NOT")(parseNot))
  ((equal (string (getSecondTokenName)) "OP_EQ")(parseEqual))((equal (string (getSecondTokenName)) "OP_GT")(parseLess))
  ((equal (string (getSecondTokenName)) "OP_SET")(parseSet))((equal (string (getSecondTokenName)) "DEFF")(parseDeffun))
  ((equal (string (getSecondTokenName)) "KW_WHILE")(parseFor))((equal (string (getSecondTokenName)) "KW_IF")(parseIf))
  ((equal (string (getSecondTokenName)) "KW_EXIT")(parseExit))((equal (string (getSecondTokenName)) "KW_LOAD")(parseLoad))
  ((equal (string (getSecondTokenName)) "KW_DISP")(parseDisp))))

(defun parseSet ()(cond( (or(equal (string (cadr(nth 3 tokens))) "KW_TRUE")(equal (string(cadr(nth 3 tokens))) "KW_FALSE")
  (equal (string(cadr(nth 3 tokens))) "KW_NIL"))(isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseBinaryValue)(isCP))(
  (or(equal (string (cadr(nth 3 tokens))) "'")(equal (string (cadr(nth 4 tokens))) "KW_APPEND")(equal (string (cadr(nth 4 tokens))) "KW_CONCAT")
  (equal (string (cadr(nth 4 tokens))) "KW_LIST")) (isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseEXPLISTI)(isCP))(t(isOP)
  (setq tokens (cdr tokens))(parseIdentifier)(parseEXPI)(isCP))))

(defun parseAppend ()(isOP)(setq tokens (cdr tokens))(parseEXPI)(parseEXPLISTI)(isCP))

(defun parseConcat ()(isOP)(setq tokens (cdr tokens))(parseEXPLISTI)(parseEXPLISTI)(isCP))

(defun parseAnd ()(isOP)(setq tokens (cdr tokens))(parseEXPB)(parseEXPB)(isCP))

(defun parseOr ()(isOP)(setq tokens (cdr tokens))(parseEXPB)(parseEXPB)(isCP))

(defun parseNot ()(isOP)(setq tokens (cdr tokens))(parseEXPB)(isCP))

(defun parseEqual ()(isOP)(setq tokens (cdr tokens))(cond((equal t (or (equal (string (getFirstTokenName)) "OP_AND") 
  (equal (string(getFirstTokenName)) "KW_FALSE") (equal (string(getFirstTokenName)) "KW_TRUE")(equal (string(getFirstTokenName)) "OP_EQ") 
  (equal (string(getFirstTokenName)) "OP_NOT") (equal (string (getFirstTokenName)) "OP_OR") (equal (string (getFirstTokenName)) "KW_NIL")))(parseEXPB)(parseEXPB))
  (t(parseEXPI)(parseEXPI)))(isCP))

(defun parseFor ()(isOP)(setq tokens (cdr tokens))(isOP)(parseIdentifier)(parseEXPI )(parseEXPI)(isCP)(parseEXPLISTI)(isCP))

(defun parseWhile ()(isOP)(setq tokens (cdr tokens))(isOP)(parseIdentifier)(parseEXPB)(isCP)(parseEXPLISTI)(isCP))

(defun parseIf ()(isOP)(setq tokens (cdr tokens))(parseEXPB)(parseEXPLISTI)(cond((not(equal (string (getFirstTokenName)) "CP"))(parseEXPLISTI)))(isCP))

(defun parseDeffun ()(isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseIdList)(parseExplisti)(isCP))

(defun parseLoad ()(isOP)(setq tokens (cdr tokens))(parseIdentifier)(isCP))

(defun parseLess ()(isOP)(setq tokens (cdr tokens))(parseEXPI)(parseEXPI)(isCP))

(defun parseExit ()(isOP)(setq tokens (cdr tokens))(isCP))

(defun parseList ()(isOP)(setq tokens (cdr tokens))(dotimes (x 1000)(cond((equal (string (cadr(nth 0 tokens))) "CP")(setq x 1001))(t (parseValue))))(isCP))

(defun parseDefvar ()(cond((or(equal (string (cadr(nth 3 tokens))) "KW_TRUE") (equal (string(cadr(nth 3 tokens))) "KW_FALSE")(equal (string(cadr(nth 3 tokens))) "KW_NIL"))
  (isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseBinaryValue)(isCP))((or(equal (string (cadr(nth 3 tokens))) "'")
  (equal (string (cadr(nth 4 tokens))) "KW_APPEND")(equal (string (cadr(nth 4 tokens))) "KW_CONCAT")(equal (string (cadr(nth 4 tokens))) "KW_LIST"))
  (isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseEXPLISTI)(isCP))(t(isOP)(setq tokens (cdr tokens))(parseIdentifier)(parseEXPI)(isCP))))

(defun parseDisp ()(isOP)(setq tokens (cdr tokens))(parseEXPI)(isCP))

(defun isOP ()(cond((equal (string (cadar tokens)) "OP")(setq tokens (cdr tokens)))(t (throwError "in open Paranthesis"))))

(defun isCP ()(cond((equal (string (cadar tokens)) "CP")(setq tokens (cdr tokens))(setq currentLayer (- currentLayer 1)))(t(throwError "in close Paranthesis"))))

(defun getFirstTokenType ()(caar tokens))

(defun getSecondTokenType ()(caadr tokens))

(defun getFirstTokenName ()(cadar tokens))

(defun getSecondTokenName ()(cadadr tokens))

(defun throwError(str)(error (concatenate 'string "syntax error: Unexpected token in " str)))

(setq variables nil)
(setq functions (list))

(defun evaluateLine (&optional(tokensEval tokenList))(let ((currentToken) (a 0) (b 0) (c 2) (d)(result 0)(e)(len (length tokensEval)))
  (setq currentToken (getTokenName (nth 1 tokensEval)))(cond((equal (string currentToken) "DEFF")(setq functions (append functions 
  (list(getTokenName (nth 2 tokensEval))))))((equal (string (getTokenType (nth 1 tokensEval))) "ID")(cond((equal (getFromFunctions 
  (getTokenName (nth 1 tokensEval))) nil)(print "syntax error: Functions is not defined")(exit))))((equal (string currentToken) "KW_DISP")
  (setq e (countParanthesis (subseq tokensEval 2)))(setq c 0) (setq c (+ c e))(setq result (evaluateLine (subseq tokensEval 2 (- len 1)))))
  
  ((or (equal (string currentToken) "OP_PLUS") (equal (string currentToken) "OP_MINUS") (equal (string currentToken) "OP_MULT")(equal(string currentToken) "OP_DIV")
  (equal (string currentToken) "OP_DBLMULT"))(cond((equal (string (getTokenType (nth 2 tokensEval))) "VALUE")(setq a (parse-integer (getTokenName (nth 2 tokensEval)))) 
  (setq c 3))((equal (string (getTokenType (nth 2 tokensEval))) "ID")(setq a (parse-integer(getFromVariables (getTokenName (nth 2 tokensEval))))) 
  (setq c 3))(t(setq e (countParanthesis (subseq tokensEval 2)))(if (equal e nil) (progn (setq result nil) (setq c nil)) (setq c (+ c e)))
  (if (and (not (equal c nil)) (< c len)) (setq a (evaluateLine (subseq tokensEval 2 c))) (setq result nil))))(if (equal c nil) 
  (setq result nil) (setq d (+ c 2)))(cond((not (equal result nil))(cond((equaL (string (getTokenType (nth c tokensEval))) "VALUE")
  (setq b (parse-integer (getTokenName (nth c tokensEval)))))((equaL (string (getTokenType (nth c tokensEval))) "ID")
  (setq b (parse-integer(getFromVariables (getTokenName (nth c tokensEval))))))
  (t(setq e (countParanthesis (subseq tokensEval c)))(if (equal e nil) (progn (setq result nil) (setq d nil)) (setq d (+ c e)))
  (if (and (not (equal d nil)) (< d len)) (progn (setq b (evaluateLine (subseq tokensEval c d))) (setq d (+ d 1))) (setq result nil))))))
  (cond((and (not (equal result nil)) (equal d len) (not (equal a nil)) (not (equal b nil)))(if (equal (string currentToken) "OP_PLUS") 
  (setq result (+ a b))) (if (equal (string currentToken) "OP_MINUS") (setq result (- a b)))(if (equal (string currentToken) "OP_MULT") 
  (setq result (* a b)))(if (equal (string currentToken) "OP_DBLMULT") (setq result (expt a b)))(if (equal (string currentToken) "OP_DIV") 
  (setq result (/ a b))))(t(setq result nil))))

  ((equal (string currentToken) "OP_SET")(cond((equal (getTokenType (nth 3 tokensEval)) "VALUE")
  (setq c (list (list (getTokenName (nth 2 tokensEval)) 
  (getTokenName (nth 3 tokensEval)))))(setq variables (append variables c))(setq result (parse-integer(getTokenName (nth 3 tokensEval)))))
  ((equal (getTokenType (nth 3 tokensEval)) "ID")(cond((or(equal (getFromVariables (getTokenName (nth 3 tokensEval))) t)
  (equal (getFromVariables (getTokenName (nth 3 tokensEval))) -3.1))
  (setq c (list (list (getTokenName (nth 2 tokensEval)) 
  (getFromVariables (getTokenName (nth 3 tokensEval))))))(setq variables (append variables c))(setq result (getFromVariables 
  (getTokenName (nth 3 tokensEval)))))((not (equal (getFromVariables (getTokenName (nth 3 tokensEval))) nil))
  (setq c (list (list (getTokenName (nth 2 tokensEval)) (getFromVariables (getTokenName (nth 3 tokensEval))))))
  (setq variables (append variables c))
  (setq result (parse-integer(getFromVariables (getTokenName (nth 3 tokensEval))))))
  (t(print "ERROR")(exit))))((equal 
  (getTokenName (nth 3 tokensEval)) "KW_TRUE")(setq c (list (list (getTokenName (nth 2 tokensEval)) t)))
  (setq variables (append variables c))(setq result t))((equal (getTokenName (nth 3 tokensEval)) "KW_FALSE")
  (setq c (list (list (getTokenName (nth 2 tokensEval)) -3.1)))
  (setq variables (append variables c))(setq result nil))
  ((equal (getTokenName (nth 3 tokensEval)) "KW_NIL")(setq c (list (list (getTokenName (nth 2 tokensEval)) -3.1)))(setq variables (append variables c))
  (setq result nil))((equal (getTokenName (nth 3 tokensEval)) "OP")(setq a (evaluateLine (subseq tokensEval 3 (- len 1))))
  (setq c (list (getTokenName (nth 2 tokensEval)) a))(setq variables (append variables c))(setq result (list (getTokenName (nth 2 tokensEval)) a)))
  ((equal (getTokenName (nth 3 tokensEval)) "'")
  (setq a (evaluateLine (subseq tokensEval 3 (- len 1))))(setq c  (list (getTokenName (nth 2 tokensEval)) a))
  (setq variables (append variables c))(setq result (list (getTokenName (nth 2 tokensEval)) a)))))((equal (string currentToken) "DEFV")(cond((equal 
  (getTokenType (nth 3 tokensEval)) "VALUE")(setq c (list (list (getTokenName (nth 2 tokensEval))
   (getTokenName (nth 3 tokensEval)))))
  (setq variables (append variables c)))((equal (getTokenType (nth 3 tokensEval)) "ID")
  (cond((not (equal (getFromVariables (getTokenName (nth 3 tokensEval))) nil))
  (setq c (list (list (getTokenName (nth 2 tokensEval)) (getFromVariables (getTokenName (nth 3 tokensEval))))))(setq variables (append variables c)))
  (t(print "ERROR"))))((equal (getTokenName (nth 3 tokensEval)) "KW_TRUE")(setq c (list (list (getTokenName (nth 2 tokensEval)) t)))
  (setq variables (append variables c)))((equal (getTokenName (nth 3 tokensEval)) "KW_FALSE")
  (setq c (list (list (getTokenName (nth 2 tokensEval)) -3.1)))
  (setq variables (append variables c)))((equal (getTokenName (nth 3 tokensEval)) "KW_NIL")(setq c (list (list (getTokenName (nth 2 tokensEval)) -3.1)))
  (setq variables (append variables c)))((equal (getTokenName (nth 3 tokensEval)) "OP")
  (setq a (evaluateLine (subseq tokensEval 3 (- len 1))))
  (setq c (list (getTokenName (nth 2 tokensEval)) a))(setq variables (append variables c))
  (setq result (list (getTokenName (nth 2 tokensEval)) a)))
  ((equal (getTokenName (nth 3 tokensEval)) "'")(setq a (evaluateLine (subseq tokensEval 3 (- len 1))))
  (setq c  (list (getTokenName (nth 2 tokensEval)) a))(setq variables (append variables c))
  (setq result (list (getTokenName (nth 2 tokensEval)) a)))))
  ((or (equal (string currentToken) "OP_AND") (equal (string currentToken) "OP_OR") 
  (equal (string currentToken) "OP_EQ") (equal (string currentToken) "OP_GT"))
  (cond((equal (string (getTokenName (nth 2 tokensEval))) "KW_TRUE")(setq a t) (setq c 3))
  ((equal (string (getTokenName (nth 2 tokensEval))) "KW_FALSE") 
  (setq a nil) (setq c 3))((equal (string (getTokenName (nth 2 tokensEval))) "KW_NIL") 
  (setq a nil) (setq c 3))((equal (string (getTokenType (nth 2 tokensEval))) "ID") 
  (setq a (getFromVariables (getTokenName (nth 2 tokensEval)))) 
  (cond((equal a -3.1)(setq a nil))((equal a t)(setq a t))(t(print "Variable has no value")(exit)))
  (setq c 3))((equal (string (getTokenType (nth 2 tokensEval))) "VALUE") (setq a (parse-integer (getTokenName (nth 2 tokensEval)))) (setq c 3))
  (t(setq e (countParanthesis (subseq tokensEval 2)))(if (equal e nil) (progn (setq result nil) (setq c nil)) (setq c (+ c e)))
  (if (and (not (equal c nil)) (< c len)) (progn (setq a (evaluateLine (subseq tokensEval 2 c))) (setq d (+ c 2))) (setq result nil))))
  (cond((equal(string (getTokenName (nth c tokensEval))) "KW_TRUE")
  (setq b t))((equal(string (getTokenName (nth c tokensEval))) "KW_FALSE")(setq b nil))
  ((equal(string (getTokenName (nth c tokensEval))) "KW_NIL")
  (setq b nil))((equal(string (getTokenType (nth c tokensEval))) "ID")
  (setq b (getFromVariables (getTokenName (nth c tokensEval))))(cond((equal b -3.1)
  (setq b nil))((equal b t)(setq b t))(t(print "Variable has no value")(exit) )))
  ((equal (string (getTokenType (nth c tokensEval))) "VALUE")(setq b (parse-integer (getTokenName (nth c tokensEval)))) (setq c 3)) 
  (t(setq e (countParanthesis (subseq tokensEval c)))(if (equal e nil) (progn (setq result nil) (setq c nil)) (setq c (+ c e)))
  (if (and (not (equal c nil)) (< c len)) (progn (setq b (evaluateLine (subseq tokensEval c d)))) 
  (setq result nil))))
  (cond((and (not (equal result nil)))(if (equal (string currentToken) "OP_AND") (setq result (and a b)))
  (if (equal (string currentToken) "OP_OR")
  (setq result (or a b)))(if (equal (string currentToken) "OP_EQ") (setq result (equal a b)))(if (equal (string currentToken) "OP_GT")
  (setq result (> a b))))(t(setq result nil))))((equal (string currentToken) "OP_NOT")
  (cond((or (equal (getTokenName (nth 2 tokensEval)) "KW_TRUE") (equal (getTokenName (nth 2 tokensEval)) "KW_NIL") 
  (equal (getTokenName (nth 2 tokensEval)) "KW_FALSE")
  (equal (getTokenType (nth 2 tokensEval)) "ID"))
  (cond((equal (string (getTokenName (nth 2 tokensEval))) "KW_TRUE")(setq a t) (setq c 3))
  ((equal (string (getTokenName (nth 2 tokensEval))) "KW_FALSE") 
  (setq a nil) (setq c 3))((equal (string (getTokenName (nth 2 tokensEval))) "KW_NIL")(setq a nil) (setq c 3))
  ((equal (string (getTokenType (nth 2 tokensEval))) "ID")
  (setq a (getFromVariables (getTokenName (nth 2 tokensEval)))) (setq c 3))))
  (t(setq e (countParanthesis (subseq tokensEval 2)))(if (equal e nil) 
  (progn (setq result nil) (setq c nil)) (setq c (+ c e)))
  (if(and (not (equal c nil)) (< c len)) (progn (setq a (evaluateLine (subseq tokensEval 2 c)))
  (setq d (+ c 2))) (setq result nil))))
  (cond((not(equal result nil))(setq result (not a)))(t(setq result nil))))

  ((and (equal (string currentToken) "KW_CONCAT") (or (equal (string (getTokenName (nth 2 tokensEval))) "'")  
  (equal (string (getTokenName (nth 3 tokensEval))) "KW_LIST")))(setq e (countParanthesis (subseq tokensEval 2)))
  (if (equal e nil) (progn (setq result nil) (setq c nil)) (setq c (+ c e)))(cond ((equal c nil) (setq result nil) (setq d (+ c 2))))
  (cond ((and (not (equal c nil)) (< c len)) (setq a (evaluateLine (subseq tokensEval 2 c)))
   (setq result nil)))(if (and (not (equal c nil)) (< c len))
  (progn(setq e (countParanthesis (subseq tokensEval c)))(if (equal e nil) (progn (setq result nil) 
  (setq d nil)) (setq d (+ c e))))(setq d nil))
  (if (and (not (equal d nil)) (< d len)) (progn (setq b (evaluateLine (subseq tokensEval c d))) (setq d (+ d 1))) (setq result nil))
  (if (and  (= d len) (not (equal a nil)) (not (equal b nil)))
  (progn(setq result (list))(setq result (append result a))(setq result (append result b)))(setq result nil)))
  ((equal (string currentToken) "KW_APPEND")
  (cond((equal (string (getTokenType (nth 2 tokensEval))) "VALUE")
  (setq a (list(parse-integer (getTokenName (nth 2 tokensEval))))) (setq c 3))
  ((string= (getTokenType (nth 2 tokensEval)) "ID")(setq a (list (parse-integer(getFromVariables (getTokenName (nth 2 tokensEval)))))) (setq c 3))
  (t(setq e (countParanthesis (subseq tokensEval 2)))(if (equal e nil) (progn (setq result nil) (setq c nil)) (setq c (+ c e)))
  (if (and (not (equal c nil)) (< c len)) (setq a (evaluateLine (subseq tokensEval 2 c))) (setq result nil))))
  (if (equal c nil) (setq result nil) (setq d (+ c 2)))
  (cond((and (not (equal c nil)) (< c len))(setq e (countParanthesis (subseq tokensEval c)))(if (equal e nil) (progn (setq result nil) (setq d nil))
  (setq d (+ c e))))(t(setq d nil)))
  (if (and (not (equal d nil)) (< d len)) (progn (setq b (evaluateLine (subseq tokensEval c d) )) (setq d (+ d 1))) (setq result nil))
  (if (and (not (equal result nil)) (= d len) (not (equal a nil)) (not (equal b nil)))
  (progn(setq result nil)(setq result (append result a))
  (setq result (append result b)))(setq result nil)))
  ((equal (string (getTokenName (nth 1 tokensEval))) "KW_LIST")(setq a (scanList (subseq tokensEval 1)))
  (setq result a))
  ((string= (getTokenName (nth 0 tokensEval)) "'")(setq a (scanList2 (subseq tokensEval 1)))(setq result a)))(return-from evaluateLine result)))

(defun getTokenType (tokens)(car tokens))

(defun getTokenName (tokens)(cadr tokens))

(defun scanList (tokensEval)(let ((len (list-length tokensEval)) (tempList (list))(x)(currentToken) (result 1) )(if (> len 2)
  (progn(loop for i from 1 to (- len 2) do(progn(setq currentToken (getTokenType (nth i tokensEval)))(if (string= currentToken "VALUE")
  (setq x (list (parse-integer (getTokenName (nth i tokensEval))))))(if (not (equal result nil)) (setq tempList (append tempList x)))))
  (if (not (equal result nil)) (setq result tempList)))(setq result nil))result))

(defun scanList2 (tokensEval)(let ((len (list-length tokensEval)) (tempList (list))(x)(currentToken) (result 1) )(if (> len 2)(progn(loop for i from 1 to (- len 2)
  do(progn(setq currentToken (getTokenType (nth i tokensEval)))(if (string= currentToken "VALUE")(setq x (list (parse-integer (getTokenName (nth i tokensEval))))))
  (if (not (equal result nil)) (setq tempList (append tempList x)))))(if (not (equal result nil)) (setq result tempList)))(setq result nil))result))

(defun countParanthesis (x)
  (let ((counter 0) (str) (j 0) (result nil)) (setq apFound nil) (if (or (string= (getTokenName (nth 0 x)) "OP") (string= (getTokenName (nth 0 x)) "'") )
    (progn(loop for i in x  do (progn (setq str (getTokenName i))(cond((equal t apFound)(setq apFound nil))((string= str "OP")(setq counter (+ counter 1)))
    ((string= str "CP")(setq counter (- counter 1)))((string= str "'")(setq counter (+ counter 1))(setq apFound t)))(setq j (+ j 1))(if (= counter 0) (return j))))
    (setq result j)))result))

(defvar i)

(defun getFromVariables (id)
    (setq i 0)(dotimes (h (length variables))(cond((equal (getTokenType (nth (-(- (length variables) 1) i) variables)) id)
          (return-from getFromVariables (getTokenName(nth (-(- (length variables) 1) i) variables)))))(setq i (+ i 1)))(return-from getFromVariables nil))

(defun getFromFunctions (id)
  (setq i 0)(dotimes (h (length functions))(cond((equal (string(nth i functions)) id)(return-from getFromFunctions t)))(setq i (+ i 1)))
  (return-from getFromFunctions nil))

(defun gppinterpreter (&optional input)
 (cond( (equal input nil)(format t "~d ~%" "first part " "~d ~%")
  (dotimes (x 1000)   (setq stringLine nil)(format t "~%~d ~%" "type read for read text " "~d ~%")(setq stringLine (read-line))(cond((not(equal (string stringLine) "read"))
  (setq breakLoop nil)(setq subStr nil)(setq isNumber nil)(setq isItIdentifier nil)(setq temp3 nil)(setq previousCharacter nil)(setq classPrevious nil)
  (setq classCurrent nil)(setq starFoundDouble nil)(setq foundedDOt nil)(setq done nil)(setq counter 0)(gppLex stringLine)(setf tokenList tokens)

  (setq isCorrect t)(interpreter tokens)(setq tt (evaluateLine))(format T "syntax= ok  ")(cond((not(equal tt 0)) (format T "result= ") (print tt)))
  (setq tokens nil))
  (t (setq x 1001)))))(t(setq breakLoop nil)(setq subStr nil)(setq isNumber nil)(setq isItIdentifier nil)(setq temp3 nil)(setq previousCharacter nil)
  (setq classPrevious nil)(setq classCurrent nil)(setq starFoundDouble nil)(setq foundedDOt nil)(setq done nil)(setq counter 0)(setq tokens nil)
  (format t "~d ~%" "SECOND PART (INPUT COMES FROM FILE)" "~d ~%")(let ((x (open input)))(when x(loop for line = (read-line x nil)
  while line
  do(gppLex line)do  (setf tokenList tokens) do  (setq isCorrect t) do  (interpreter tokens)do  (setq tt (evaluateLine)) do  (format T "SYNTAX: OK  ") 
  (format T line) (terpri) do  (cond((not(equal tt 0)) (format T "Result: ") (print tt) (terpri)))
    do(setq breakLoop nil) do(setq subStr nil) do(setq isNumber nil) do(setq isItIdentifier nil) do(setq temp3 nil)
    do(setq previousCharacter nil) do(setq classPrevious nil) do(setq classCurrent nil) do(setq starFoundDouble nil)
    do(setq foundedDOt nil) do(setq done nil) do(setq counter 0)) (close x))))))(gppinterpreter)(format t "~d ~%" "TYPE fileName: ")(setq stringLine nil)
    
    (setq stringLine (read-line))
    (gppinterpreter stringLine)        
