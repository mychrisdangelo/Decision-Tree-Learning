;;; Chris D'Angelo
;;; cd2665@columbia.edu
;;; May 4, 2013
;;; AI - Spring 2013 - Professor Stolfo
;;;
;;; Decision Tree Learning - Project 4
;;;
;;; Sources:
;;;		*	Section 18.3 of the textbook AIMA by Russell/Norvig to 
;;;			understand the concept
;;; 	*	ID3 algorithm posted here as guide: 
;;;			http://www.cs.columbia.edu/~sal/notes/Mitchells-ID3.pdf
;;;		*	Invaluable book: Touretzky's Common LISP
;;;			"A Gentle Intorudction to Symbolic Computation"
;;;		*	AIMA code was used as a reference:
;;;			http://aima.cs.berkeley.edu/lisp/doc/overview-LEARNING.html
;;;		* 	best-choice function is a translation of the formula
;;;			using entropy on page 704 of AIMA


(defvar *training-data* nil)
(defvar *attribute-names* nil)

;;;
;;;	Read in Data
;;;

(defun print-table ()
	(format t "~A" *attribute-names*)
	(dolist (row *training-data*)
		(format t "~&~A" row)))

(defun prompt-for (prompt-string)
	(format t "~A" prompt-string)
	(read))

(defun get-the-attributes ()
	"requires *training-data* is already in place. function asks the user
	for the headers to the data."
	(let ((add-names (y-or-n-p "Add attribute names? ")))
		(when add-names
			(format t "Please enter the attribute for these examples ")
			(format t "you've loaded.")
			(format t "~&A few rules: ") 
			(format t "~& * Please don't use spaces in the names.")
			(format t "~& * The last column is assumed to be the goal column.")
			(format t "~& * The first column is named example. Please don't")
			(format t "~&   use the same name twice.")
			(format t "~%~%"))
		; first column name myself
		(setf *attribute-names* nil)
		(setf *attribute-names*
			(cons 'example *attribute-names*))
		(let (tmp-string)
			; skip first column
			(dotimes (column (1- (length (first *training-data*))))
				(setf tmp-string 
					(format nil "Attribute Name Column ~A: " (+ 2 column)))
				(if add-names
					(setf *attribute-names*
						(cons (prompt-for tmp-string) 
							*attribute-names*))
					; else
					(setf *attribute-names*
						(cons
							(intern
								(format nil "~A~A" 
									'attribute 
									(+ 1 column)))
							*attribute-names*))))))
		(setf *attribute-names* (reverse *attribute-names*))
		(print-table))

(defun get-training-data (filename)
	"reads filename (filename with full path name) reads a lisp style list of 
	data with headers missing. Style of:
	(sunny hot high ... )
	(overcast hot normal ...)"
	(let ((i 0))
		(setf *training-data* 
			(with-open-file (stream filename)
			(loop for line = (read stream nil)
				while line
				collect (cons
							; intern will convert string to symbol
							(intern 
								(format nil "~A~A" 'example (incf i)))
							line))))))

(defun load-file (&optional filename)
	(cond ((null filename)
			(get-training-data "/Users/chrisdangelo/Desktop/restuarantlsp.txt")
			(get-the-attributes))
		(t (get-training-data filename)
			(get-the-attributes))))

;;;
;;; Functions to speed-up testing for Chris
;;;

(defun test ()
	(load-file)
	(functional-id3 'attribute11))

(defun loadme ()
	(load (compile-file "/Users/chrisdangelo/Desktop/cd2665-hw4.lisp")))

;;;
;;; Utility Functions
;;;

(defun fetch-attribute-column (target-attribute-name)
	"get the column number of a particular attribute "
	(let ((attribute-names (just-attribute-names)) (i 0))
		(loop for each-attribute-name in attribute-names do
			(if (equal each-attribute-name target-attribute-name)
				(return-from fetch-attribute-column i)
				(incf i)))))

(defun get-attribute-value (example-name attribute-name)
	"return example's attribute value"
	(nth (fetch-attribute-column attribute-name) (rest (assoc example-name *training-data*))))

(defun goal-state-of-example (example)
	(last example))

(defun name-of-example (example)
	(first example))

(defun just-attribute-names ()
	(rest *attribute-names*))

(defun get-target-attribute-states (target-attribute examples)
	"return a set of states available in examples for an attribute"
	(let ((result nil))
		(loop for each-example in examples do
			(pushnew (get-attribute-value (name-of-example each-example) target-attribute) 
				result))
		(return-from get-target-attribute-states result)))

;;;
;;; Helper functions for id3
;;;

(defun get-most-common-target-value (target-attribute examples)
	"counts all the examples under each value for an attribute. returns the 
	value-class-label of class with the most examples"
	(let ((max-count 0) (max-attribute-value nil))
		(loop for each-value-set in (rest (split-by-attribute target-attribute examples)) do
			(cond ((> (length (rest each-value-set)) max-count)
				(setf max-count (length (rest each-value-set)))
				(setf max-attribute-value (first each-value-set)))))
		max-attribute-value))

(defun split-by-attribute (attribute-name examples)
	"sub-categorize attributes by attribute-name. breaking on attribute value.
	modeled after AIMA figure 18.4 Example:
	(TYPE? 
		(ITALIAN 
			(EXAMPLE6 NO YES NO YES SOME $$ YES YES ITALIAN 0-10 YES) 
			(EXAMPLE10 YES YES YES YES FULL $$$ NO YES ITALIAN 10-30 NO)) 
		(BURGER 
			(EXAMPLE3 NO YES NO NO SOME $ NO NO BURGER 0-10 YES) 
			(EXAMPLE7 NO YES NO NO NONE $ YES NO BURGER 0-10 NO) 
			(EXAMPLE9 NO YES YES NO FULL $ YES NO BURGER >60 NO) 
			(EXAMPLE12 YES YES YES YES FULL $ NO NO BURGER 30-60 YES)) 
		(THAI 
			(EXAMPLE2 YES NO NO YES FULL $ NO NO THAI 30-60 NO) 
			(EXAMPLE4 YES NO YES YES FULL $ NO NO THAI 10-30 YES) 
			(EXAMPLE8 NO NO NO YES SOME $$ YES YES THAI 0-10 YES) 
			(EXAMPLE11 NO NO NO NO NONE $ NO NO THAI 0-10 NO)) 
		(FRENCH 
			(EXAMPLE1 YES NO NO YES SOME $$$ NO YES FRENCH 0-10 YES) 
			(EXAMPLE5 YES NO YES NO FULL $$$ NO YES FRENCH >60 NO)))"
	(let ((categorized-values nil) (current-value nil) (current-split nil))
		(loop for each-example in examples do
			(setf current-value (get-attribute-value (name-of-example each-example) 
									attribute-name))
			(setf current-split (assoc current-value categorized-values))
			(cond (current-split
					(setf current-split (append current-split (list each-example)))
					(rplacd (assoc current-value categorized-values) (rest current-split)))
				; otherwise insert for first time
				(t (setf categorized-values (cons (list current-value each-example) 
												categorized-values)))))
		(cons attribute-name categorized-values)))

;;;
;;; Best Choice Function. Heuristice based on Entropy
;;;

(defun entropy (numerator-denominator-list)
	"classic entropy formula = sum (P_k * log_2 (P_k))"
	(* -1 (reduce #'+ (mapcar #'(lambda (nd) 
									(* (/ (first nd) (second nd))
										(log (/ (first nd) (second nd)) 2)))
								numerator-denominator-list))))

(defun form-numerator-denominator-lst (attribute-name examples)
	"numerator = # of positives, denominator = # of possibilities. In this
	multi-class system however numerator is not strictly a postive but in
	reality is just he current goal-value we're presently testing for"
	(loop for each-value in (rest (split-by-attribute attribute-name examples)) collect
		(list (length (rest each-value)) (length examples))))

(defun filter-to-attribute-value (attribute-name attribute-value examples)
	"filters to a single attribute value so that split-by-attribute can be
	used conveniently for each attribute value and ultimately for the
	particular value's entropy calculation. in other words a second level of
	sub-categorization"
	(remove-if-not #'(lambda (example)
						(equal (get-attribute-value
									(name-of-example example) 
									attribute-name)
							attribute-value))
					examples))

(defun entropy-system (goal-attribute examples)
	"calculates the entropy of the entire system given a goal attribute"
	(entropy (form-numerator-denominator-lst goal-attribute examples)))

(defun entropy-attribute (test-attribute goal-attribute examples)
	"entropy calculation for a single attribute for a target-attribute"
	(let ((sorted-data (split-by-attribute test-attribute examples)))
		(reduce #'+ (mapcar #'(lambda (value)
						; portion value / total examples
						(* (/ (length (rest value)) (length examples))
							; * entropy of that value
							(entropy
								(form-numerator-denominator-lst 
									goal-attribute
									; we're taking the entropy of
									; one value at a time
									(filter-to-attribute-value
										(first sorted-data)
										(first value)
										examples)))))
						(rest sorted-data)))))

(defun information-gain (test-attribute goal-attribute examples)
	"information-gain per attribute"
	(- (entropy-system goal-attribute examples) 
		(entropy-attribute test-attribute goal-attribute examples)))

(defun information-gain-lst (goal-attribute examples attributes)
	"returns assoc list of attributenames and their information gain value
	gain. Example: ((ATTRIBUTE1 0.0) (ATTRIBUTE2 0.0) (ATTRIBUTE3 0.02072084) 
	(ATTRIBUTE4 0.19570965) (ATTRIBUTE5 0.54085207) (ATTRIBUTE6 0.19570959) 
	(ATTRIBUTE7 0.0) (ATTRIBUTE8 0.02072084) (ATTRIBUTE9 0.0) 
	(ATTRIBUTE10 0.20751876))"
	(loop for each-attribute in (remove goal-attribute attributes) collect
		(list each-attribute 
			(information-gain
				each-attribute
				goal-attribute
				examples))))

(defun choose-best-attribute (examples target-attribute attributes)
	"sorts list with the greatest information gain value first. returns
	the name of the attribute with greatest information gain."
	(first (first 
		(sort (information-gain-lst target-attribute examples attributes)
			#'(lambda (lhs rhs) (> (second lhs) (second rhs)))))))

;;;
;;; id3 Algorithm
;;;

(defun id3 (target-attribute)
	(private-id3 *training-data* target-attribute (just-attribute-names)))

(defun private-id3 (examples target-attribute attributes)
	"examples are training examples, target-attribute is the attributes
	whose value is to be predicted by the tree. Attributes is a list of other
	attributes that may be tested by the learned decision tree. Returns
	a decision tree that correctly classifies the given examples in the style of:
	(ATTRIBUTE5 
		(NONE NO) 
			(FULL 
				(ATTRIBUTE4 
					(NO NO) 
					(YES 
						(ATTRIBUTE9 
							(BURGER YES) 
							(ITALIAN NO) 
						(THAI 
							(ATTRIBUTE3 
								(YES YES) 
								(NO NO))))))) 
		(SOME YES))"
	; Create a root node for the tree
	; if all examples are +, or if all examples are - return root
	; however the assignment requests that this program work for multi-class
	; problems. therefore we test if all the remaining examples is all of
	; one goal state and return that state b/c it's the only option
	(let ((target-attribute-states (get-target-attribute-states target-attribute examples))
		(a nil)
		(examples-v-i nil))
		(cond
			((equal (length target-attribute-states) 1)
				(car target-attribute-states))
			; if attributes is empty, return the single-node tree root, with 
			; label = most common value of target attribute in examples
			((null attributes)
				(get-most-common-target-value target-attribute examples))
			; Otherwise begin
			; A <- the attribute from attributes that best classifies examples
			; the decision attribute for root <- A
			; let examples_v_i be the subset of examples that have 
			; value v_i for A
			(t (setf a (choose-best-attribute examples target-attribute attributes))
				(setf examples-v-i (split-by-attribute a examples))
				; else below this new branch add the subtree
				; id3 (examples_v_i, target-attribute, attributes - A)
				(cons a
					; for each possible value v_i of A
					(loop for each-branch in (rest examples-v-i) collect
						; add a new tree branch below root, corresponding to the 
						; test A = v_i
						; id3 (examples_v_i, target-attribute, attributes - A)
						(list (first each-branch)
							(private-id3 (cdr each-branch) target-attribute (remove a attributes)))))))))

;;;
;;; Helper functions for Functional id3
;;;

(defun get-input-value (attribute input)
	(nth (fetch-attribute-column attribute) input))

(defun fetch-branch-goal-value (attribute-value tree)
	(second (assoc attribute-value (rest tree))))

(defun get-next-node (attribute input tree)
	(fetch-branch-goal-value (get-input-value attribute input) tree))

(defun is-leaf? (goal-value)
	(atom goal-value))

(defun root-name (root)
	(first root))

(defun traverse-tree (tree input)
	"read tree attributes sequentially looking up input values to guide path"
	(let ((next-node (get-next-node (first tree) input tree)))
		(if (is-leaf? next-node)
			(return-from traverse-tree next-node)
			(traverse-tree next-node input))))

;;;
;;; Functional id3
;;;

(defun functional-id3 (target-attribute)
	"formats readible lisp tree into eval-able lisp list (driver function)"
	(private-functional-id3 *training-data* target-attribute 
		(just-attribute-names)))

(defun private-functional-id3 (examples target-attribute attributes)
	"formats readible lisp tree into eval-able lisp list"
	(defun decision-tree (input)
		(let ((id3-output (private-id3 examples target-attribute attributes))) 
			(traverse-tree id3-output input))))