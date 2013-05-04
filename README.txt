Chris D'Angelo
cd2665@columbia.edu
May 4, 2013
AI - Spring 2013 - Professor Stolfo

Assumes that user is able to enter in the heading titles (attributes) manually or
heading tites are added automatically. Program assumes that the user will provide a 
database file in plain text form in the style of the enclosed document "restuarantlsp.txt".

Decision Tree Learning - Project 4

1.  Project implemented in Common Lisp.
2.  Programming environment: Sublime Text 2 and emacs+slime+clozure.
3.  To run the program on clic machines I suggest starting clisp in
    in the extracted project directory. Then executing the following
    example commands. The paths are an example on my home computer.

  	(load (compile-file "/Users/chrisdangelo/Desktop/cd2665-hw4.lisp"))

  	; to load the data from file
	(load-file "/Users/chrisdangelo/Desktop/restuarantlsp.txt")
  	
  	; to create a decision-tree function with its goal as attribute11
  	(functional-id3 'attribute11)

  	; using that function to find the result of a new input
  	(eval '(decision-tree '(no yes no no some $ no no burger 0-10 yes)))
  	
  	; to see the lisp list representation of the decision tree issue command...
  	(id3 'attribute11)

4.  Example Output:

	CL-USER> (load-file "/Users/chrisdangelo/Desktop/restuarantlsp.txt")
	Add attribute names?  (y or n)  n
	(EXAMPLE ATTRIBUTE1 ATTRIBUTE2 ATTRIBUTE3 ATTRIBUTE4 ATTRIBUTE5 ATTRIBUTE6 
	ATTRIBUTE7 ATTRIBUTE8 ATTRIBUTE9 ATTRIBUTE10 ATTRIBUTE11)
	(EXAMPLE1 YES NO NO YES SOME $$$ NO YES FRENCH 0-10 YES)
	(EXAMPLE2 YES NO NO YES FULL $ NO NO THAI 30-60 NO)
	(EXAMPLE3 NO YES NO NO SOME $ NO NO BURGER 0-10 YES)
	(EXAMPLE4 YES NO YES YES FULL $ NO NO THAI 10-30 YES)
	(EXAMPLE5 YES NO YES NO FULL $$$ NO YES FRENCH >60 NO)
	(EXAMPLE6 NO YES NO YES SOME $$ YES YES ITALIAN 0-10 YES)
	(EXAMPLE7 NO YES NO NO NONE $ YES NO BURGER 0-10 NO)
	(EXAMPLE8 NO NO NO YES SOME $$ YES YES THAI 0-10 YES)
	(EXAMPLE9 NO YES YES NO FULL $ YES NO BURGER >60 NO)
	(EXAMPLE10 YES YES YES YES FULL $$$ NO YES ITALIAN 10-30 NO)
	(EXAMPLE11 NO NO NO NO NONE $ NO NO THAI 0-10 NO)
	(EXAMPLE12 YES YES YES YES FULL $ NO NO BURGER 30-60 YES)
	NIL
	CL-USER> (functional-id3 'attribute11)
	DECISION-TREE
	CL-USER> (eval '(decision-tree '(no yes no no some $ no no burger 0-10 yes)))
	YES
	CL-USER> (id3 'attribute11)
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
		(SOME YES))