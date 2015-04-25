> module LabH2SOLS15
>     where

Part 1 User defined data type
Problem 1:
I will talk about deriving in class.

> data Point a = Pt a a deriving (Show, Eq)

   ...> :t Pt 0 0
   Pt 0 0 :: Num a => Point a
   ...> :t Pt 'a' 'b'
   Pt 'a' 'b' :: Point Char


  Now define a function "isOrigin" that returns true if the point is the origin 
  and false otherwise.  
  
> isOrigin :: (Eq a, Num a) => Point a -> Bool
> isOrigin (Pt 0 0) = True
> isOrigin (Pt a b) = False

    ....> isOrigin (Pt 0 0)
    True  
    ....> isOrigin (Pt 3.0 0.0)
    False     
 

  b) Redefine isOrigin (name it isOriginL) using lambda expression only.  

> isOriginL (Pt a b) = (\x y -> x == 0 && y == 0) a b

Problem 2 [10pts]Write a Haskell function "inside point r " which returns true 
if and only if the point lies inside a circle of radius r and false otherwise.  

 "inside" can be defined as: 
        inside r point (x, y) is true  if and only if x^2 + y^2 < r^2 
where x and y are the coordinates of the point. 

Your Haskell code should not be using tuples. Since contructor for Point is Pt 
to execute you type:

   Lab2> inside 10.0 (Pt 3.0 2.5) 
   True 
   Lab2> inside 10.0 (Pt (-1) (-2)) 
   True  
   Lab2> inside 2.0 (Pt 3.0 2.5) 
   False  

> inside r (Pt x y) = x*x + y*y < r*r

Part 2

Problem 1: Write a recursive definition, mul x y, that returns the product x*y. (Do
not use "*" operator.

> mul 0 _ = 0
> mul x y = y + (mul (x-1) y)

Problem 2: ack

> ack m n
> 	| m == 0			= 2*n
>	| n == 0 && m > 0	= 0
>	| n == 1 && m > 0	= 2
>	| otherwise			= ack (m-1) (ack m (n-1))

Part 3
problem 1:
Below is the code for the first 2 written questions.


> power :: Integer -> Integer -> Integer
> power a 0 = 1
> power a b = a * power a (b-1)

power 2 5
2 * power 2 (5-1)
	2 * power 2 (4-1)
		2 * power 2 (3-1)
			2 * power 2 (2-1)
				2 * power 2 (1-1)
					1
				2
			4
		8
	16
32

The number of expressions evaluated are decided solely by the value of k
therefore, given k as the primary determining factor, complexity is O(n)
  
> powerT :: Integer -> Integer -> Integer
> powerT a b = trp b 1
>      where
>      trp n p = if (n==0) then p else trp (n-1) (a*p)

powerT 2 5 = trp 5 1
	trp 5 1 = trp (5-1) (2*1)
		trp 4 2 = trp (4-1) (2*2)
			trp 3 4 = trp (3-1) (2*4)
				trp 2 8 = trp (2-1) (2*8)
					trp 1 16 = trp (1-1) (2*16)
						trp 0 32 = 32
					32
			32
		32
	32
powerT 2 5 = trp 5 1 = 32

Integer powerT(Integer a, Integer b){
	Integer t1 = 1;
	Integer t2 = b;
	while(true){
		if(t2 == 0){
			return t1;
		}
		else{
			t2--;
			t1 = a*t1;
		}
	}
}

problem 2:
Below is the code for the 2 written question.

> turboPower a 0 = 1
> turboPower a b
>     | even b = turboPower (a*a) (b `div` 2)
>     | otherwise = a * turboPower a (b-1)

Time complexity of O(log n)

  2b:
  Convert the algorithm used in turboPower to tail recursive form. Call the
  definition turboTPower.

> turboTPower a b = if (b==0) then 1 else if (even b) then (turboTPower (a*a) (b `div` 2)) else a * turboTPower a (b-1)

Part 4
 
  Problem 1: 
List comprehension notation is defined as follows

  [<expr> | <generators> ]

where <expr> is an expression,  and <generators> is a generator
and the notation denotes a finite or infinite list.

For example the function, lc, below converts a list of items to a list of tuples of the item
using list comprehension technique.

> lc lst = [(n,n) | n <- lst]


The symbol "<-" reads as "belongs to" "x <- xs"  is called the generator.
The symbol "|" reads as such that 

Define a function add10 that takes a list of numbers and builds a list 
of with 10 added to each number in the origin list.

i.e.
     ... > add10  [1,4,9]
     ...>  [11,14,19]

> add10 lst = [ n+10 | n <- lst]

Problem 2 & 3:

What is the result of the following function applications of the 2 Prelude functions
   drop and take 
(see https://www.haskell.org/onlinereport/prelude-index.html for definition):

   ...> take 3 [0,1,2,3,4,5,6,7,8]
   ...> drop 3 [0,1,2,3,4,5,6,7,8]

Tree below is a data type used to build a binary tree. and tree1 
is an example of a tree.

> data Tree a = Leaf  | Node a (Tree a) (Tree a) deriving Show

> tree1 = Node 5 (Node 10 Leaf (Node 12 Leaf Leaf) ) Leaf

Problem 2: Define toList which changes a tree to a list.  You may use "++". What are 
your assumptions?  Can you predict the most general type of toList?

    ...> toList tree1
    [10,12,5]

Assumptions:
	In-Order Traversal
	Will never be given an empty Tree, assume that Tree will always conform to its definition

> toList (Leaf) = []
> toList (Node a lT rT) = toList lT ++ [a] ++ toList rT

Problem 3:

Define toList which changes a list to a tree.  You may use "drop", "take"
and "length". What are your assumptions?  Can you predict the most general type of toTree?

    ...> fromList [0,1,2,3,4]
    Node 0 (Node 1 Leaf (Node 2 Leaf Leaf)) (Node 3 Leaf (Node 4 Leaf Leaf))

> fromList []		= (Leaf)
> fromList (h:t)
>	| even (length t)	= (Node h (fromList (take ((length t) `div` 2) t)) (fromList (drop ((length t) `div` 2) t)))
>	| otherwise			= (Node h (fromList (take (((length t) - 1) `div` 2) t)) (fromList (drop (((length t) - 1) `div` 2) t)))
