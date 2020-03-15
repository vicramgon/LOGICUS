var CSF_8Queens = `
---
************************************************************************************************
**                                      N QUEENS PROBLEM                                      **
************************************************************************************************
The problem of the N queens consists in placing N queens on an NxN chessboard so that they cannot attack each other. 
We're going to model 8 queens case. Here, I leave you a solution...	
						 		 
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |     |||||||     |     |     |     |
				|     |     |     |||||||     |     |     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |     |     |     |     |||||||     |
				|     |     |     |     |     |     |||||||     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |||||||     |     |     |     |     |
				|     |     |||||||     |     |     |     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |     |     |     |     |     |||||||
				|     |     |     |     |     |     |     |||||||
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |||||||     |     |     |     |     |     |
				|     |||||||     |     |     |     |     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |     |     |||||||     |     |     |
				|     |     |     |     |||||||     |     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|||||||     |     |     |     |     |     |     |
				|||||||     |     |     |     |     |     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						
				|     |     |     |     |     |||||||     |     |
				|     |     |     |     |     |||||||     |     |
				+++++++++++++++++++++++++++++++++++++++++++++++++						

For modeling it we're going to use pij that represents that one queen is on the chessboard at the
position (i,j) (if it is true).
Rules are exposed below.
---

--- There must be a queen in each row  ---

&_ {I[0:7]}{T}(|_ {J[0:7]}{T}(pIJ));

--- There cannot be more than one queen in the same row. ---

&_ {I[0:7], J[0:7]}{T}(pIJ -> &_ {U[0:7]} {[I!=U]} (¬pUJ));

--- There cannot be more than one queen in the same column. ---

&_ {I[0:7], J[0:7]}{T}(pIJ -> &_ {U[0:7]} {[J!=U]} (¬pIU));

--- There cannot be more than one queen in the same main diagonal. ---

&_ {I[0:7], J[0:7]}{T}(pIJ -> &_ {K[-7:7],U[0:7],V[0:7]} {[K!=0]AND[U=I+K]AND[V=J+K]} (¬pUV));

--- There cannot be more than one queen in the same secondary diagonal. ---


&_ {I[0:7], J[0:7]}{T}(pIJ -> &_ {K[-7:7],U[0:7],V[0:7]} {[K!=0]AND[U=I+K]AND[V=J-K]} (¬pUV));

`


var CSF_Sudoku = `---
************************************************************************************************
**                                         SUDOKU 6X6                                         **
************************************************************************************************

A Sudoku puzzle consists of 81 cells which are divided into nine columns, rows and regions.
The task is now to place the numbers from 1 to 9 into the empty cells in such a way that 
in every row, column and 3×3 region each number appears only once.

We're show a reduced version 6x6:
			
				+++++++++++++  +++++++++++++ 
				| 6 | 4 |   |  | 2 |   |   | 
				+++++++++++++  +++++++++++++ 
				|   | 5 | 1 |  |   |   |   | 
				+++++++++++++  +++++++++++++   

				+++++++++++++  +++++++++++++ 
				| 3 | 6 | 4 |  |   |   | 2 | 
				+++++++++++++  +++++++++++++  
				| 5 |   |   |  | 4 | 3 | 6 | 
				+++++++++++++  +++++++++++++ 

				+++++++++++++  +++++++++++++
				|   |   |   |  | 6 | 4 |   |
				+++++++++++++  +++++++++++++ 
				|   |   | 6 |  |   | 2 | 5 |
				+++++++++++++  +++++++++++++ 

We're going to model this problem as a CSP in wich variable pijk represents if the box in the
position (i,j) is set with the value k (or not).

Following constraints model our problem:

Each box have to been set with a unique value
---

&_{I[1:6],J[1:6]}{T}(|_{K[1:6]}{T}(pIJK));

---
Although this formula is correct:

&_{I[1:6],J[1:6],K[1:6]}{T}(pIJK -> &_{L[1:6]}{[K!=L]}(¬pIJL));


we're going to divide it by rows to reduce de complexity
---

&_{J[1:6],K[1:6]}{T}(p1JK -> &_{L[1:6]}{[K!=L]}(¬p1JL));
&_{J[1:6],K[1:6]}{T}(p2JK -> &_{L[1:6]}{[K!=L]}(¬p2JL));
&_{J[1:6],K[1:6]}{T}(p3JK -> &_{L[1:6]}{[K!=L]}(¬p3JL));
&_{J[1:6],K[1:6]}{T}(p4JK -> &_{L[1:6]}{[K!=L]}(¬p4JL));
&_{J[1:6],K[1:6]}{T}(p5JK -> &_{L[1:6]}{[K!=L]}(¬p5JL));
&_{J[1:6],K[1:6]}{T}(p6JK -> &_{L[1:6]}{[K!=L]}(¬p6JL));

---
In each row all numbers appear
---

&_{K[1:6],I[1:6]}{T}(|_{J[1:6]}{T}(pIJK));

---
In each column all numbers appear
---

&_{K[1:6],J[1:6]}{T}(|_{I[1:6]}{T}(pIJK));

---
In each region all numbers appear
---

&_{K[1:6]}{T}(|_{I[1:2],J[1:3]}{T}(pIJK));
&_{K[1:6]}{T}(|_{I[1:2],J[4:6]}{T}(pIJK));

&_{K[1:6]}{T}(|_{I[3:4],J[1:3]}{T}(pIJK));
&_{K[1:6]}{T}(|_{I[3:4],J[4:6]}{T}(pIJK));

&_{K[1:6]}{T}(|_{I[5:6],J[1:3]}{T}(pIJK));
&_{K[1:6]}{T}(|_{I[5:6],J[4:6]}{T}(pIJK));


---
Fixed boxes
---
p116;p142;
p225;p231;
p313;p326;p362;
p415;p453;p466;
p546;p554;
p636;p652;`;

var CSF_MapColoring = `
---
************************************************************************************************
**                                        MAP COLORING                                        **
************************************************************************************************
The problem is to set a color (of x possible colors) for each region on a map so that two border 
regions do not have the same color. We're going to ilustrate this problem modeling the andalusian
provinces map coloring problem with 3 colors.

							+++++++++++
							+         +        
							+         +++++++++++++
					++++++++++        +           +
			+++++++++        +  CO    +    JA     +
			+       +        +        +           + 
			+       +        +        ++++++++++++++++++++++++
			+  HU   +  SE    ++++++++++             +        +
			+       +        +      +               +        +
			++		+        +      +       GR      +   AL   +
			++	++++++++++      +               +        +
				++  +      +   MA   +               +        +
				++  CA  +++++++++++++++++++++++++++++++++++
					+      +
					++++++++

We're going to represent each province by a number: AL(1),CA(2),CO(3),GR(4),HU(5),JA(6),MA(7),SE(8).
The variable cij represents that the province i is colored with color j.
---

---
(1) All province have to be set with a unique color
---
&_{I[1:8]}{T}(|_{J[1:3]}{T}(cIJ));
&_{I[1:8],J[1:3]}{T}(cIJ -> &_{K[1:3]}{[K!=J]}(¬cIK));

---
(2) Constraints based on the adjacencies

>> ALMERIA
---

&_{J[1:3]}{T}(c1J -> (¬c4J));

---
>> CADIZ
---

&_{J[1:3]}{T}(c2J -> (¬c5J));
&_{J[1:3]}{T}(c2J -> (¬c7J));
&_{J[1:3]}{T}(c2J -> (¬c8J));

---
>> CORDOBA
---

&_{J[1:3]}{T}(c3J -> (¬c4J));
&_{J[1:3]}{T}(c3J -> (¬c6J));
&_{J[1:3]}{T}(c3J -> (¬c7J));
&_{J[1:3]}{T}(c3J -> (¬c8J));

---
>> GRANADA
---

&_{J[1:3]}{T}(c4J -> (¬c1J));
&_{J[1:3]}{T}(c4J -> (¬c3J));
&_{J[1:3]}{T}(c4J -> (¬c6J));
&_{J[1:3]}{T}(c4J -> (¬c7J));

---
>> HUELVA
---

&_{J[1:3]}{T}(c5J -> (¬c2J));
&_{J[1:3]}{T}(c5J -> (¬c8J));

---
>> JAEN
---

&_{J[1:3]}{T}(c6J -> (¬c3J));
&_{J[1:3]}{T}(c6J -> (¬c4J));

---
>> MALAGA
---

&_{J[1:3]}{T}(c7J -> (¬c2J));
&_{J[1:3]}{T}(c7J -> (¬c3J));
&_{J[1:3]}{T}(c7J -> (¬c4J));
&_{J[1:3]}{T}(c7J -> (¬c8J));

---
>> SEVILLA
---


&_{J[1:3]}{T}(c8J -> (¬c2J));
&_{J[1:3]}{T}(c8J -> (¬c3J));
&_{J[1:3]}{T}(c8J -> (¬c5J));
&_{J[1:3]}{T}(c8J -> (¬c7J));

`;

var CSF_SendMoreMoney = `---
************************************************************************************************
**                                     SEND + MORE = MONEY                                    **
************************************************************************************************

This is the best known cryptographic arithmetic problem. of a mathematical equation among unknown 
numbers, whose digits are represented by letters. The goal is to identify the value of each letter.

Lets model the most known problem, that exposes the follow equation:

				  s   e   n   d
				+ m   o   r   e
				·················
				m   o   n   e   y

To give a correct solution implies:
	-> Each letter have assigned a different digit.
	-> m is not numberly null (m is not 0)
	-> The sum is correct

We are going to represent each letter with a number to define de constraints more confortable.

		.................................
		| d | e | m | n | o | r | s | y |
		.................................
		| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
		.................................

So that, if the variable pij is true indicates that the letter represented by number i is set with the value j.
Apart from these variables we're going to consider the variables auk that represent the carries produced in
the sum.

So constraints are 

(1) Each letter has to be set with a unique value
---

&_{I[1:8]}{T}(|_{J[0:9]}{T}(pIJ));
&_{I[1:8],J[0:9]}{T}(pIJ -> &_{K[0:9]}{[K!=J]}(¬pIK));

---
(2) Carries can be 0 or 1
---

&_{A[1:4]}{T}(aA0 <-> ¬aA1);

---
(3) Different letters represents differents digits
---

&_{I[1:8],J[0:9]}{T}(pIJ -> &_{U[1:8]}{[I!=U]}(¬pUJ));

---
(4) Constraints derivated from the equation
---

&_{J[0:9],K[0:9],U[0:9],X[0:1]}{[J+K=10*X+U]}(p1J & p2K -> p8U & a1X));
&_{J[0:9],K[0:9],U[0:9],X[0:1],Y[0:1]}{[J+K+X=10*Y+U]}(p4J & p6K & a1X -> p2U & a2Y));
&_{J[0:9],K[0:9],U[0:9],X[0:1],Y[0:1]}{[J+K+X=10*Y+U]}(p2J & p5K & a2X -> p4U & a3Y));
&_{J[0:9],K[0:9],U[0:9],X[0:1],Y[0:1]}{[J+K+X=10*Y+U]}(p7J & p3K & a3X -> p5U & a4Y));

&_{X[0:1]}{T}(a4X <-> p3X);

---
(5) m is not numberly null (m is not 0) 
---

p31;

`;