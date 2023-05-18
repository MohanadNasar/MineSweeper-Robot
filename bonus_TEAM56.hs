type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show ,Eq)

up:: MyState -> MyState 
up Null = Null 
up  (S (x,y) mines lastA myState) = 	if x==0 then Null
										else (S (x-1,y) mines "up" (S (x,y) mines lastA myState))
																	
down:: MyState -> MyState 
down Null = Null 
down  (S (x,y) mines lastA myState) = 	if x==9 then Null
										else (S (x+1,y) mines "down" (S (x,y) mines lastA myState))								
											
right:: MyState -> MyState 
right Null = Null 
right  (S (x,y) mines lastA myState) = 	if y==9 then Null
										else (S (x,y+1) mines "right" (S (x,y) mines lastA myState))	

left:: MyState -> MyState 
left Null = Null 
left  (S (x,y) mines lastA myState) = 	if y==0 then Null
										else (S (x,y-1) mines "left" (S (x,y) mines lastA myState))


collect:: MyState -> MyState
collect (S (x,y) mines lastA myState) | (collectContains(x,y) mines)==True  = (S (x,y) (collectHelper (x,y) mines) "collect"  (S (x,y) mines lastA myState))
									  | otherwise = Null
											 
collectHelper:: Cell -> [Cell] -> [Cell]
collectHelper _ [] = []
collectHelper (x,y) ((x1,y1):m2) | x==x1 && y==y1 = collectHelper (x,y) m2
								 | otherwise = (x1,y1):collectHelper (x,y) m2
								
collectContains:: Cell -> [Cell] -> Bool 
collectContains _ [] = False
collectContains (x,y) ((x1,y1):m2) | x==x1 && y==y1 = True
								   | otherwise = collectContains (x,y) m2
								 
								 
nextMyStates:: MyState -> [MyState]	
nextMyStates (S (x,y) mines lastA myState) = (nmsUp (S (x,y) mines lastA myState)) ++ 
											(nmsDown(S (x,y) mines lastA myState)) ++ 
											(nmsLeft(S (x,y) mines lastA myState)) ++
											(nmsRight(S (x,y) mines lastA myState)) ++
											(nmsCollect(S (x,y) mines lastA myState))  

nmsUp:: MyState -> [MyState]
nmsUp  (S (x,y) mines lastA myState) = if x==0 then []
									   else [(S (x-1,y) mines "up" (S (x,y) mines lastA myState))]
nmsDown::MyState -> [MyState]
nmsDown  (S (x,y) mines lastA myState) = if x==9 then []
										 else [(S (x+1,y) mines "down" (S (x,y) mines lastA myState))]	
nmsRight::MyState -> [MyState]
nmsRight  (S (x,y) mines lastA myState) = if y==9 then []
										  else [(S (x,y+1) mines "right" (S (x,y) mines lastA myState))]
nmsLeft::MyState -> [MyState]
nmsLeft  (S (x,y) mines lastA myState)= if y==0 then []
										else [(S (x,y-1) mines "left" (S (x,y) mines lastA myState))]					 
											
nmsCollect::  MyState-> [MyState]
nmsCollect (S (x,y) mines lastA myState) | collect (S (x,y) mines lastA myState)/=Null = [collect (S (x,y) mines lastA myState)]
									     | otherwise = []											
											 
isGoal::MyState->Bool
isGoal (S _ mines _ _) | mines == [] = True
									 | otherwise = False
								
search::[MyState]->MyState
search(s1:s) | isGoal(s1) == True = s1
			 | otherwise = search(s ++ (nextMyStates s1))
			 
constructSolution:: MyState ->[String]
constructSolution Null = [] 
constructSolution (S _ _ lastA myState) | (lastA== "") =  constructSolution myState
										| otherwise = constructSolution myState ++ [lastA]
								
solve :: Cell->[Cell]->[String]
solve r [] = [] 
solve r (m1:ms) = constructSolution(solveHelper(S r (m1:ms) "" Null))  							
									 
solveHelper(S (x,y) [] lastA myState) = S (x,y) [] lastA myState

solveHelper(S (x,y) ((x1,y1):m2) lastA myState)   | x==x1 && y==y1 = solveHelper(collect(S (x,y) ((x1,y1):m2) lastA myState))  											 
												  |	x>x1           = solveHelper(up(S (x,y) ((x1,y1):m2) lastA myState))
												  | x<x1           = solveHelper(down(S (x,y) ((x1,y1):m2) lastA myState))
												  | y>y1           = solveHelper(left(S (x,y) ((x1,y1):m2) lastA myState))
												  | y<y1           = solveHelper(right(S (x,y) ((x1,y1):m2) lastA myState))