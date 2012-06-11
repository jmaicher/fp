Functional Programming - Lecture: 2012-06-11
============================================

7. Infinite lists / flows
--------------------------

### 7.8 Infinite flow of prime numbers

- p -> sift -> no multiples of p


### 4.7./7.9. Coin change

([], [5,2,1], 9) = (coins until now, available coins, amount)
-> ([5], [5,2,1], 4) @ ([], [2,1], 9)
-> ...

- nodes of the solution space are representated by the parameters
- results are concatenated (this has to change when we use flows)

Goal: Create a flow which provides solutions to a coin change problem

### 7.10. Recursive coin change with flows

- paramters 1,2 and 3 characterize the nodes of the solution tree
- parameter 4 is a parameter-less function which calculates the rest of the flow
  of solutions after these of the current sub tree (backtracking)
- direct recursion in the the left sub tree
- recursion in the right sub tree via 4th paramter (backtracking) 

programming technique used in this example: programming with continuations.
(describe a calculation and have a paramter which says where the calculation continues)

### 7.11 Coin change as example for DFS in solution trees

- application-specific representation of nodes for the coin change problem:
  (coins, coinvals, amount)
- fun nextCoins to create sub nodes
   
  fun nextCoins (_, _, 0) = []
  |   nextCoins (_, nil, _) = []
  |   nextCoins (paid, c::coinvals, amount) =
        if amount < 0
          then []
          else [(c::paid, c::coinvals, amount - c), (paid, coinvals, amount)]

- fun predCoins which says if we have a solution

  fun predCoins (paid, ...)

Abstractions:
A: flows encapsulate the search in the solution graph
B: application charactericed with next and pred (fun depthFirst)
C: search strategy

Transform into BFS: Use queue instead of stack
