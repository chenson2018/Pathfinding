import Pathfinding
open Function Prod

def weigthed_successors (n : Fin 9) : List (Fin 9 × Nat) :=
  match n with
  | 0 => [(1, 7), (2, 7), (3, 6)]
  | 1 => [(0, 8), (6, 7)]
  | 2 => [(5, 7)]
  | 3 => [(7, 7)]
  | 4 => [(4, 2)]
  | 5 => [(1, 1)]
  | 6 => [(2, 5), (4, 5), (5, 2)]
  | 7 => [(5, 8)]
  | 8 => []

-- bfs loops
-- TODO: prevent getting stuck in an unrelated loop
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 0
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 1
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 2
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 3
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 4
#eval bfs_loop (List.map Prod.fst ∘ weigthed_successors) 8
  
#eval dijkstra weigthed_successors (·==0) 1
#eval dijkstra weigthed_successors (·==1) 1
#eval dijkstra weigthed_successors (·==2) 1
#eval dijkstra weigthed_successors (·==3) 1
#eval dijkstra weigthed_successors (·==4) 1
#eval dijkstra weigthed_successors (·==5) 1
#eval dijkstra weigthed_successors (·==6) 1
#eval dijkstra weigthed_successors (·==7) 1
#eval dijkstra weigthed_successors (·==8) 1

#eval dijkstra (λ _ ↦  [(1,1)]) (·==2) 1

def maze :=
"#########
#.#.....#
###.##..#
#...#...#
#...#...#
#...#...#
#...#...#
#########
"

def OPEN := maze |>.splitOn "\n" |>.map (λ l ↦ l.toList.map (·=='.'))

def maze_successors (x y : Nat) := 
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] |>.filterMap
  (λ (nx, ny) ↦ let b := OPEN[ny]![nx]!; if b then some ((nx, ny), 1) else none)

def maze_sol := dijkstra (uncurry maze_successors) (·==(6,3)) (2,3) |>.get!

#eval snd maze_sol
#eval fst maze_sol |>.all (λ (x,y) ↦ OPEN[y]![x]!)
#eval dijkstra (uncurry maze_successors) (·==(1,1)) (2,3)
