import Pathfinding

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
  
-- TODO: this is wrong...  
#eval dijkstra weigthed_successors (·==0) 1
#eval dijkstra weigthed_successors (·==1) 1
#eval dijkstra weigthed_successors (·==2) 1 -- wrong
#eval dijkstra weigthed_successors (·==3) 1
#eval dijkstra weigthed_successors (·==4) 1
#eval dijkstra weigthed_successors (·==5) 1 -- wrong
#eval dijkstra weigthed_successors (·==6) 1
#eval dijkstra weigthed_successors (·==7) 1
#eval dijkstra weigthed_successors (·==8) 1



