import Pathfinding
open Function Prod

def weighted_successors (n : Fin 9) : List (Fin 9 × Nat) :=
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

def weighted_successors_expected (n : Fin 9) : Option (Array (Fin 9) × Nat) :=
  match n with
  | 0 => some (#[1, 0], 8)
  | 1 => some (#[1], 0)
  | 2 => some (#[1, 6, 2], 12)
  | 3 => some (#[1, 0, 3], 14)
  | 4 => some (#[1, 6, 4], 12)
  | 5 => some (#[1, 6, 5], 9)
  | 6 => some (#[1, 6], 7)
  | 7 => some (#[1, 0, 3, 7], 21)
  | 8 => none

-- bfs loops
-- TODO: prevent getting stuck in an unrelated loop
example : bfs_loop (List.map Prod.fst ∘ weighted_successors) 0 = some #[0, 1, 0]       := by native_decide
example : bfs_loop (List.map Prod.fst ∘ weighted_successors) 1 = some #[1, 0, 1]       := by native_decide
example : bfs_loop (List.map Prod.fst ∘ weighted_successors) 2 = some #[2, 5, 1, 0, 2] := by native_decide
example : bfs_loop (List.map Prod.fst ∘ weighted_successors) 8 = none                  := by native_decide

-- weighted least paths
example (n : Fin 9) : dijkstra weighted_successors (·==n) 1 = weighted_successors_expected n := by native_decide +revert
example : dijkstra (λ _ ↦  [(1,1)]) (·==2) 1 = none := by native_decide

def maze :=
"#########
#.#.....#
###.##..#
#...#...#
#...#...#
#...#...#
#...#...#
#########"

def OPEN := maze |>.splitOn "\n" |>.map (λ l ↦ l.toList.map (·=='.'))

def maze_successors (x y : Nat) := 
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] |>.filterMap
  (λ (nx, ny) ↦ let b := OPEN[ny]![nx]!; if b then some ((nx, ny), 1) else none)

def maze_sol := dijkstra (uncurry maze_successors) (·==(6,3)) (2,3) |>.get!

example : snd maze_sol = 8 := by native_decide
example : (fst maze_sol |>.all (λ (x,y) ↦ OPEN[y]![x]!)) = true := by native_decide
example : dijkstra (uncurry maze_successors) (·==(1,1)) (2,3) = none := by native_decide
