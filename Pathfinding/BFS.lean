import Pathfinding.IndexMap
open Std

def bfs_core 
  [BEq α] [Hashable α] [Inhabited α] 
  (check_first: Bool) (successors : α → List α) (success : α → Bool) (start : α) : Option (Array α) := do

  if success start ∧ check_first then
    some #[start]
  else
    let mut res : Option (Array α) := none
    let mut i := 0
    let mut parents := IndexMap.empty
    parents := parents.insert start UInt64.size

    repeat 
      if let some (node,_) := parents.get_index i then
        let nexts := successors node
        if let some goal := nexts.find? success then
          res := parents.reverse_path id i |>.push goal
          break
        else 
          parents := nexts.foldl (λ m v ↦ m.insert v i) parents
      else
        break 
      i := i + 1

    res

def bfs [BEq α] [Hashable α] [Inhabited α] := @bfs_core α _ _ _ true

def bfs_loop [BEq α] [Hashable α] [Inhabited α] (successors : α → List α) (start : α) := 
  bfs_core false successors (·==start) start

section testing

def succ (n : Fin 9) : List (Fin 9) :=
  match n with
  | 0 => [1,2,3]
  | 1 => [0,6]
  | 2 => [5]
  | 3 => [7]
  | 4 => [4]
  | 5 => [1]
  | 6 => [2,4,5]
  | 7 => [5]
  | 8 => []

#eval bfs_loop succ 0
#eval bfs_loop succ 1
#eval bfs_loop succ 2
#eval bfs_loop succ 8

end testing
