import Pathfinding.IndexMap
open Std

def reverse_path (parents : IndexMap N V) (parent : V → Nat) (start : Nat) : List N := Id.run do
  let mut res := []
  let mut i := start
  repeat 
    if let some (node, value) := parents.get_index i then
      -- TODO this is odd!
      if i = 0 then break
      i := parent value
      res := node :: res
    else
      break
  res.reverse

def bfs [BEq α] [Hashable α] [Inhabited α] (start : α) (successors : α → List α) (success : α → Bool) : Option (List α) := do
  if success start then
    some [start]
  else
    let mut res : Option (List α) := none
    let mut i := 0
    let mut parents := IndexMap.empty
    parents := parents.insert start 0

    repeat 
      if let some (node,_) := parents.get_index i then
        let nexts := successors node
        if let some goal := nexts.find? success then
          res := goal :: reverse_path parents id i
          break
        else 
          parents := nexts.foldl (λ m v ↦ m.insert v i) parents
      else
        break 
      i := i + 1
   
    let res' ← res
    start :: res'.reverse

#eval bfs (0,0) (λ (x,y) ↦ if x > 5 ∨ y > 5 then [] else [(x+2,y), (x+1,y), (x,y+1)]) (·=(0,3))

