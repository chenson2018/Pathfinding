import Pathfinding.IndexMap
import Batteries.Data.BinaryHeap
open Batteries

structure SmallestHolder (K : Type) where
  cost : K
  index : Nat

def run_dijkstra
  [BEq α] [Hashable α] [Inhabited α] 
  [LT β] [(x y : β) → Decidable (x < y)] [Zero β] [Inhabited β] [HAdd β β β]
  (successors : α → List (α × β)) (success : α → Bool) (start : α) : IndexMap α (Nat × β) × Option Nat := Id.run do
    let mut to_see : BinaryHeap (SmallestHolder β) (λ s1 s2 ↦ s1.cost < s2.cost) := BinaryHeap.empty _
    to_see := to_see.insert {cost := Zero.zero, index := 0}

    let mut parents : IndexMap α (Nat × β) := IndexMap.empty
    parents := parents.insert start (UInt64.size,Zero.zero)
    let mut target_reached : Option Nat := none

    while let some {cost,index} := to_see.max do
      to_see := to_see.popMax
      let (node,_) := parents.get_index index |>.get!
      if success node then 
        target_reached := some index 
      else
        for (successor, move_cost) in successors node do
          let new_cost := cost + move_cost
          let (n,new_best?) := 
            match parents.get successor with
            | none => (parents.indices.size,true)
            | some (_,cost') => (index, cost' > new_cost)
          if new_best? then 
            parents := parents.insert successor (index,new_cost)
            to_see := to_see.insert {cost := new_cost, index := n}

    (parents,target_reached)

def dijkstra
  [BEq α] [Hashable α] [Inhabited α]
  [LT β] [(x y : β) → Decidable (x < y)] [Zero β] [Inhabited β] [HAdd β β β]
  (successors : α → List (α × β)) (success : α → Bool) (start : α) :=
  let (parents,reached) := run_dijkstra successors success start
  reached 
    |>.map (
      λ target ↦ 
      (
        parents.reverse_path Prod.fst target,
        parents.get_index target |>.get! |>.snd |>.snd
      )
    )
