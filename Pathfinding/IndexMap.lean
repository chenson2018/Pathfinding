import Batteries

open Std

structure Bucket (K V : Type) where
  hash : UInt64
  key : K
  value : V
deriving Inhabited, Repr

-- hashes are always 

abbrev Indices := HashMap UInt64 Nat
abbrev Entries (K V : Type) := Array (Bucket K V)

structure IndexMap (K V : Type) where
  indices : Indices
  entries : Entries K V
deriving Repr

namespace IndexMap

def empty : IndexMap K V := {indices := HashMap.empty, entries := #[]}

def insert [Inhabited K] [Inhabited V] [Hashable K] (map : IndexMap K V) (key : K) (value : V) : IndexMap K V :=
  let hash := Hashable.hash key
  match map.indices[hash]? with
  | some idx => 
      let current := map.entries[idx]!
      let entries := map.entries.set! idx {current with value}
      {map with entries}
  | none => 
      let idx := map.entries.size
      let indices := map.indices.insert hash idx
      let entries := map.entries.push {hash, key, value}
      {indices, entries}

def get_index (map : IndexMap K V) (index : Nat) : Option (K × V) := do
  let {key,value,..} ← map.entries[index]?
  pure (key,value)

#eval (IndexMap.empty : IndexMap String String) 
  |>.insert "1" "one"
  |>.insert "1" "fake"
  |>.insert "2" "two"

/-
def Bucket.refs (b : Bucket K V) := (b.key,b.value)


structure IndexMap (K V : Type) where
  entries : Entries K V

def IndexMap.get_index (im : IndexMap K V) (index : Nat) : Option (K × V) :=
  Bucket.refs <$> im.entries[index]?


def IndexMap.insert_full (im : IndexMap K V) (hash : Nat) (key : K) (value : V) :=
  1

#check (#[{hash := 0, key := "key", value := "value"}] : Entries String String)
-/

/-
def f (x : Nat) :=
  match x with
  | 1 => [2,3]
  | 3 => []
  | 2 => [4]
  | _ => []

partial def bfs (step : α → List α) (start : α) : List (List α) := go [(start,[start])] |>.map List.reverse where
  go (xs : List (α × List α)) := 
    match xs with
    | [] => []
    | (s,path) :: tl =>  
        let x := step s |>.map (λ x ↦ (x,x :: path))
        path :: go (tl ++ x)


#eval bfs f 1
-/
