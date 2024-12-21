import Batteries
import Pathfinding.WhileLet

open Std

structure Bucket (K V : Type) where
  hash : UInt64
  key : K
  value : V
deriving Inhabited, Repr

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

def get_index_of [Hashable K] (map : IndexMap K V) (key : K) : Option Nat := 
  map.indices[Hashable.hash key]?

def get [Inhabited K] [Inhabited V] [Hashable K] (map : IndexMap K V) (key : K) : Option V := 
  map.get_index_of key |>.map (map.entries[·]! |> Bucket.value)

def reverse_path (parents : IndexMap N V) (parent : V → Nat) (start : Nat) : Array N := Id.run do
  let mut res := []
  let mut i := start
  while let some (node,value) := parents.get_index i do
    i := parent value
    res := node :: res
  res.toArray
