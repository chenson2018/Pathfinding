-- from https://github.com/leanprover/lean4/issues/1996
open Lean in instance : Coe (TSyntax `Lean.Parser.Term.termBeforeDo) Syntax.Term where coe s := ⟨s⟩

macro "while" "let " pat:term " := " val:termBeforeDo " do " body:doSeq : doElem =>
  `(doElem| repeat do if let $pat := $val then $body else break)

macro "while" "let " pat:term " ← " val:termBeforeDo " do " body:doSeq : doElem =>
  `(doElem| while let $pat := ← $val do $body)
