/--
  **FILE** `Picture/Basic.lean`
  **PURPOSE** Compute the string to chuck into Desmos
-/

/- SECTION: Enumeration/Ranges -/

partial def range (start step stop : Float) : List Float :=
  if start > stop
  then []
  else start :: range (start + step) step stop



/- SECTION: List monad (why isn't this used by default??) -/

instance : Monad List.{u} where
  pure := .singleton
  bind := .flatMap



/- SECTION: Read a simple float -/

def Int.toFloat (x : Int) : Float :=
  if x >= 0
  then x.toNat.toFloat
  else - (-x).toNat.toFloat

instance : HPow Float Nat Float where
  hPow := go
    where
      go (x : Float) : Nat → Float
        | .zero => 1
        | (.succ n) => x * go x n

def String.toFloat? (xs : String) : Option Float :=
  match xs.splitOn "." with
  | [] => -- Too few `"."`s
    none
  | (_ :: _ :: _ :: _) => -- Too many `"."`s
    none
  | [big, small] => -- Just right
    (fun sign intBeforePoint natAfterPoint numberOfDecimalPlaces =>
      sign * (intBeforePoint + natAfterPoint / 10^numberOfDecimalPlaces))
      -- e.g. `-123.00456` goes to `-123 + 456 / (10^2)`
    <$> (big.toList.head? |>.map fun c => if c == '-' then -1 else 1)
    <*> (big.toInt? |>.map Int.toFloat |>.map Float.abs)
    <*> small.toNat?.map Nat.toFloat
    <*> some small.length
  | [_] => -- Could be e.g. `"13"`, which should be transformed `"13" -> (13 : Float)`
    xs.toInt?.map Int.toFloat



/- SECTION: Compute the transformed grid -/

partial def block (x y : Float) : List String := do
  let options {α : Type} [ToString α] (a : α) :=
    [s!"{a}", s!"{a} + t", s!"-{a} - t"]
  let lhs ← options x
  let rhs ← options y
  pure s!"A({lhs}, {rhs})"

partial def grid (as : List α) : List (α × α) := do
  let x ← as
  let y ← as
  pure (x, y)

partial def transformed (start step stop : Float) : List String := do
  let theGrid := grid (range start step stop)
  let cell ← theGrid
  Function.uncurry block cell



/- SECTION: Compute the original grid -/

/-- Get desmos commands to show an original grid. Range for `t` will need manual configuring. -/
partial def originalGrid (start step stop : Float) : List String :=
  (do
    let x ← range start step stop
    pure s!"({x}, t)")
  ++ (do
    let y ← range start step stop
    pure s!"(t, {y})")



/- LAUNCH: Write out desmos commands to file -/

structure DEBUG where
  debugging : Bool

partial def go (start step stop : Float) : StateT DEBUG IO Unit := do
  if (← getThe DEBUG).debugging then
    IO.println "<!> In debug mode!"
    IO.println ∘ ("<!> " ++ ·) ∘ toString <| range start step stop
    IO.println "<!> End debug mode."
  IO.FS.writeFile "./out/transformed.csv" (transformed start step stop).toString
  IO.FS.writeFile "./out/original.csv" (originalGrid start step stop).toString
