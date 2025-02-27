import Picture

/- LAUNCH: `main` -/

def main' : StateT DEBUG IO Unit := do
  -- Setup
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let getFloat? : StateT DEBUG IO (Option Float) := do
    let input ← String.trim <$> stdin.getLine
    let converted := String.toFloat? input
    if (←get).debugging then
      stdout.putStrLn s!"<!> input = {input}<!>"
      stdout.putStrLn s!"<!> String.toFloat? input = {converted}"
    pure converted
    -- let input ← stdin.getLine
    -- let input := input.trim
    -- if (←get).debugging then
    --   stdout.putStrLn s!"<!> input = {input}<!>"
    --   stdout.putStrLn s!"<!> String.toNat? input = {String.toNat? input}"
    --   stdout.putStrLn s!"<!> Option.map Nat.toFloat ∘ String.toNat? <| input = {Option.map Nat.toFloat ∘ String.toNat? <| input}"
    -- pure ∘ .map Nat.toFloat ∘ String.toNat? <| input
    -- DEBUG: stdin.getLine |>.map String.toNat? |>.map (.map Nat.toFloat)
  let reportBadInput : IO Unit :=
    stdout.putStrLn "You've gotta give actual floats, mate"
  -- Grab bounds of grid to be transformed from the user
  stdout.putStr "Give lower bound: "
  let start? : Option Float ← getFloat?
  stdout.putStr "Give step size:   "
  let step?  : Option Float ← getFloat?
  stdout.putStr "Give upper bound: "
  let stop?  : Option Float ← getFloat?
  -- Transform the grid, and output Desmos commands
  match (·, ·, ·) <$> start? <*> step? <*> stop? with
  | none =>
    reportBadInput
    if (←get).debugging then
      stdout.putStrLn s!"<!> start? = {start?}"
      stdout.putStrLn s!"<!> step?  = {step?}"
      stdout.putStrLn s!"<!> stop?  = {stop?}"
  | some (start, step, stop) =>
    go start step stop

/-- Entry point for the program. -/
def main : IO Unit :=
  main'.run' { debugging := true }

#eval "0001".takeWhile (· == '0') |>.length
