curlyBrackets := method(
  l := List clone
  call message arguments foreach(arg,
    l append(arg)
  )
)

women := {
  "Mary",
  "Noleen",
  "Niamh",
  "Shiobhan"
  }
  
women println