OperatorTable addAssignOperator(":", "atPutNumber")

Map atPutNumber := method(
  self atPut(
    call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
    call evalArgAt(1))
)

Builder := Object clone

Builder level := 0

Builder indent := method(
  return(" " repeated(level))
)

Builder curlyBrackets := method(
  r := Map clone
  call message arguments foreach(arg,
    r doMessage(arg)
  )
)

Builder forward := method(
  write(self indent, "<", call message name)
  attr := call message arguments at(0)
  if(attr type == "Map", "Hello!!!" println)
  writeln(">")
  self level = level + 1
  call message arguments foreach(arg,
    content := self doMessage(arg);
    if(content type == "Sequence", writeln(self indent, content)))
  self level = level - 1
  writeln(self indent, "</", call message name, ">")
)

Builder ul({"style": "hello"}
  li("Io")
  li("Lua")
  li("Javascript")
)