List sum_2d := method(
  self flatten sum
)

List myAverage := method(
  return(self sum / self size)
)

Matrix := List clone

Matrix dim := method(x, y,
  for(i, 0, x - 1, self append(List clone setSize(y)))
)

Matrix set := method(x, y, value,
  self at(x) atPut(y, value)
)

Matrix get := method(x, y,
  self at(x) at(y)
)

Matrix width := method(
  self size
)

Matrix height := method(
  if(self width == 0, 0, self at(0) size)
)

Matrix transpose := method(
  trans := Matrix clone
  trans dim(self height, self width)
  for(x, 0, self width - 1,
    for(y, 0, self height - 1,
      trans set(y, x, self get(x, y))
    )
  )
  return (trans)
)

Matrix write := method(filename,
  f := File with(filename)
  f openForUpdating
  f write(self serialized)
  f close
)

Matrix read := method(filename,
  return (self doFile(filename))
)

myList := list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
flatList := list(1, 2, 3, 4, 5, 6, 7, 8, 9)

myList sum_2d println
flatList sum println

flatList average println
flatList myAverage println

mat := Matrix clone
mat dim(3, 2)
mat set(0, 0, 10)
mat set(1, 0, 20)
mat set(2, 0, 30)
mat set(0, 1, 100)
mat set(1, 1, 200)
mat set(2, 1, 300)
mat println
mat get(2, 1) println
mat width println
mat height println

trans := mat transpose
trans println

mat write("matrix.mat")
input := mat read("matrix.mat")

mat println
input println