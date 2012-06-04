Number coreDiv := Number getSlot("/")
Number / = method(denom, if(denom == 0, 0, self coreDiv(denom)))

4 / 2 println
4 / 0 println