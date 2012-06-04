fib := method(num,
  if(num == 1 or num == 2, 1, fib(num - 1) + fib(num - 2))
)