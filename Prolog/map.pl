different(red, green).
different(red, blue).
different(green, red).
different(green, blue).
different(blue, red).
different(blue, green).

colouring(A, M, G, T, F) :-
  different(M, T),
  different(M, A),
  different(A, T),
  different(A, M),
  different(A, G),
  different(A, F),
  different(G, F),
  different(G, T).