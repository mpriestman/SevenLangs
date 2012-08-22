produced(robyn, body_talk).
produced(lights, siberia).
produced(kesha, animal).
produced(jewel, spirit).
produced(jewel, this_way).

genre(robyn, pop).
genre(lights, pop).
genre(kesha, pop).
genre(jewel, folk).

album_genre(X, Y) :- produced(Z, X), genre(Z, Y).