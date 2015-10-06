import Scanner

main :: IO()
main = print $ tokenize "program intDiv(in  const m:int32, in  const n:int32, \
                        \               out const q:int32, out const r:int32) \
                        \  global                                               \
                        \    proc divide(in copy const m:int32, in copy const n:int32, \
                        \                out ref var q:int32, out ref var r:int32) \
                        \    do \
                        \      q init := 0; \
                        \      r init := m; \
                        \      while r >= n do \
                        \        q := q + 1; \
                        \        r := r - n \
                        \      endwhile \
                        \   endproc \
                        \  do \
                        \    call divide(m, n, q init, r init) \
                        \  endprogram"

main2 :: IO()
main2 = print $ tokenize "-4 + 8 * +4"

main3 :: IO()
main3 = print $ tokenize "(31/42) + (7/8)"
