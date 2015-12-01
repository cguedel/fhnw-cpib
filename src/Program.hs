import Scanner

main :: IO()
main = print $ tokenize "+=<=\n\
  \>=<=a134 734'000/123'000 bool"
