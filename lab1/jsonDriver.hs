import JSON

testNested = JObject [
  ("name", JObject [("first",JString "Tyrion"), ("last",JString "Lannister")]),
  ("age", JNumber 32),
  ("siblings", JArray [
    JString "Jamie",
    JString "Cersei"]),
  ("pet", JNull)]

main :: IO ()
main = let arr = JSON.jsonRightTriangles
  in do
    putStrLn $ JSON.toString arr
    putStrLn $ JSON.toString testNested

