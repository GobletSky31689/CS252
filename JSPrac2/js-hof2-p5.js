/*
  Given two functions f and g, yield a 
  function that first applies f, and if the
  result is falsy (in the JavaScript sense),
  yields the result of applying g if g was
  supplied. Otherwise return the result of
  applying f
*/
function orElse(f, g) {
    return function(x) { 
      var val = f(x);
      return !val && g? g(x) : val;
    }
}


// The following file is used to check your work:

// Tester.js

// var f1 = orElse(function(x) { return x - 1 },
//                 function(x) { return x + 1 })
// print(f1(2))
// print("Expected: 1")
// print(f1(1))
// print("Expected: 2")

// var f2 = orElse(function(x) { return x - 1 })
// print(f2(2))
// print("Expected: 1")
// print(f2(1))
// print("Expected: 0")

// var f3 = orElse(function(x) { return x > 0 },
//                 function(x) { return x < 0 })
// print(f3(1))
// print("Expected: true")
// print(f3(-1))
// print("Expected: true")
// print(f3(0))
// print("Expected: false")