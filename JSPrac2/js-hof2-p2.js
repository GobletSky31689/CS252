/*
  Provide a function repeat that repeats a given function
  at least once until a given condition is true. 
*/
function repeatUntil(f, cond) {
	return function(x){
		var result = f(x);
		while(!cond(result)){
			result = f(result);
		}
		return result;
	};
}


// The following file is used to check your work:

// Tester.js

// var r = repeatUntil(function(x) { return x + x }, function(x) { return x >= 20 })
// print(r(2))
// print("Expected: 32")
// print(r(10))
// print("Expected: 20")
// print(r(20))
// print("Expected: 40")

// r = repeatUntil(function(x) { return x + x }, function(x) { return x.length() >= 10 })

// print(r("Hello"))
// print("Expected: HelloHello");