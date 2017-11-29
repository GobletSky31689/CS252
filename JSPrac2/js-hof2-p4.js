function comp(x, y) {
    return x == y ? 0 : x < y ? -1 : 1;
}

/*
  Given one or two "key extraction functions", yield a comparator
  that compares its arguments by applying the first key extraction
  function to both objects and comparing the result (which is expected
  to be a number). If there is a tie, and there is a second function
  supplied, use it in the same way.
*/
function compare(fun1, fun2) {
    return function(x, y) { 
    	var first = comp(fun1(x), fun1(y));
    	return first==0&&fun2?comp(fun2(x), fun2(y)):first;
    };
}


// The following file is used to check your work:

// Tester.js

// var a = ["Mary", "had", "a", "little", "lamb"]
// a.sort(compare(function(x) { return x.length(); }))
// print(a)
// print("Expected: a,had,Mary,lamb,little")
// a = ["Mary", "had", "a", "little", "lamb"]
// a.sort(compare(function(x) { return x.length() },
//                function(x) { return x.hashCode() % 10 }))
// print(a)
// print("Expected: a,had,lamb,Mary,little")
// a = [{width: 30, height: 20},
//      {width: 20, height: 30},
//      {width: 20, height: 20},
//      {width: 30, height: 30}]
// a.sort(compare(function(x) { return x.height }))
// print(JSON.stringify(a))
// print('Expected: [{"width":30,"height":20},{"width":20,"height":20},{"width":20,"height":30},{"width":30,"height":30}]')
// a.sort(compare(function(x) { return x.height }, function(x) { return x.width }))
// print(JSON.stringify(a))
// print('Expected: [{"width":20,"height":20},{"width":30,"height":20},{"width":20,"height":30},{"width":30,"height":30}]')