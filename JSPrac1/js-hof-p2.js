/**
   Given an array of numbers, return the product of the square roots 
   of the non-negative ones. Use filter/map/reduce. Don't use a loop,
   and don't use recursion.
*/
function prodNonNegRoots(a) {
    return a.filter(function(x){
  			return x >=0;
		}).map(function(x){
			return Math.sqrt(x);
		}).reduce(function(acc, val){
			return acc * val;
		}, 1);
}


// console.log(prodNonNegRoots([1, 4, -9, 16]));