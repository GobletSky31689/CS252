/**
   Given two arrays of equal lengths, return an array of pairs (i.e.
   arrays of length 2) of corresponding entries. Don't use a loop,
   and don't use recursion.
*/
function correspondingPairs(a, b) {
    return a.map(function(elem){
    	return [elem, b.splice(0, 1).reduce(function(acc, val){return val;})];
    });
}

// console.log(correspondingPairs([], []));