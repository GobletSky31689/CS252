/**
   Turn an array of strings containing integers into an array
   of integers. Don't use a loop, and don't use recursion.
*/
function toNums(a) {
	return a.map(function(x) {
   		return parseInt(x);
	});
}
