/**
   Given an array of arrays of equal lengths, return an array of arrays
   of corresponding entries (i.e. just like the last assignment,
   except now there can be n arrays, not just two, and they are 
   put into an array. Don't use a loop, and don't use recursion.
*/

arr = [[], [], [], []];

function correspondingElements(as) {

	if(as.length == 0)
		return as;

	var first = as.splice(0, 1).reduce(function(acc, val){return val;}, []);
	first = first.map(function(elem){
		return [elem];
	});	

	if(as.length == 0)
		return first;

	return as.reduce(function(acc, val){
		return acc.map(function(elem){
			return elem.concat(val.splice(0, 1));
		});
	}, first);

}

// console.log(correspondingElements(arr));

