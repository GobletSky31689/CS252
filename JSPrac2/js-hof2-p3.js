/*
  Compute the minimum element in the (possibly sparse) array a, skipping
  any non-existent elements. (Hint: forEach)
  If there are no elements, return undefined.
  If a comparison function is provided, use it to compare elements.
  Otherwise, convert elements to strings and convert them. (That 
  may sound bizarre, but it is exactly what Array.prototype.sort does.)
*/
function min(a, comp) {
   if(a.length==0)
      return undefined;
	var min_val = 0;
	// Init min_val with first element
	for(x in a){
		min_val = a[x];
		break;
	}
	if(comp){
		a.forEach(function(elem){
			if(comp(min_val, elem)>0){
				min_val = elem;	
			}
		});
	} else {
		a.forEach(function(elem){
			if(min_val.toString() > elem.toString()) {
				min_val = elem;
			}
		});
	}
	return min_val;
}


console.log(min([80, 9, 99], function(x, y) { return x - y }));//, function(x, y){return x>y?1:(x==y?0:-1)}));