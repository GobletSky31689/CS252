// 'use strict'

var foldl = function (f, acc, array) {
	if(array.length == 0)
		return acc;
	var new_acc = f(acc, array[0]);
	if(array.length == 1)
		return new_acc;
	return foldl(f, new_acc, array.slice(1));
}

console.log(foldl(function(x,y){return x+y}, 0, [1,2,3]));

var foldr = function (f, z, array) {
	if(array.length == 0)
		return z;
	var new_acc = f(array.slice(-1)[0], z);
	if(array.length == 1)
		return new_acc;
	return foldr(f, new_acc, array.slice(0, -1));
}

console.log(foldr(function(x,y){return x/y}, 1, [2,4,8]));

var map = function (f, array) {
	if (array.length == 0)
		return [];
	var arr = map(f, array.slice(1))
	arr.unshift(f(array[0]));
	return arr;
}

console.log(map(function(x){return x+x}, [1,2,3,5,7,9,11,13]));


// Write a curry function as we discussed in class.
// Create a `double` method using the curry function
// and the following `mult` function.
function mult(x,y) {
  return x * y;
}

Function.prototype.curry = function() {
  var slice = Array.prototype.slice,
      args = slice.apply(arguments),
      that = this;
  return function () {
    return that.apply(null, args.concat(slice.apply(arguments)));
  };
};

var double = mult.curry(2);

console.log(double(5));


function Student(fName, lName, sId) {
  this.firstName = fName;
  this.lastName = lName;
  this.studentID = sId
  this.display = function() { 
  	console.log(this.firstName, this.lastName, this.studentID);
  }
}

var aplp_class = [
	new Student('A', 'a', 1),
	new Student('B', 'b', 2),
	new Student('C', 'c', 3),
	new Student('D', 'd', 4),
]

aplp_class[0].display();

for(var index in aplp_class) {
	aplp_class[index].display();
}

aplp_class[2].grduated = true; //C just graduated

var student_e = {
	firstName: "E",
	lastName: "e",
	studentID: 5,
	__proto__: new Student()
}

student_e.display()

