//JSLint Options - "in development", "for statement", "white mess", "this"
var name = "Monty";
function Rabbit(name) {
    "use strict";
    this.name = name;
}
var r = new Rabbit("Python");
console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"














//JSLint Options - "in development", "for statement", "white mess"
function swap(arr, i, j) {
  "use strict";
  var tmp = arr[i]; 
  arr[i] = arr[j]; 
  arr[j] = tmp;
}
function sortAndGetLargest (arr) {
  "use strict";
  var tmp = arr[0]; // largest elem
  var i = 0;
  var j = 0;
  for (i=0; i<arr.length; i += 1) {
    if (arr[i] > tmp) {
     tmp = arr[i];
    }
    for (j=i+1; j<arr.length; j+=1) {
      if (arr[i] < arr[j]) {
        swap(arr,i,j);
      }
    }
  }
  return tmp;
}
var largest = sortAndGetLargest([99,2,43,8,0,21,12]);
console.log(largest); // should be 99, but prints 0 (Now it prints 99)












var name:string = "Monty";
class Rabbit{
  name: string;
  constructor(n: string){
    this.name = n;
  }
}
var r = new Rabbit("Python");
console.log(r.name); 
console.log(name);
export {};










function swap(arr,i,j) {
  var tmp = arr[i]; 
  arr[i] = arr[j]; 
  arr[j] = tmp;
}
function sortAndGetLargest (arr) {
  var tmp = arr[0];
  for (var i=0; i<arr.length; i++) {
    if (arr[i] > tmp){
    	tmp = arr[i];
    } 
    for (var j=i+1; j<arr.length; j++)
    {
    	if (arr[i] < arr[j]) swap(arr,i,j);
    }  
  }
  return tmp;
}
var largest = sortAndGetLargest([99,2,43,8,0,21,12]);
console.log(largest);

