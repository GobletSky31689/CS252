name = "Monty";
function Rabbit(name) {
  this.name = name;
}
var r = Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"
