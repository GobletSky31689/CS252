// Highlights some of the basic functionality of JavaScript.

// With JavaScript, we can specify new values with the var keyword.
// Using the keyword var is not strictly required, but it is considered a best practice.
var x=42, y=7;


// JavaScript is a functional language, in the sense that functions are first class objects.
// (WARNING: not everyone likes this definition of functional languages).
// We can write them in two different ways.
function add(a,b) {
  return a + b;
}

var square = function(a) {
  return a * a;
}

// Printing varies by platform. Node uses console.log.
console.log("x + y = " + add(x,y));

// That is annoying, so let's store it in another variable.
var print = console.log;

print("x^2 = " + square(x));

// Since functions are first class values, we can do interesting stuff with them.
function applyFunToX(f) {
  f(x);
}

applyFunToX(print);

// JavaScript functions are **closures**.  They remember their surrounding scope.
var getNextInt = function() {
  var nextInt = 0;
  return function() {
    return nextInt++;
  }
}();

print(getNextInt());
print(getNextInt());
print(getNextInt());


// Objects in JavaScript work a little differently.
var complex = { real: 3, imaginary: 1 };
print (complex);

var Dog = {
  speak: function() { print('bark!'); }
};

rex = { name: 'rex', __proto__: Dog } // Manually setting the prototype chain -- not universal
rex.speak();

// We can add or remove properties at runtime
rex['favoriteToy'] = 'squeaky ball'; // could write this as rex.favoriteToy
print(rex);
//delete rex.name;

// We can override properties if we wish
rex.speak = function() { print('grr....'); }
rex.speak();

// Or delete them, with perhaps surprising results
delete rex.speak;
rex.speak();


// The more standard way of adding JavaScript objects: first create a constructor
function Cat(name, breed) {
  this.name = name;
  this.breed = breed;
  this.speak = function() { print('meow!'); }
}

var garfield = new Cat('Garfield', 'Orange tabby');
print(garfield);
garfield.speak();

Cat.prototype.makeAngryNoise = function() {
  print('hiss');
}
garfield.makeAngryNoise();

Cat.prototype.sayName = function() {
  print("My name is " + this.name);
}
garfield.sayName();

garfield.favoriteFood = 'lasagna';

var animals = [garfield, new Cat('mimi', 'Calico'), rex];

var forEach = function(arr,f) {
  var i;
  for (i=0; i<arr.length; i++) {
    f(arr[i]);
  }
}
print("Displaying all animals");
forEach(animals, print);

print("Each animal say your name");
forEach(animals, function(animal) {
  animal.speak();
  //animal.sayName(); // Won't work -- rex has no sayName method
  Cat.prototype.sayName.apply(animal);
});


Function.prototype.curry = function() {
  var slice = Array.prototype.slice,
      args = slice.apply(arguments),
      that = this;
  return function () {
    return that.apply(null, args.concat(slice.apply(arguments)));
  };
};

var addOne = add.curry(1);
console.log(addOne(3));

