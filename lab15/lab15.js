let roHandler = {
  deleteProperty: function(t, prop) { 
    delete t[prop];
    console.log('Delete property: ' + prop);
    return true;
  },
  has: function(target, prop) {
    console.log('Check property: ' + prop);
    return prop in target;
  },
  set: function(t, prop, val, rcvr) {
    t[prop] = val; 
    console.log('Set property: ' + prop);
    return true;
  },
  get: function(target, prop, receiver) {
    console.log('Get property: ' + prop);
    return target[prop];
  },
  apply: function(target, thisArg, argumentsList) {
    console.log('Function called with args: ' + argumentsList);
    return target.apply(thisArg, argumentsList);
  },
  getPrototypeOf(t) {
    console.log('Prototype accessed');
    return Object.getPrototypeOf(t);
  },
  setPrototypeOf: function(t,p) { 
    console.log('Prototype changed');
    Object.setPrototypeOf(t, p);
    return true; 
  },
  isExtensible: function(t) {
    console.log('Extensibility checked');
    return Object.isExtensible(t);
  },
  preventExtensions: function(t) {
    console.log('Extensions prevented');
    Object.preventExtensions(t);
    return true;
  },
  construct: function(t, args, nt) {
    console.log('New instance created woth args: ' + args);
    return new t(args);
  },
  getOwnPropertyDescriptor: function(t, p) {
    console.log('PropertyDescriptor accessed for property: ' + p);
    return Object.getOwnPropertyDescriptor(t, p)
  },
  defineProperty: function(t, p, d) {
    console.log('New propDesc defined for prop ' + p);
    Object.defineProperty(t, p, d);
  },
  ownKeys: function(t) {
    console.log('keys list accessed');
    return Object.getOwnPropertyNames(t);
  }
};

// var obj = {a: "lol"};
// obj = new Proxy(obj, roHandler);
// console.log(Object.getOwnPropertyDescriptor(obj, "a"));
// console.log(Object.keys(obj));

// var fa = function(){
//   return "lol";
// }
// fa = new Proxy(fa, roHandler);
// console.log(Object.getPrototypeOf(fa));


// var Dog = function(name){
//   this.name = name;
// }
// Dog = new Proxy(Dog, roHandler);
// var myDog = new Dog("Bolt");


// var constantVals = {
//   pi: 3.14,
//   e: 2.718,
//   goldenRatio: 1.30357 };
// var p = new Proxy(constantVals, roHandler);
// console.log(p.pi);
// p.ad = false;
// console.log("ad" in p);
// delete p.pi;
// console.log(p.pi);
// p.pi = 3;
// console.log(p.pi);