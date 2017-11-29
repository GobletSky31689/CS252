function withProb(prob, f) {
  if (Math.random() < prob) {
    return f();
  }
}


function foo(x) {
  withProb(0.5, function() {
    console.log("Execution withProb callback");
    return 0;
  });
  return x;
}

console.log("Foo is " + foo(1));

