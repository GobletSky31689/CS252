/*
  The scan function has three arguments: an array, a function with
  two arguments, and an initial value. Apply f as follows:

        f
       / \
      f   a[2]
     / \
    f   a[1]
   / \
init  a[0]

  and return an array of all function results.

*/
function scan(a, init, f)
{
  return a.map(function(x) {
      init = f(init, x);
      return init;
  });
}
