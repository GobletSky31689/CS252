// syntax rotate = function (ctx) {
//   var inCtx = ctx.contextify(ctx.next().value);
//   var arrCtx = [];
//   for (let stx of inCtx) {
//     arrCtx = arrCtx.concat([stx]);
//     inCtx.next();
//   }
//   var argLength = arrCtx.length-1;
//   var result = #`var tmp;`;
//   for (var i=0;i<arrCtx.length/2;i++) {
//     result = result.concat(
//       #`tmp = ${arrCtx[i].value}; 
//       ${arrCtx[i].value} = ${arrCtx[argLength-i].value}; 
//       ${arrCtx[argLength-i].value} = tmp`);
//   }
//   return result;
// }

import { unwrap } from '@sweet-js/helpers' for syntax;

syntax rotate = function (ctx) {
  var arrCtx = [];
  var item = ctx.next().value;
  while (unwrap(item).value != ';') {
    arrCtx = arrCtx.concat([item]);
    item = ctx.next().value;
  }
  var result = #`var tmp;`;
  var argLength = arrCtx.length-1;
  for (var i=0;i<arrCtx.length/2;i++) {
    result = result.concat(
      #`tmp = ${arrCtx[i].value}; 
      ${arrCtx[i].value} = ${arrCtx[argLength-i].value}; 
      ${arrCtx[argLength-i].value} = tmp`);
  }
  return result;
}

var aa = 10; var b = 20; var c = 22; var d = 2;
console.log("aa:" + aa + " b:" + b + " c:" + c + " d:" + d);
// rotate(aa,b,c);
rotate aa b c d;
console.log("aa:" + aa + " b:" + b + " c:" + c + " d:" + d);
