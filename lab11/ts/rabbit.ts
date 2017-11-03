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



