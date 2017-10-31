function Employee(fname, lname, salary) {
  this.fname = fname;
  this.lname = lname;
  this.salary = salary;
}

var emps = [new Employee("Alice", "Alleyson", 95000),
            new Employee("Robert", "Tables", "80000"),
            new Employee("Charles", "Chaplin", 42350)];

function totalSalary(empList) {
  var ttl = 0;
  for (i in empList) {
    ttl += empList[i].salary;
  }
  return ttl;
}

console.log(totalSalary(emps));

