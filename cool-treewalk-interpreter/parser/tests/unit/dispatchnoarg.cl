class A inherits IO {
  greet(name : String) : Object {
    out_string("Hello, ") . out_string(name) . out_string("\n")
  };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in a.greet("world")
  };
};
