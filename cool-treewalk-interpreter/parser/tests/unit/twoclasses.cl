class A inherits IO {
  greet() : Object { out_string("hi\n") };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in a.greet()
  };
};
