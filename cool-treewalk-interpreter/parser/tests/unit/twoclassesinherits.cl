class A { x : Int <- 1; };

class B inherits A {
  y : Int <- 2;
  f() : Int { x + y };
};

class Main inherits IO {
  main() : Object {
    let b : B <- new B in out_int(b.f())
  };
};
