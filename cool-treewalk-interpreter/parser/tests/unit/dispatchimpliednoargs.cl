class A inherits IO {
  f() : Int { 42 };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in out_int(a.f())
  };
};
