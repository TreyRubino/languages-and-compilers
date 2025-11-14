class A inherits IO {
  f(x : Int) : Int { x + 1 };
};

class Main inherits IO {
  a : A <- new A;
  main() : Object {
    out_int(a.f(5))
  };
};
