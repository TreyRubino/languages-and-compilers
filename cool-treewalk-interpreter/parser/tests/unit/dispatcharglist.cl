class A inherits IO {
  f(x : Int, y : Int) : Int { x + y };
};

class Main inherits IO {
  a : A <- new A;
  main() : Object {
    out_int(a.f(3, 4))
  };
};
