class A inherits IO {
  f(x : Int, y : Int) : Int { x + y };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in out_int((a@A.f(2, 3)))
  };
};
