class A inherits IO {
  f(x : Int) : Int { x + 2 };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in out_int((a@A.f(3)))
  };
};
