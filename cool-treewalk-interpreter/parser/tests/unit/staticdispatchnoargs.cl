class A inherits IO {
  f() : Int { 10 };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in out_int((a@A.f()))
  };
};
