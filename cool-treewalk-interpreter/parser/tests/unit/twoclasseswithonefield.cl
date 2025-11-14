class A { x : Int <- 5; };

class Main inherits IO {
  main() : Object {
    let a : A <- new A in out_int(a.x)
  };
};
