class Main inherits IO {
  method(x : Int) : Int {
    x + 1
  };

  main() : Object {
    out_int(method(5))
  };
};
