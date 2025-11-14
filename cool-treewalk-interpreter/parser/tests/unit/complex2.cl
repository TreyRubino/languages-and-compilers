class Main inherits IO {
  x : Int <- 10;
  y : Int <- 20;
  main() : Object {
    {
      out_int((x + y) * (y - x));
    }
  };
};
