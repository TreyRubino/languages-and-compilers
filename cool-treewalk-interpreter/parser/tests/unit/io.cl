class Main inherits IO {
  main() : Object {
    {
      out_string("Enter a number: ");
      let x : Int <- in_int() in out_int(x + 1);
    }
  };
};
