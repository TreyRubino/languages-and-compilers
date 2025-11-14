class Main inherits IO {
  main(): Object {{
    let a:Int <- in_int(),
        b:Int <- in_int()
    in {
      out_int(a); out_string("\n");   -- expect 2147483647
      out_int(b); out_string("\n");   -- expect -2147483648
      0;
    };
  }};
};
