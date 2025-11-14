class Main inherits IO {
  main(): Object {{
    let x:Int <- in_int() in { out_int(x); out_string("\n"); 0; };
  }};
};
