class Main inherits IO {
  g(): Int {{ out_string("G\n"); 1; }};
  h(): Int {{ out_string("H\n"); 2; }};
  f(a:Int, b:Int): Int { 0 };
  main(): Object {{ f(g(), h()); 0; }};
};
