class A {
  a : Int <- { (new IO).out_string("A\n"); 0; };
};

class B inherits A {
  b : Int <- {(new IO).out_string("B\n"); 0; };
};

class Main {
  main(): Object {{ new B; 0; }};
};
