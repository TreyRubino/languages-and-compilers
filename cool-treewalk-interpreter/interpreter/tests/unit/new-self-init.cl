class A {
  tag(): String { "A" };
  mk(): A { new SELF_TYPE };
};

class B inherits A {
  tag(): String { "B" };
};

class Main inherits IO {
  main(): Object {{
    let b:B <- new B in {
      if b.mk().tag() = "B" then out_string("OK\n") else out_string("BAD\n") fi;
      0;
    };
  }};
};
