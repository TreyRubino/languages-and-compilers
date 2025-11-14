class A {
  x : Int <- 1;
  tag(): String { "A" };
};

class B inherits A {
  tag(): String { "B" };
};

class Main inherits IO {
  main(): Object {{
    let b:B <- new B in
    let c:B <- b.copy() in {
      if c.tag() = "B" then out_string("OK1\n") else out_string("BAD\n") fi;
      if b = c then out_string("BAD\n") else out_string("OK2\n") fi;  -- reference inequality
      0;
    };
  }};
};
