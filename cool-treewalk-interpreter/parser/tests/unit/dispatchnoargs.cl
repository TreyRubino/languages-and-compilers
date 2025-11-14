class A inherits IO {
  say() : Object { out_string("hello\n") };
};

class Main inherits IO {
  main() : Object {
    let a : A <- new A in a.say()
  };
};
