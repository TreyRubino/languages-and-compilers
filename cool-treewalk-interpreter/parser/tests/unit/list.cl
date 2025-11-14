class List {
  head : Int;
  tail : List;

  isNil() : Bool { tail = void };
};

class Main inherits IO {
  main() : Object {
    let l : List <- new List in out_string("ok\n")
  };
};
