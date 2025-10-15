(*
test case to show off dispatching on void and that
the isvoid check is catching it properly.
*)

class A {
  get() : SELF_TYPE { self };
};

class Main inherits IO {
  a : A; (* variable a has type A but the so it was declared, but not yet defined. So its runtime type is void *)
  main() : Object {
    if isvoid a then {
      a.get();
      out_string("Not void\n");
    } else {
      out_string("Void\n");
      0;
    } fi
  };
};