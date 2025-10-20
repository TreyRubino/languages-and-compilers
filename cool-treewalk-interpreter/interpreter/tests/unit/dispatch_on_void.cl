(*
test case to show off dispatching on void and that
the isvoid check is catching it properly.
*)

class A {
  get() : SELF_TYPE { self };
};

class Main inherits IO {
  a : A; (* variable a has type A but it was only declared, but not yet defined. So its runtime type is void *)
  main() : Object {
    if isvoid a then {
      a.get();
    } else {
      out_string("Not void\n");
    } fi
  };
};