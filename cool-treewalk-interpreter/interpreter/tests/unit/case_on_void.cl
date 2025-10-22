(*
this case file will demonstrate the interpreters response 
to a case statement on a void.
*)

class A {
  var : Object; (* expecting type Int, but has not been defined, only declared. *)
  getVar() : Object { var };
};

class Main inherits IO {
  a : A <- new A;
  main() : Object {
    case a.getVar() of
      a : A => out_string("Class type is A\n");
      i : Int => out_string("Class type is Int\n");
      o : Object => out_string("Class type is Object\n");
    esac
  };
};