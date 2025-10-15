(*
test case to show off aliasing
aliasing is when two "names" refer to the same "object" in memory
as well as dispatching and SELF_TYPE resolution
*)

class IntWrapper {
  myInt : Int <- 0;
  set(newInt : Int) : Int { myInt <- newInt };
  get()             : Int { myInt };
  incr()            : SELF_TYPE { { myInt <- myInt + 1; self; } }; 
};

class InWrapperChild inherits IntWrapper {

};

(*
ways to make new names for the same object: 
  (1) formal parameters + dispatch
  (2) let
*)
class Main inherits IO {
  a : IntWrapper <- new InWrapperChild; 
  b : IntWrapper <- new InWrapperChild; 
  c : IntWrapper <- new InWrapperChild; 

  process(x : IntWrapper, y : IntWrapper) : Object {
    {
      out_int(x.get());
      out_string(" ");
      out_int(y.get());
      out_string(" ");
      x.incr();
      out_int(x.get());
      out_string(" ");
      out_int(y.get());
      out_string("\n");
    }
  };

  main() : Object {
    {
      a.set(11);
      b.set(33);
      c.set(55); 
      process(a.incr(), b.incr());
      process(c.incr(), c.incr());
    }
  }; 
};