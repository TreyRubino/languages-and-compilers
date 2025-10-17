(*
test case to show off aliasing
aliasing is when two "names" refer to the same "object" in memory
*)

class IntWrapper {
  myInt : Int <- 0;
  set(newInt : Int) : Int { myInt <- newInt };
  get()             : Int { myInt };
  incr()            : Int { myInt <- myInt + 1 }; 
};

class Main inherits IO {
  a : IntWrapper <- new IntWrapper; 
  b : IntWrapper <- new IntWrapper; 
  c : IntWrapper <- new IntWrapper; 

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
      process(a, b);
      process(c, c);

      let p : IntWrapper <- b in
      let q : IntWrapper <- b in
      let r : IntWrapper <- c in
      {
        out_int(p.get());
        out_string(" ");
        out_int(q.get());
        out_string(" ");
        out_int(r.get());
        out_string(" ");
        p.incr();
        out_int(p.get());
        out_string(" ");
        out_int(q.get());
        out_string(" ");
        out_int(r.get());
        out_string(" ");
        out_string("\n");
      };
    }
  }; 
};