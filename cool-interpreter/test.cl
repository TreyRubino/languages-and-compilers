class A {
  a_val : Int <- 41;
};

class B inherits A {
  b_msg       : String <- "B";
  from_a_copy : Int    <- a_val;        -- uses inherited attribute in initializer
  get_a_plus_one() : Int { a_val + 1 }; -- uses inherited attribute in method body
};

class Main inherits IO {
  my_attr : Int <- 5 + 5;               -- init conformance check (Int)

  main() : Object {
    {
      out_string("Hello, world.\n");
      out_int((new B).get_a_plus_one());  -- 42

      -- Case: branches return Int and String → LUB(Object)
      let o : Object <-
        case new B of
          x : A  => 1;
          y : IO => "io";
        esac
      in
        if isvoid o then abort() else o fi 
      ;
    }
  };
};
