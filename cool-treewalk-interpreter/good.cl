(*
  The class A2I provides integer-to-string and string-to-integer
  conversion routines.  To use these routines, either inherit them
  in the class where needed, have a dummy variable bound to
  something of type A2I, or simpl write (new A2I).method(argument).
*)

(*
  c2i   Converts a 1-character string to an integer.  Aborts
        if the string is not "0" through "9"
*)
class A2I {

  c2i(char : String) : Int {
    if char = "0" then 0 else
    if char = "1" then 1 else
    if char = "2" then 2 else
    if char = "3" then 3 else
    if char = "4" then 4 else
    if char = "5" then 5 else
    if char = "6" then 6 else
    if char = "7" then 7 else
    if char = "8" then 8 else
    if char = "9" then 9 else
    { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
    fi fi fi fi fi fi fi fi fi fi
  };

  (*
    i2c is the inverse of c2i.
  *)
  i2c(i : Int) : String {
    if i = 0 then "0" else
    if i = 1 then "1" else
    if i = 2 then "2" else
    if i = 3 then "3" else
    if i = 4 then "4" else
    if i = 5 then "5" else
    if i = 6 then "6" else
    if i = 7 then "7" else
    if i = 8 then "8" else
    if i = 9 then "9" else
    { abort(); ""; }  -- the "" is needed to satisfy the typchecker
    fi fi fi fi fi fi fi fi fi fi
  };

  (*
    a2i converts an ASCII string into an integer.  The empty string
    is converted to 0.  Signed and unsigned strings are handled.  The
    method aborts if the string does not represent an integer.  Very
    long strings of digits produce strange answers because of arithmetic 
    overflow.
  *)
  a2i(s : String) : Int {
    if s.length() = 0 then 0 else
    if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
    if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
      a2i_aux(s)
    fi fi fi
  };

  (*
    a2i_aux converts the usigned portion of the string.  As a programming
    example, this method is written iteratively.
  *)
  a2i_aux(s : String) : Int {
    (let int : Int <- 0 in
      {
        (let j : Int <- s.length() in
          (let i : Int <- 0 in
            while i < j loop
              {
                int <- int * 10 + c2i(s.substr(i,1));
                i <- i + 1;
              }
            pool
          )
        );
        int;
      }
    )
  };

  (*
    i2a converts an integer to a string.  Positive and negative 
    numbers are handled correctly.  
  *)
  i2a(i : Int) : String {
    if i = 0 then "0" else 
    if 0 < i then i2a_aux(i) else
      "-".concat(i2a_aux(i * ~1)) 
    fi fi
  };

  (*
    i2a_aux is an example using recursion.
  *)    
  i2a_aux(i : Int) : String {
    if i = 0 then "" else 
      (let next : Int <- i / 10 in
        i2a_aux(next).concat(i2c(i - next * 10))
      )
    fi
  };

};

class Stack {
  items : List <- new Nil;

  push(x : Int) : Object {
    {
      items <- items.cons(x);
      self;
    }
  };

  pop() : Int {
    {
      let val : Int <- items.head() in
      {
        items <- items.tail();
        val;
      };
    }
  };

  top() : Int {
    items.head()
  };

  isEmpty() : Bool {
    items.isNil()
  };

  print() : Object {
    items.print()
  };
};

class List inherits IO {
  isNil() : Bool { false };
  head() : Int { 0 };
  tail() : List { self };
  cons(x : Int) : List {
    let n : Node <- new Node in n.init(x, self)
  };
  print() : Object {
    out_string("()")
  };
};

class Nil inherits List {
  isNil() : Bool { true };
  print() : Object {
    out_string("()")
  };
};

class Node inherits List {
  value : Int;
  next  : List;

  init(v : Int, n : List) : Node {
    {
      value <- v;
      next <- n;
      self;
    }
  };

  head() : Int { value };

  tail() : List { next };

  print() : Object {
    {
      out_string("(");
      out_int(value);
      let curr : List <- next in {
        while not curr.isNil() loop {
          out_string(" ");
          out_int(curr.head());
          curr <- curr.tail();
        } pool;
        out_string(")");
      };
    }
  };
};

class Main inherits IO {
  stack : Stack <- new Stack;
  input : String;

  main() : Object {
    {
      out_string("Stack Calculator. Commands: push <n>, pop, add, sub, mul, div, print, exit\n");
      while true loop
      {
        out_string("> ");
        input <- in_string();

        if input = "exit" then
          { abort(); }
        else
          (let len : Int <- input.length() in
            if 4 <= len then
              if input.substr(0,4) = "push" then
                if len <= 5 then
                  out_string("Usage: push <int>\n")
                else
                  if input.substr(4,1) = " " then
                    let arglen : Int <- len - 5 in
                      stack.push((new A2I).a2i(input.substr(5, arglen)))
                  else
                    out_string("Usage: push <int>\n")
                  fi
                fi
              else
                {
                  if input = "pop" then
                    { if stack.isEmpty() then out_string("Stack empty\n")
                      else {
                        let v : Int <- stack.pop() in {
                          out_string("Popped ");
                          out_int(v);
                          out_string("\n");
                        };
                      }
                      fi;
                    }
                  else
                    if input = "add" then { binop("+"); }
                    else
                      if input = "sub" then { binop("-"); }
                      else
                        if input = "mul" then { binop("*"); }
                        else
                          if input = "div" then { binop("/"); }
                          else
                            if input = "clear" then { clear(); }
                            else
                              if input = "print" then { stack.print(); out_string("\n"); }
                              else { out_string("Unknown command\n"); }
                              fi
                            fi
                          fi
                        fi
                      fi
                    fi
                  fi;
                }
              fi
            else
              {
                if input = "pop" then
                  { if stack.isEmpty() then out_string("Stack empty\n")
                    else {
                      let v : Int <- stack.pop() in {
                        out_string("Popped ");
                        out_int(v);
                        out_string("\n");
                      };
                    }
                    fi;
                  }
                else
                  if input = "add" then { binop("+"); }
                  else
                    if input = "sub" then { binop("-"); }
                    else
                      if input = "mul" then { binop("*"); }
                      else
                        if input = "div" then { binop("/"); }
                        else
                          if input = "clear" then { clear(); }
                          else
                            if input = "print" then { stack.print(); out_string("\n"); }
                            else { out_string("Unknown command\n"); }
                            fi
                          fi
                        fi
                      fi
                    fi
                  fi
                fi;
              }
            fi
          )
        fi;
      }
      pool;
    }
  };


  binop(op : String) : Object {
    if stack.isEmpty() then out_string("Stack empty\n")
    else {
      let b : Int <- stack.pop(),
        a : Int <- stack.pop(),
        result : Int <- 0 in {
        if op = "+" then result <- a + b
        else if op = "-" then result <- a - b
        else if op = "*" then result <- a * b
        else if op = "/" then result <- a / b
        else 0 fi fi fi fi;
        stack.push(result);
        out_int(result);
        out_string("\n");
      };
    } fi
  };

  clear() : Object {
    let i : Int <- 0 in {
      while i < 100 loop {
        out_string("\n");
        i <- i + 1;
      } pool;
      out_string("Stack Calculator. Commands: push <n>, pop, add, sub, mul, div, print, clear, exit\n");
      self;
    }
  };
};
