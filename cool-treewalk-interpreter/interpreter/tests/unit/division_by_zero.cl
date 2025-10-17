(*
test showcases the reaction of the interpreter when 
a division by zero error happens.
*)

class Main inherits IO {
  main() : Object {
    let x : Int <- 5 in
    {
      x / 0;
    }
  };  
};  