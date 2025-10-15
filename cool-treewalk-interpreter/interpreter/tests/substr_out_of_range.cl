(*
test showcases the reaction of the interpreter when 
a substr is requested that is out of range of the base string.
*)


class Main inherits IO {
  s : String <- "I guess we'll get started.";
  main() : Object {
    (* this will create a runtime error *)
    out_string(s.substr(26, s.length()))
  };  
};