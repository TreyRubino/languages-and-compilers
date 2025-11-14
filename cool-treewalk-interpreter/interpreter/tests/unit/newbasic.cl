class Main inherits IO {
  main(): Object {{
    let i:Int    <- (new Int),
        b:Bool   <- (new Bool),
        s:String <- (new String)
    in {
      out_int(i); out_string("\n");          -- expect 0
      if b then out_string("T\n") else out_string("F\n") fi;  -- expect F
      out_int(s.length()); out_string("\n"); -- expect 0
      0;
    };
  }};
};
