class Main inherits IO {
  main(): Object {{
    let o : Object in {
      if isvoid o then out_string("OK1\n") else out_string("BAD\n") fi;
      if isvoid (new Object) then out_string("BAD\n") else out_string("OK2\n") fi;

      let a : Object, b : Object in {
        if a = b then out_string("OK3\n") else out_string("BAD\n") fi;         -- null==null
        if (new Object) = a then out_string("BAD\n") else out_string("OK4\n") fi; -- obj!=null
        0;
      };

      0;
    };
  }};
};
