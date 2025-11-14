class Main inherits IO {
  main(): Object {
    {
    if "a"  <  "b"  then out_string("OK1\n") else out_string("BAD\n") fi;
    if "x"  <= "x"  then out_string("OK2\n") else out_string("BAD\n") fi;
    if "abc" < "abz" then out_string("OK3\n") else out_string("BAD\n") fi;
    0;
  }
  };
};
