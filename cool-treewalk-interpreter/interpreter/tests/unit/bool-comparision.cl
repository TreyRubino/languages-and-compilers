class Main inherits IO {
  main(): Object {
    {
      if true = false then out_string("BAD\n") else out_string("OK1\n") fi;
      if true = true  then out_string("OK2\n") else out_string("BAD\n") fi;
      0;
    }
  };
};
