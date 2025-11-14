class Main inherits IO {
  main() : Object {
    if 1 < 2 then
      if 2 < 3 then out_string("nested ok\n")
      else out_string("inner fail\n")
      fi
    else
      out_string("outer fail\n")
    fi
  };
};
