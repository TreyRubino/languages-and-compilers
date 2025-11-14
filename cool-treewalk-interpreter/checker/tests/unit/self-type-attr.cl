class Main inherits IO {
  f() : SELF_TYPE {
    if true then self else new Object fi
  };
  main() : Object { out_string("OK\n") };
};
