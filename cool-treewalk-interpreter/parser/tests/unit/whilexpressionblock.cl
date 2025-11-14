class Main inherits IO {
  main() : Object {
    {
      let i : Int <- 0 in
        while i < 3 loop {
          out_int(i);
          i <- i + 1;
        } pool;
    }
  };
};
