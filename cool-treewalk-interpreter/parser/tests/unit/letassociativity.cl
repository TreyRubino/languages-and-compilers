class Main inherits IO {
  main() : Object {
    let x : Int <- 1 in
    let y : Int <- x + 1 in
    let z : Int <- y + 1 in
    out_int(z)
  };
};
