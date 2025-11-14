class Graph inherits IO {
  nodes : Int <- 5;

  print() : Object {
    out_string("Graph with ")
      . out_int(nodes)
      . out_string(" nodes\n")
  };
};

class Main inherits IO {
  main() : Object {
    let g : Graph <- new Graph in g.print()
  };
};
