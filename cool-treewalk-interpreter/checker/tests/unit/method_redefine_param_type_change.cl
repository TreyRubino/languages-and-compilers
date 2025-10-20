class A {
  f(x : Int) : Int { x };
};

class B inherits A {
  f(x : String) : Int { 0 }; 
};

class Main inherits IO {
  main() : Object {};
};