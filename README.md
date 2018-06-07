# CamlPaver

An interval constraint solver implemented with OCaml.


## Dependencies

- [kv](http://verifiedby.me/kv/)
    - It depends on [Boost](https://www.boost.org/)
- [Hashcons](https://github.com/backtracking/ocaml-hashcons)
- [Jbuilder (Dune)](https://github.com/ocaml/dune)


## Usage 

```.sh
$ cd interval; make; cd ..

$ jbuilder build main.exe

$ cat benchs/example.bch
Variables
  x in [-1e+8,  1e+8];

Constraints
x^2 = 2;

end
    
$ ./_build/default/main.exe -e 1 ./benchs/1.bch
{
<|"x" -> Interval[{1.414214,1.414214}]|>,

<|"x" -> Interval[{-1.414214,-1.414214}]|>
}
```
