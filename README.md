# Linear Temporal Logic to Generalized Buechi Automaton converter

## Implementation

Based on ["Specification and Verification using Temporal Logics"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.217.7298) by Stéphane Demri , Paul Gastin.

First, Automaton module construct a set of edges including epslion transitions and sigma transitions.
Then remove epsilont transitions and annotate sigma transitions with postponed until formulae.
At last, merge parallel edges if possible, and output a graphviz file.

## Usage

```sh
$ omake
$ ./ltl2ba
# input LTL, one by one
00> XXXp
X X X p
01> F(not p or X(qUr))
F (¬p ∨ X (q U r))
<Exit with Ctrl-C>
# convert all .gv output into png
$ for s in *.gv; do dot -Tpng -o $s.png $s; done
```

## Limitations

- No simplification between states (only for parallel edges)

## License

The MIT License (MIT)

Copyright (c) 2013 tomykaira

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
