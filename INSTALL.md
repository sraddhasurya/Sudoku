# INSTALL

1) For this project we are using OCaml 4.14.1. Create/load the local switch  
```bash
opam switch create . ocaml-base-compiler.4.14.1  # skip if already created
eval "$(opam env)"                               # run in each new shell
```

2) Install deps (dune, yojson, ounit2)  
```bash
opam install . --deps-only --with-test
```

3) Build/Run  
```bash
dune build
```
Run the CLI:  
```bash
dune exec sudoku ./sample_board.json
```

4) Run tests  
```bash
dune test
```
