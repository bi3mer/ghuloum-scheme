# ghuloum-scheme

Simple implementation of Scheme based on Abdulaziz Ghuloum's [paper](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) written in OCaml. Note, this outputs [x86 assembly.](https://en.wikipedia.org/wiki/X86_assembly_language) So, for example, if you are on a mac with an M-series chip, this will not work since those use [ARM.](https://en.wikipedia.org/wiki/ARM_architecture_family)

## Running

```
dune exec ghuloum-scheme && gcc startup.c out.s && ./a.out
```

## Testing

```
dune test
```
