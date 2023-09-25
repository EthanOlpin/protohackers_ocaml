# Protohackers OCaml

A beginners' attempts at solving the [Protohackers networking problems](https://protohackers.com/) with OCaml

## Running the solutions

1. Clone the repo
2. Switch to the new directory
3. Create an empty switch

    ```
    opam switch create protohackers_ocaml 5.1.0
    ```
4. Install the project dependencies

    ```
    opam install . --locked
    ```
5. Run a solution, e.g. for [Problem 1](https://protohackers.com/problem/1)

    ```
    dune exec problem01
    ```
