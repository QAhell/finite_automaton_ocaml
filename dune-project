(lang dune 3.4)

(name finite_automaton)

(generate_opam_files true)

(source
 (github QAhell/finite_automaton_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license "Apache-2.0")

(documentation https://github.com/QAhell/finite_automaton_ocaml)

(implicit_transitive_deps false)

(package
 (name finite_automaton)
 (version 0.1)
 (synopsis "An implementation of finite automata")
 (description "An implementation of finite automat")
 (depends ocaml dune)
 (tags (topics automaton automata finite_automata)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
