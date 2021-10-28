# JavaSrc2cpg

This is a [CPG](https://docs.joern.io/code-property-graph/) frontend based on Java source code powered by 
[JavaParser](https://javaparser.org).

[![release](https://github.com/joernio/javasrc2cpg/actions/workflows/release.yml/badge.svg)](https://github.com/joernio/javasrc2cpg/actions/workflows/release.yml)
[![Discord](https://img.shields.io/badge/-Discord-lime?style=for-the-badge&logo=discord&logoColor=white&color=black)](https://discord.com/invite/vv4MH284Hc)

## Setup

Requirements:
- \>= JDK 11. We recommend OpenJDK 11.
- sbt (https://www.scala-sbt.org/)

### Quickstart

1. Clone the project
2. Build the project `sbt stage`
3. Create a CPG `./javasrc2cpg.sh /path/to/your/code -o /path/to/cpg.bin`
4. Download Joern with
   ```
   wget https://github.com/joernio/joern/releases/latest/download/joern-cli.zip
   unzip joern-cli.zip
   cd joern-cli
   ```
5. Copy `cpg.bin` into the Joern directory
6. Start Joern with `./joern.sh`
7. Import the cpg with `importCpg("cpg.bin")`
8. Now you can query the CPG 

### Development

Some general development habits for the project:

- When making a branch, use the following template `<short-name>/<feature-or-bug-name>` 
  e.g. `fabs/control-structure-nodes`.
- We currently focus around test driven development. Pay attention to the code coverage when creating new tests and 
  features. The code coverage report can be found under `./target/scala-2.13/scoverage-report`.

### TODO
- [x] Explicit constructor invocations
- [x] Propagate context up and down while creating AST
- [ ] `code` field for method calls (`this` for `System.out` in `System.out.println`)
- [ ] Logging
- [ ] Handle body of `try`/`catch` 
- [ ] Local class declaration statements
- [ ] Lambda expressions
- [ ] Method Reference Expr
- [ ] Throw statements (AST is simple, control flow to catch seems harder)
- [ ] `this`/`super` expressions (scope for FieldAccess)
- [ ] Type expressions (part of MethodReferenceExpr)
- [ ] `instanceof` 
- [ ] Pattern expr as part of `instanceof` (Java 14)
- [ ] `switch` expressions (including `yield` statements) (Introduced Java 12/13)
- [ ] Local record declaration statements (Java 14 Preview feature)
- [ ] Dataflow tests for inheritance

### MAYBE TODO
- [ ] Type arguments for generics
- [ ] Annotations
- [ ] Cast expressions (maybe not necessary if `javaparser` resolves types correctly)
- [ ] Synchronized statements (if we don't just ignore those)
- [ ] Control flow for labeled breaks
