## Project Overview

Orchid is a command-line tool that accelerates compiler and interpreter development by generating production-ready parsing code from EBNF grammar definitions.

### What It Does

Given an EBNF (Extended Backus-Naur Form) grammar specification, Orchid performs code generation to produce a complete Golang package containing an integrated lexer and parser. The generated package provides a clean, single-function API for parsing input that conforms to the specified grammar.

### Key Features

- **Input**: EBNF grammar definition file
- **Output**: Complete Golang package with lexer and parser
- **API**: Simple interface with one entry point: `Parse(input io.Reader) (<package_name>.Node, error)`
- **Integration**: Lexer and parser are combined into a unified package for ease of use

### Design Goals

The tool prioritizes developer productivity by:
- Eliminating boilerplate code for lexical analysis and parsing
- Providing a minimal, idiomatic Go API surface
- Generating self-contained packages that can be easily integrated into larger projects
- Reducing the time from grammar specification to working parser

## Development Standards
- All business logic of the orchid commandline tool should exist within the "internal" package. There is no reason for other people's code to depend on the inner workings of orchid.

## Common Developer Commands
- `make test`: Run all tests

## Important Files


## Project Structure
