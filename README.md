# Python Instruction DSL proc macro

This repository is for a Rust proc macro that allows you to define Python opcodes with their stack effects using the Domain Specific Language (DSL) syntax. The syntax is described [here](https://github.com/python/cpython/blob/main/Tools/cases_generator/interpreter_definition.md).

It only supports a subset of the full syntax. This is meant to be used by the [pyc-editor](https://github.com/Svenskithesource/pyc-editor) library to define Python opcodes in a concise manner.

## Custom modification

For the purpose of the `pyc-editor` library, you may also use `( / )` to indicate this opcode is not supported. (used for specified opcodes)

You can also have `*EXCEPTION ( -- exception )` to specify what values are pushed to the stack when an exception is raised (for 3.11+). 
