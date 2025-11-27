use python_instruction_dsl_proc::define_opcodes;

define_opcodes!(
    NOP = 0 (value, unused[oparg] --)
);
