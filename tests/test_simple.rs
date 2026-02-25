use pyc_editor::traits::StackEffectTrait;
use pyc_editor::utils::StackEffect;
use pyc_editor::v310::ext_instructions::ExtInstruction;
use pyc_editor::v310::instructions::Instruction;
use python_instruction_dsl_proc::define_opcodes;

define_opcodes!(
    NOP = 0 (value, unused[oparg] -- array[if oparg != 0 {1} else {0}]),
    SPECIALIZED = 1 ( / ),
    SWAP = 2 (item_to_swap, unused[oparg-2], tos -- tos, unused[oparg-2], item_to_swap),
);
