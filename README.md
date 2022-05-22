# Universal Turing Machine
An implementation of an universal Truing machine in Haskell

## Getting started
run

1. `stack build`
2. `stack exec UniversalTuringMachine-exe`

to execute the program

The current configuration is a multiplying Turing Machine, which is configured in 
[Lib.hs](./src/Lib.hs) : `multiplierConfiguration`

You can change it and encode your own turing machine.

## Turing Machine encoding
The turing machine is encoded binary as followed:
- Transitions are seperated by `11`
- The initial state is `1`
- Transition encoding: `<stateIn>`1`<tapeIn>`1`<stateOut>`1`tapeOut`1`headDirection`
- stateIn / stateOut: An positive integer, starting with 1, unary encoded
  - eg: 2 => `00`
- tapeIn / tapeOut: The value read / written, unary encoded:
  - 0 => `0`
  - 1 => `00`
  - BLANK => `000`
- headDirection: The direction, in which the head should move on the tape, unary encoded
  - LEFT => `0`
  - RIGHT => `00`
