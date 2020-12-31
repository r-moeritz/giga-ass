# About

This is the re-assembled source code for *Giga-Ass*, an assembler by Thomas Dachsel, first published in a special issue of the German magazine *64'er* in 1988. 

## Modifications

See [ChangeLog](./ChangeLog.md) for details.

# Build instructions

## Prerequisites

- [dasm assembler](https://dasm-assembler.github.io)
- [cartconv utility from VICE emulator](https://vice-emu.sourceforge.io)
- [GNU make](https://www.gnu.org/software/make)

## Building

1. Type `make`
1. That's all :grin:

The output are a [PRG file](./dist/giga-ass.prg) and a [CRT cartridge image](./dist/giga-ass.crt) that can be started directly via `make run` (requires [VICE](https://vice-emu.sourceforge.io)), both to be found in the [dist](./dist/) sub-directory.

# Usage instructions

*TODO*
