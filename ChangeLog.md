## 2020-12-31 Ralph Möritz

- Disassemble with JC64Dis:
    - Fix reference errors due to missing labels
    - Tweak tables for readability
    - Fix bug caused by JC64Dis turning `2c a9 00` (abs) into `bit $00a9` which dasm assembles to `24 a9` (zp). Force absolute addressing with `bit.a $00a9`.

## 2023-09-08 Ralph Möritz

- Add some meaningful labels
- Add listing to dist subdir
