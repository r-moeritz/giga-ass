;****************************
;  Giga-Ass by Thomas Dachsel
;
;  Disassembled by Ralph Moeritz
;  using JC64dis  by Ice Team
;
;  https://github.com/ice00/jc64
;  
;****************************
      processor 6502

      .org $8000

      .word RESET
      .word NMI
      .byte $C3, $C2, $CD, "80" ; CBM80
      .word INIT
T_MNEMONIC:
      .byte "CPX", "CPY", "LDX", "LDY" 
      .byte "CMP", "ADC", "AND", "DEC"
      .byte "EOR", "INC", "LDA", "ASL"
      .byte "BIT", "LSR", "ORA", "ROL"
      .byte "ROR", "SBC", "STA", "STX"
      .byte "STY", "JMP", "JSR", "TXA"
      .byte "TAX", "TYA", "TAY", "TSX"
      .byte "TXS", "PHP", "PLP", "PHA"
      .byte "PLA", "BRK", "RTI", "RTS"
      .byte "NOP", "CLC", "SEC", "CLI"
      .byte "SEI", "CLV", "CLD", "SED"
      .byte "DEY", "INY", "DEX", "INX"
      .byte "BPL", "BMI", "BVC", "BVS"
      .byte "BCC", "BCS", "BNE", "BEQ"
T_OPCODES:
      .byte $E4, $C4, $A6, $A4
      .byte $C5, $65, $25, $C6 
      .byte $45, $E6, $A5, $06
      .byte $24, $46, $05, $26
      .byte $66, $E5, $85, $86
      .byte $84, $4C, $20, $8A
      .byte $AA, $98, $A8, $BA
      .byte $9A, $08, $28, $48
      .byte $68, $00, $40, $60
      .byte $EA, $18, $38, $58
      .byte $78, $B8, $D8, $F8
      .byte $88, $C8, $CA, $E8
      .byte $10, $30, $50, $70
      .byte $90, $B0, $D0, $F0
T_MNEMXFORM1:
      .byte $40, $40, $54, $68, $7B, $7B, $7B, $28
      .byte $7B, $28, $7B, $A8, $00, $A8, $7B, $A8
      .byte $A8, $7B, $3B, $04, $08
T_MNEMXFORM2:
      .byte $0C, $FC, $10, $10, $14, $18, $04, $04
T_MNEMXFORM3:
      .byte $02, $02, $02, $02, $03, $03, $02
T_MNEMXFORM4:
      .byte $01 
T_BINARYOPS:
      .byte $2B, $2D, $2A, $2F, $5E
      .byte $41, $4F, $3E, $3D, $3C
T_ERRMSG1:
      .byte "TERM EVALUATIO", $CE
T_ERRMSG2:
      .byte "TOKE", $CE
T_ERRMSG3:
      .byte "INDEXIN", $C7
T_ERRMSG4:
      .byte "LINE FORMA", $D4
T_ERRMSG5:
      .byte "ADDRESSIN", $C7
T_ERRMSG6:
      .byte "BRANC", $C8
T_ERRMSG7:
      .byte "UNDEFINED SYMBO", $CC
T_ERRMSG8:
      .byte "ILLEGAL SYMBO", $CC
T_ERRMSG9:
      .byte "SYMBOL TABLE FUL", $CC
T_ERRMSG10:
      .byte "NO MACRO TO CLOS", $C5
T_ERRMSG11:
      .byte "DOUBLE LABE", $CC
T_ERRMSG12:
      .byte "PARAMETE", $D2
T_ERRMSG13:
      .byte "RETUR", $CE
T_ERRMSG14:
      .byte "UNDEFINED MACR", $CF
T_ERRMSG15:
      .byte "MACRO NOT CLOSE", $C4
T_ERRMSG16:
      .byte "IF-ELSE-ENDI", $C6
T_ERRMSG17:
      .byte ".BASE MISSIN", $C7
T_EMSGADRLO:
      .byte #<T_ERRMSG1
T_EMSGADRHI:
      .byte #>T_ERRMSG1
      .word T_ERRMSG2, T_ERRMSG3, T_ERRMSG4, T_ERRMSG5
      .word T_ERRMSG6, T_ERRMSG7, T_ERRMSG8, T_ERRMSG9
      .word T_ERRMSG10, T_ERRMSG11, T_ERRMSG12, T_ERRMSG13
      .word T_ERRMSG14, T_ERRMSG15, T_ERRMSG16, T_ERRMSG17
T_PSEUDOOPS:
      .byte "CAL", $CC
      .byte "MACR", $CF
      .byte "ENDMACR", $CF
      .byte "GLOBA", $CC
      .byte "EQUAT", $C5
      .byte "BYT", $C5
      .byte "WOR", $C4
      .byte "D", $D3
      .byte "TEX", $D4
      .byte "OBJEC", $D4
      .byte "BAS", $C5
      .byte "COD", $C5
      .byte "O", $CE
      .byte "GOT", $CF
      .byte "I", $C6
      .byte "ELS", $C5
      .byte "ENDI", $C6
      .byte "SYMBOL", $D3
      .byte "LISTIN", $C7
      .byte "EN", $C4
      .byte "STO", $D0
      .byte "PAG", $C5
      .byte "NOCOD", $C5
      .byte "STAR", $D4
      .byte "NOEX", $D0
V_PSEUDOOPS:
      .word $9079, $9053, $9152, $8E09
      .word $8E18, $8E9A, $8F2E, $8F9E
      .word $8EC0, $8FE0, $8E69, $8E78
      .word $917A, $9193, $9199, $91A6
      .word $91CC, $9421, $9331, $903E
      .word $946B, $9369, $949E, $94A5
      .word $94B1
T_EDICMD:
      .byte "MVLSADNETPFRI@QOBGCXY"
V_EDICMD:
      .word $995A, $998F, $9992, $999F
      .word $974E, $97D7, $98F2, $9873
      .word $9F07, $988D, $99A8, $9B32
      .word $9C49, $9CBD, $9F23, $9E12
      .word $9E2C, $9EEC, $84A9, $8BFD
      .word $9D09
T_STARTMSG:
      .byte $93, $0D
      .byte "  *** GIGA-ASS (C) MARKT & TECHNIK ***", $0D, $0D
      .byte "  BY THOMAS DACHSEL   ", $00, $0D
      .byte "ASSEMBLY BEGINS", $0D, $0D, $00, $0D
      .byte "END OF ASSEMBLY", $0D, $00
      .byte "PAGE ", $00
      .byte ": LINE ", $00
      .byte "PASS ", $00, $0D
      .byte "ASSEMBLY TIME USED", $00
      .byte "OBJECT RANGE $", $00 
      .byte " - $", $00
      .byte "LINE #  LOC    CODE       LINE", $00, $0D
      .byte "SYMBOL TABLE:", $0D, $00, $0D
      .byte "ARE YOU SURE? ", $00
      .byte " BYTES USED      ", $00, $0D
      .byte "DISK ERROR: ", $00
      .byte "GLOBAL", $00
      .byte " MACRO", $00, $0D
      .byte "SOURCE TEXT  USES ", $00
      .byte "OBJECT CODE  USES ", $00
      .byte "SYMBOL TABLE USES ", $00
      .byte " BYTES", $00, $0D
      .byte "AVAILABLE MEMORY: $", $00, $0D
      .byte "SOURCE TEXT USES: $", $00
      .byte " K)", $0D, $00, $0D
      .byte "<SPACE> FOR .START OR <RUN/STOP>", $00, $0D
      .byte "GIGA-ASS READY", $0D, $00
RESET:
      sei                               
      jsr  $FDA3                        
      jsr  $FD50                        ; Routine RAMTAS of KERNAL
      jsr  $FD15                        ; Routine RESTOR of KERNAL
W84A7:
      jsr  $FF5B                        ; Routine CINT of KERNAL
INIT:
      sei                               
      lda  #$00                         
      ldx  #$80                         
      stx  $23                          ; Utility programs pointers area
      sta  $22                          ; Utility programs pointers area
      ldy  #$00                         
W84B5:
      lda  ($22),y                      ; Utility programs pointers area
      sta  ($22),y                      ; Utility programs pointers area
      iny                               
      bne  W84B5                        
      inc  $23                          ; Utility programs pointers area
      ldx  $23                          ; Utility programs pointers area
      cpx  #$A0                         
      bcc  W84B5                        
      lda  #$71                         
      ldx  #$9F                         
W84C8:
      stx  $0315                        ; Vector: Hardware Interrupt (IRQ)
      sta  $0314                        ; Vector: Hardware Interrupt (IRQ)
      cli                               
      lda  #$00                         
      ldx  #$80                         
      stx  $0284                        ; Pointer: Memory top for  Operative System
      sta  $0283                        ; Pointer: Memory top for  Operative System
      jsr  $E453                        ; Routine: Set BASIC vectors (case 0x300..case 0x309)
      jsr  $E3BF                        ; Routine: Set USR instruction and memory for BASIC
      lda  #$44                         
      ldx  #$85                         
      stx  $0301                        ; Vector: Write BASIC error messages
      sta  $0300                        ; Vector: Write BASIC error messages
      lda  #$BF                         
      ldx  #$94                         
      stx  $0303                        ; Vector: BASIC start up
      sta  $0302                        ; Vector: BASIC start up
      lda  #$D2                         
      ldx  #$8B                         
      stx  $0309                        ; Vector: BASIC chars sending
      sta  $0308                        ; Vector: BASIC chars sending
      lda  #$C1                         
      ldx  #$85                         
      stx  $030B                        ; Vector: BASIC "token" valutation
      sta  $030A                        ; Vector: BASIC "token" valutation
      lda  #$0A                         
      sta  $033E                        ; Tape I/O buffer
      lda  #$18                         
      sta  $033F                        ; Tape I/O buffer
      lda  #$FF                         
      sta  $0360                        ; Tape I/O buffer
      sta  $0370                        ; Tape I/O buffer
      lda  #$EB                         
      ldy  #$82                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $37                          ; Pointer: BASIC ending memory
      sec                               
      sbc  $2B                          ; Pointer: BASIC starting programs
      tax                               
      lda  $38                          ; Pointer: BASIC ending memory
      sbc  $2C                          ; Pointer: BASIC starting programs
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$66                         
      ldy  #$E4                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jsr  $A644                        ; BASIC ROM
W8537:
      lda  #$8C                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
W853E:
      jsr  W94BA                        
      jmp  $A47B                        ; BASIC ROM

      lda  $02                          
      cmp  #$02                         
      bne  W8557                        
W854A:
      lda  $033B                        ; Not used
      ldy  $033C                        ; Tape I/O buffer
      sta  $2B                          ; Pointer: BASIC starting programs
      sty  $2C                          ; Pointer: BASIC starting programs
      txa                               
      beq  W853E                        
W8557:
      txa                               
      bmi  W8537                        
      asl                               
      tax                               
      lda  $A326,x                      ; BASIC ROM
      sta  $22                          ; Utility programs pointers area
      lda  $A327,x                      ; BASIC ROM
      sta  $23                          ; Utility programs pointers area
      bne  W8574                        
W8568:
      asl                               
      tax                               
      lda  T_EMSGADRLO,x                      
      sta  $22                          ; Utility programs pointers area
      lda  T_EMSGADRHI,x                      
      sta  $23                          ; Utility programs pointers area
W8574:
      lda  #$0E                         
      jsr  $FFC3                        ; Routine: Close a specified logical file
      jsr  $FFCC                        ; Routine: Close the input and output channel
      lda  #$00                         
      sta  $13                          ; Flag: INPUT request
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      jsr  $AB45                        ; Routine: Introduces '?' in canal+ errors and char
      ldy  #$00                         
W8588:
      lda  ($22),y                      ; Utility programs pointers area
      php                               
      and  #$7F                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      iny                               
      plp                               
      bpl  W8588                        
      jsr  $A67A                        ; BASIC ROM
      lda  #$6A                         
      ldy  #$A3                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      ldy  $3A                          ; BASIC current line number
      iny                               
      beq  W8537                        
      jsr  $BDC2                        ; Routine: Write 'IN' + basic line
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  $02FE                        ; Not used
      ldx  $02FF                        ; Not used
      sta  $5F                          ; Scratch for numeric operation
      stx  $60                          ; Scratch for numeric operation
      jsr  W9A44                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  #$91                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  $A47B                        ; BASIC ROM

      lda  #$00                         
      sta  $0D                          ; Data type: case 0xFF=Stringa, case 0x00=Numerico
W85C5:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bcs  W85D0                        
      jsr  $BCF3                        ; BASIC ROM
      jmp  W865D                        

W85D0:
      cmp  #$2A                         
      bne  W85E1                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      ldy  $FB                          ; Free 0 page for user program
      lda  $FC                          
W85DB:
      jsr  W88A8                        
      jmp  W865D                        

W85E1:
      jsr  $B113                        ; Routine: Verify if the char in A is in 'A'..'Z'
      bcc  W85EC                        
      jsr  INSYMTBL                        
      jmp  W865D                        

W85EC:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      bpl  W85F4                        
      jmp  $AEB1                        ; BASIC ROM

W85F4:
      cmp  #$25                         
      beq  W8623                        
      cmp  #$24                         
      beq  W8620                        
      cmp  #$2D                         
      beq  W861A                        
      cmp  #$22                         
      beq  W861D                        
      cmp  #$21                         
      beq  W8647                        
      cmp  #$2B                         
      beq  W85C5                        
      cmp  #$3E                         
      beq  W868E                        
      cmp  #$3C                         
      beq  W868E                        
      jsr  $AEF1                        ; Routine: Evaluates expression within brackets
      jmp  W865D                        

W861A:
      jmp  $AF0D                        ; Routine: Calculates NOT

W861D:
      jmp  $AEBD                        ; BASIC ROM

W8620:
      jmp  W8870                        

W8623:
      lda  #$00                         
      sta  $3C                          ; BASIC precedent line number
      sta  $3D                          ; Pointer: BASIC instruction for CONT
W8629:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      tax                               
      bne  W8636                        
W862F:
      ldy  $3C                          ; BASIC precedent line number
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      jmp  W85DB                        

W8636:
      bcs  W862F                        
      and  #$FE                         
      cmp  #$30                         
      bne  W8658                        
      txa                               
      lsr                               
      rol  $3C                          ; BASIC precedent line number
      rol  $3D                          ; Pointer: BASIC instruction for CONT
      jmp  W8629                        

W8647:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$4E                         
      bne  W8658                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$21                         
      bne  W8658                        
      jmp  $AED0                        ; BASIC ROM

W8658:
      lda  #$00                         
      jmp  W8568                        

W865D:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      tax                               
      bpl  W8664                        
      rts                               

W8664:
      pla                               
      pla                               
      txa                               
      cmp  #$21                         
      bne  W8677                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      tax                               
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$21                         
      bne  W8658                        
      txa                               
W8677:
      ldx  #$0A                         
W8679:
      cmp  T_MNEMXFORM4,x                      
      beq  W8683                        
      dex                               
      bne  W8679                        
      beq  W8687                        
W8683:
      txa                               
      clc                               
      adc  #$A9                         
W8687:
      ldx  #$00                         
      stx  $4D                          ; Accumulator for the simbols compare
      jmp  $ADBB                        ; BASIC ROM

W868E:
      pha                               
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $AEF1                        ; Routine: Evaluates expression within brackets
      jsr  $B7F7                        ; Routine: Converts FAC in 2-byte integer to $14, $15 and YA
      tax                               
      pla                               
      cmp  #$3C                         
      beq  W86A0                        
      txa                               
      tay                               
W86A0:
      jsr  $B3A2                        ; Routine: Convert the byte in Y to Floating Point
      jmp  W865D                        

W86A6:
      lda  #$00                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$23                         
      beq  W8720                        
      cmp  #$28                         
      beq  W86D5                        
      cmp  #$26                         
      bne  W872A                        
      jsr  W924C                        
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      bne  W8700                        
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      cmp  #$58                         
      beq  W86CF                        
      lda  #$59                         
      jsr  $AEFF                        ; Routine: Verify if A is in current char (Syntax)
      jmp  W8773                        

W86CF:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jmp  W8710                        

W86D5:
      jsr  W924C                        
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      bne  W86F0                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      beq  W8705                        
      cmp  #$29                         
      bne  W8700                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      beq  W8715                        
      bne  W86F3                        
W86F0:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W86F3:
      lda  $41                          ; Pointer: DATA current element address
      cmp  #$15                         
      bne  W8700                        
      lda  #$09                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jmp  W8773                        

W8700:
      lda  #$04                         
      jmp  W8568                        

W8705:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      lda  #$58                         
      jsr  $AEFF                        ; Routine: Verify if A is in current char (Syntax)
      jsr  $AEF7                        ; Routine: Verify if there's ')' in current char (Syntax)
W8710:
      inc  $3E                          ; Pointer: BASIC instruction for CONT
      jmp  W8773                        

W8715:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      lda  #$59                         
      jsr  $AEFF                        ; Routine: Verify if A is in current char (Syntax)
      jmp  W8773                        

W8720:
      lda  #$06                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jsr  W925A                        
      jmp  W8773                        

W872A:
      lda  #$07                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      beq  W876E                        
      cmp  #$3B                         
      beq  W876E                        
      ldx  #$05                         
      stx  $3E                          ; Pointer: BASIC instruction for CONT
      jsr  W924F                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      bne  W8767                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      tax                               
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      txa                               
      cmp  #$58                         
      beq  W875C                        
      cmp  #$59                         
      beq  W875A                        
      lda  #$02                         
      jmp  W8568                        

W875A:
      dec  $3E                          ; Pointer: BASIC instruction for CONT
W875C:
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      bne  W8773                        
      dec  $3E                          ; Pointer: BASIC instruction for CONT
      dec  $3E                          ; Pointer: BASIC instruction for CONT
      jmp  W8773                        

W8767:
      lda  #$0A                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jmp  W875C                        

W876E:
      lda  #$07                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
W8772:
      rts                               

W8773:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      beq  W8772                        
      cmp  #$3B                         
      beq  W8772                        
      lda  #$03                         
      jmp  W8568                        

W8781:
      ldx  $3E                          ; Pointer: BASIC instruction for CONT
      cpx  #$06                         
      bne  W8793                        
      lda  $41                          ; Pointer: DATA current element address
      cmp  #$04                         
      bcs  W8793                        
      lda  $3B                          ; BASIC precedent line number
      sbc  #$07                         
      sta  $3B                          ; BASIC precedent line number
W8793:
      cpx  #$09                         
      beq  W87C0                        
      lda  $41                          ; Pointer: DATA current element address
      cmp  #$15                         
      bcc  W87D2                        
      beq  W87B3                        
      cmp  #$16                         
      beq  W87B3                        
      cmp  #$30                         
      bcc  W87AA                        
      jmp  W882E                        

W87AA:
      cpx  #$07                         
      bne  W87CD                        
      lda  #$01                         
      sta  $42                          ; Pointer: DATA current element address
      rts                               

W87B3:
      cpx  #$08                         
      beq  W87BB                        
      cpx  #$0A                         
      bne  W87CD                        
W87BB:
      lda  #$03                         
      sta  $42                          ; Pointer: DATA current element address
      rts                               

W87C0:
      lda  $41                          ; Pointer: DATA current element address
      cmp  #$15                         
      bne  W87CD                        
      lda  #$6C                         
      sta  $3B                          ; BASIC precedent line number
      jmp  W87BB                        

W87CD:
      lda  #$04                         
      jmp  W8568                        

W87D2:
      lda  #$02                         
      sta  $42                          ; Pointer: DATA current element address
      cpx  #$08                         
      beq  W87E7                        
      cpx  #$0A                         
      bne  W87E8                        
      inc  $42                          ; Pointer: DATA current element address
      lda  $3B                          ; BASIC precedent line number
      clc                               
      adc  #$08                         
      sta  $3B                          ; BASIC precedent line number
W87E7:
      rts                               

W87E8:
      lda  #$01                         
W87EA:
      cpx  #$00                         
      beq  W87F3                        
      asl                               
      dex                               
      jmp  W87EA                        

W87F3:
      ldx  $41                          ; Pointer: DATA current element address
      and  T_MNEMXFORM1,x                      
      bne  W880B                        
      ldx  $3E                          ; Pointer: BASIC instruction for CONT
      cpx  #$02                         
      beq  W8804                        
      cpx  #$03                         
      bne  W87CD                        
W8804:
      inx                               
      inx                               
      stx  $3E                          ; Pointer: BASIC instruction for CONT
      jmp  W87E8                        

W880B:
      ldx  $3E                          ; Pointer: BASIC instruction for CONT
      cpx  #$04                         
      bne  W8820                        
      lda  $41                          ; Pointer: DATA current element address
      cmp  #$02                         
      bne  W8820                        
      lda  #$03                         
      sta  $42                          ; Pointer: DATA current element address
      lda  #$BE                         
      sta  $3B                          ; BASIC precedent line number
      rts                               

W8820:
      lda  T_MNEMXFORM3,x                      
      sta  $42                          ; Pointer: DATA current element address
      lda  T_MNEMXFORM2,x                      
      clc                               
      adc  $3B                          ; BASIC precedent line number
      sta  $3B                          ; BASIC precedent line number
W882D:
      rts                               

W882E:
      jsr  W87B3                        
      dec  $42                          ; Pointer: DATA current element address
      lda  $FD                          
      cmp  #$02                         
      bcc  W882D                        
      lda  $3C                          ; BASIC precedent line number
      ldx  $3D                          ; Pointer: BASIC instruction for CONT
      sec                               
      sbc  $FB                          ; Free 0 page for user program
      tay                               
      txa                               
      sbc  $FC                          
      bcc  W885D                        
      tax                               
      tya                               
      sbc  #$02                         
      bcs  W884D                        
      dex                               
W884D:
      sta  $3C                          ; BASIC precedent line number
      cpx  #$00                         
      bne  W8858                        
      cmp  #$80                         
      bcs  W8858                        
      rts                               

W8858:
      lda  #$05                         
      jmp  W8568                        

W885D:
      tax                               
      tya                               
      sec                               
      sbc  #$02                         
      bcs  W8865                        
      dex                               
W8865:
      sta  $3C                          ; BASIC precedent line number
      cpx  #$FF                         
      bne  W8858                        
      cmp  #$80                         
      bcc  W8858                        
      rts                               

W8870:
      ldx  #$0A                         
      lda  #$00                         
W8874:
      sta  $5D,x                        ; Scratch for numeric operation
      dex                               
      bne  W8874                        
W8879:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bcc  W8889                        
      jsr  $B113                        ; Routine: Verify if the char in A is in 'A'..'Z'
      bcc  W889A                        
      sbc  #$07                         
      cmp  #$40                         
      bcs  W889A                        
W8889:
      sbc  #$2F                         
      jsr  $BD7E                        ; BASIC ROM
      lda  $61                          ; Floating point accumulator #1: Exponent
      clc                               
      adc  #$04                         
      sta  $61                          ; Floating point accumulator #1: Exponent
      bcc  W8879                        
      jmp  $B97E                        ; Routine: Write (OVERFLOW) error

W889A:
      lda  $61                          ; Floating point accumulator #1: Exponent
      sbc  #$03                         
      bcc  W88A5                        
      sta  $61                          ; Floating point accumulator #1: Exponent
      jmp  W865D                        

W88A5:
      jmp  $AF08                        ; Routine: Write (SYNTAX) error

W88A8:
      sta  $62                          ; Floating point accumulator #1: Mantissa
      sty  $63                          ; Floating point accumulator #1: Mantissa
W88AC:
      ldx  #$90                         
      sec                               
      jmp  $BC49                        ; BASIC ROM

INSYMTBL:
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      ldx  $7B                          ; CHRGET (Introduce a char) subroutine
      sta  $49                          ; Pointer: variable for the FOR..NEXT
      stx  $4A                          ; Pointer: variable for the FOR..NEXT
GETSYM:
      ldx  #$00                         
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      jsr  $B113                        ; Routine: Verify if the char in A is in 'A'..'Z'
      bcs  W88CD                        
      lda  #$20                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
ILLSYM:     
      lda  #$07                         
      jmp  W8568                        

W88CD:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bcc  W88D7                        
      jsr  $B113                        ; Routine: Verify if the char in A is in 'A'..'Z'
      bcc  GETDELIM                        
W88D7:
      inx                               
      jmp  W88CD                        

GETDELIM:
      cmp  #$5F                         
      beq  W88D7                        
      inx                               
      stx  $45                          ; BASIC current variable name
      lda  #$36                         
      sta  $01                          ; 6510 I/O register
ISSYMDEFINED:      
      lda  #$F9                         
      ldx  #$BF                         
W88EA:
      stx  $60                          ; Scratch for numeric operation
W88EC:
      sta  $5F                          ; Scratch for numeric operation
      cpx  $30                          ; Pointer: BASIC starting arrays
      bcc  ISSYMONSTACK                        
      bne  W88F8                        
      cmp  $2F                          ; Pointer: BASIC starting arrays
      bcc  ISSYMONSTACK                        
W88F8:
      ldy  #$01                         
      lda  ($5F),y                      ; Scratch for numeric operation
      cmp  #$FF                         
      beq  W8927                        
      cmp  #$FE                         
      bne  W891B                        
      pla                               
      tay                               
      pla                               
      pha                               
      cmp  #$90                         
      beq  W8911                        
      tya                               
      pha                               
W890E:
      jmp  W8948                        

W8911:
      tya                               
      pha                               
      cmp  #$8B                         
      bne  W890E                        
      ldy  #$02                         
      bne  W8928                        
W891B:
      cmp  $8C                          ; Real value of the RND seed
      bne  W8948                        
      dey                               
      lda  ($5F),y                      ; Scratch for numeric operation
      cmp  $8B                          ; Real value of the RND seed
      bne  W8948                        
      iny                               
W8927:
      iny                               
W8928:
      lda  ($5F),y                      ; Scratch for numeric operation
      cmp  $45                          ; BASIC current variable name
      bne  W8948                        
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $47                          ; Pointer: BASIC current variable data
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $48                          ; Pointer: BASIC current variable data
      ldy  #$00                         
W893A:
      lda  ($49),y                      ; Pointer: variable for the FOR..NEXT
      cmp  ($47),y                      ; Pointer: BASIC current variable data
      bne  W8948                        
      iny                               
      cpy  $45                          ; BASIC current variable name
      bne  W893A                        
      jmp  FOUNDSYM                        

W8948:
      lda  $5F                          ; Scratch for numeric operation
      sec                               
      sbc  #$07                         
      ldx  $60                          ; Scratch for numeric operation
      bcs  W88EC                        
      dex                               
      jmp  W88EA                        

ISSYMONSTACK:
      pla                               
      tax                               
      pla                               
      cmp  #$85                         
      bne  W8965                        
      pha                               
      txa                               
      cmp  #$E8                         
      bne  W8975                        
      pha                               
      beq  SYMUNDEF                        
W8965:
      cmp  #$90                         
      beq  W896F                        
      pha                               
      txa                               
      pha                               
      jmp  SAVESYM                        

W896F:
      pha                               
      txa                               
      cmp  #$8B                         
      beq  MACROUNDEF                        
W8975:
      pha                               
      jmp  SAVESYM                        

MACROUNDEF:
      pha                               
      jsr  W89E0                        
      lda  #$0D                         
      jmp  W8568                        

SYMUNDEF:
      jsr  W89E0                        
      lda  $FD                          
      cmp  #$02                         
      bcc  W8990                        
      lda  #$06                         
      jmp  W8568                        

W8990:
      lda  $3E                          ; Pointer: BASIC instruction for CONT
      cmp  #$06                         
      bne  W899D                        
      lda  #$00                         
      ldy  #$80                         
      jmp  W88A8                        

W899D:
      lda  #$80                         
      ldy  #$00                         
      jmp  W88A8                        

SAVESYM:
      lda  $5F                          ; Scratch for numeric operation
      ldy  $60                          ; Scratch for numeric operation
      cpy  #$A0                         
      bcc  SYMTBLFULL                        
      sta  $2F                          ; Pointer: BASIC starting arrays
      sty  $30                          ; Pointer: BASIC starting arrays
      ldy  #$00                         
      lda  $8B                          ; Real value of the RND seed
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      lda  $8C                          ; Real value of the RND seed
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      lda  $45                          ; BASIC current variable name
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      lda  $49                          ; Pointer: variable for the FOR..NEXT
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      lda  $4A                          ; Pointer: variable for the FOR..NEXT
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      lda  #$FF                         
      sta  ($5F),y                      ; Scratch for numeric operation
      iny                               
      sta  ($5F),y                      ; Scratch for numeric operation
W89D2:
      lda  $5F                          ; Scratch for numeric operation
      ldy  $60                          ; Scratch for numeric operation
      clc                               
      adc  #$05                         
      bcc  W89DC                        
      iny                               
W89DC:
      sta  $49                          ; Pointer: variable for the FOR..NEXT
      sty  $4A                          ; Pointer: variable for the FOR..NEXT
W89E0:
      lda  #$37                         
      sta  $01                          ; 6510 I/O register
      sec                               
      rts                               

FOUNDSYM:
      jsr  W89D2                        
      lda  #$36                         
      sta  $01                          ; 6510 I/O register
      ldy  #$01                         
      lda  ($49),y                      ; Pointer: variable for the FOR..NEXT
      sta  $62                          ; Floating point accumulator #1: Mantissa
      dey                               
      lda  ($49),y                      ; Pointer: variable for the FOR..NEXT
      sta  $63                          ; Floating point accumulator #1: Mantissa
      cmp  #$FF                         
      bne  PUTSYMVINFAC                        
      lda  $62                          ; Floating point accumulator #1: Mantissa
      cmp  #$FF                         
      bne  PUTSYMVINFAC                        
      lda  #$20                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
      lda  #$37                         
      sta  $01                          ; 6510 I/O register
      lda  #$06                         
      jmp  W8568                        

PUTSYMVINFAC:
      sty  $70                          ; Lo Byte #1 (rounding)
      jsr  W89E0                        
      jsr  W88AC                        
      clc                               
      rts                               

SYMTBLFULL:
      jsr  W89E0                        
      lda  #$20                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
      lda  #$08                         
      jmp  W8568                        

W8A25:
      ldy  #$02                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W8A4A                        
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      sta  $39                          ; BASIC current line number
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      sta  $3A                          ; BASIC current line number
      ldy  $7B                          ; CHRGET (Introduce a char) subroutine
      ldx  $7A                          ; CHRGET (Introduce a char) subroutine
      inx                               
      bne  W8A3D                        
      iny                               
W8A3D:
      stx  $02FE                        ; Not used
      sty  $02FF                        ; Not used
      ldy  #$04                         
      jsr  $A8FB                        ; BASIC ROM
      clc                               
      rts                               

W8A4A:
      sec                               
      rts                               

W8A4C:
      lda  $FD                          
      cmp  #$02                         
      bcs  W8A55                        
      jmp  W8BB4                        

W8A55:
      jsr  $FFCC                        ; Routine: Close the input and output channel
      lda  #$FF                         
      sta  $3A                          ; BASIC current line number
      lda  #$3F                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $02F9                        ; Not used
      beq  W8A6B                        
      jsr  W9442                        
W8A6B:
      lda  $02F8                        ; Not used
      bne  W8A76                        
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
W8A76:
      jsr  W92E0                        
      lda  #$79                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $02EC                        ; Not used
      clc                               
      adc  $02EE                        ; Not used
      tax                               
      lda  $02ED                        ; Not used
      adc  $02EF                        ; Not used
      jsr  W9379                        
      txa                               
      jsr  W9379                        
      lda  #$88                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jsr  W931F                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  #$FA                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $2D                          ; Pointer: BASIC starting variables
      sec                               
      sbc  $2B                          ; Pointer: BASIC starting programs
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  $2E                          ; Pointer: BASIC starting variables
      sbc  $2C                          ; Pointer: BASIC starting programs
      sta  $62                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
      lda  #$34                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$28                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $2D                          ; Pointer: BASIC starting variables
      sec                               
      sbc  $2B                          ; Pointer: BASIC starting programs
      lda  $2E                          ; Pointer: BASIC starting variables
      sbc  $2C                          ; Pointer: BASIC starting programs
      lsr                               
      lsr                               
      tax                               
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$65                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$0E                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $FB                          ; Free 0 page for user program
      sec                               
      sbc  $02EC                        ; Not used
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  $FC                          
      sbc  $02ED                        ; Not used
      sta  $62                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
      lda  #$34                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$28                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $FB                          ; Free 0 page for user program
      sec                               
      sbc  $02EC                        ; Not used
      lda  $FC                          
      sbc  $02ED                        ; Not used
      lsr                               
      lsr                               
      tax                               
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$65                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$21                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$00                         
      sec                               
      sbc  $2F                          ; Pointer: BASIC starting arrays
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  #$C0                         
      sta  $8F                          ; Real value of the RND seed
      sbc  $30                          ; Pointer: BASIC starting arrays
      sta  $62                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
      lda  #$34                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$28                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $8E                          ; Real value of the RND seed
      sec                               
      sbc  $2F                          ; Pointer: BASIC starting arrays
      lda  $8F                          ; Real value of the RND seed
      sbc  $30                          ; Pointer: BASIC starting arrays
      lsr                               
      lsr                               
      tax                               
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$65                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $02F7                        ; Not used
      jsr  $FFC3                        ; Routine: Close a specified logical file
      jsr  $FFCC                        ; Routine: Close the input and output channel
      lda  $02EB                        ; Not used
      cmp  #$FF                         
      beq  W8BB1                        
      lda  #$6A                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      ldy  #$00                         
      sty  $CC                          ; Flash state: 0=flashing
W8B83:
      lda  $CB                          ; Flag: write chars with SHIFT pressed
      cmp  #$40                         
      beq  W8B83                        
      ldx  #$01                         
      stx  $CC                          ; Flash state: 0=flashing
      cmp  #$3F                         
      bne  W8B99                        
      lda  #$20                         
      ldy  $D3                          ; Column of cursor on the current line
      sta  ($D1),y                      ; Pointer: current screen line address
      bne  W8BB1                        
W8B99:
      cmp  #$3C                         
      bne  W8B83                        
      sty  $C6                          ; Number of char in keyboard buffer
      lda  #$93                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $02EA                        ; Not used
      ldx  $02EB                        ; Not used
      sta  $14                          ; Transient: integer value
      stx  $15                          ; Transient: integer value
      jsr  $E130                        
W8BB1:
      jmp  $E37B                        ; Routine (from case 0xA002): close canals, start up

W8BB4:
      lda  $02ED                        ; Not used
      cmp  #$FF                         
      bne  W8BC0                        
      lda  #$10                         
      jmp  W8568                        

W8BC0:
      jsr  W92CA                        
      jmp  W8CC0                        

W8BC6:
      ldy  #$00                         
      sty  $8B                          ; Real value of the RND seed
      sty  $8C                          ; Real value of the RND seed
      sty  $8E                          ; Real value of the RND seed
      iny                               
      sty  $8D                          ; Real value of the RND seed
      rts                               

      bit  $9D                          ; Flag: 80=direct mode 00=program mode
      bpl  W8BFE                        
      lda  #$00                         
      sta  $3E                          ; Pointer: BASIC instruction for CONT
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bne  W8BE2                        
      jmp  W94BF                        

W8BE2:
      sbc  #$80                         
      bcs  W8BE9                        
W8BE6:
      jmp  $AF08                        ; Routine: Write (SYNTAX) error

W8BE9:
      cmp  #$0A                         
      beq  W8BFE                        
      cmp  #$13                         
      bcc  W8BE6                        
      cmp  #$1B                         
      bne  W8BF8                        
      jmp  W9874                        

W8BF8:
      jsr  $A7F3                        ; BASIC ROM
      jmp  $A7AE                        ; Routine: Interpreter loop, set up next statement for execution

W8BFE:
      jsr  $FFE7                        ; Routine: Close all canals and files
      lda  #$05                         
      ldx  #$08                         
      stx  $7B                          ; CHRGET (Introduce a char) subroutine
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$9E                         
      bne  W8C19                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $E12A                        ; Routine: SYS instruction
      jmp  $E37B                        ; Routine (from case 0xA002): close canals, start up

W8C19:
      lda  #$2C                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$2F                         
      sta  $00                          
      lda  $DC0E                        ; Control register A of CIA #1
      ora  #$80                         
      sta  $DC0E                        ; Control register A of CIA #1
      lda  #$08                         
      sta  $DC0F                        ; Control register B of CIA #1
      jsr  $A68E                        ; Routine: Set program pointer to BASIC-start (loads $7A, $7B with $2B-1, $2C-1)
      lda  #$FF                         
      sta  $02ED                        ; Not used
      sta  $02EB                        ; Not used
      lda  #$00                         
      sta  $02EE                        ; Not used
      sta  $02EF                        ; Not used
      lda  #$00                         
      ldx  #$C0                         
      stx  $30                          ; Pointer: BASIC starting arrays
      sta  $2F                          ; Pointer: BASIC starting arrays
      pha                               
      sta  $DC0B                        ; Day time clock #1: Hour+[indicator AM/PM]
      sta  $DC0A                        ; Day time clock #1: Minutes
      sta  $DC09                        ; Day time clock #1: Second
      sta  $DC08                        ; Day time clock #1: 1/10 second
      sta  $FD                          
      sta  $02E8                        ; Not used
      sta  $FE                          ; Free 0 page for user program
      sta  $02F9                        ; Not used
      sta  $02FA                        ; Not used
      sta  $02FB                        ; Not used
      sta  $02E7                        ; Not used
      jsr  W8BC6                        
      sty  $02F8                        ; Not used
W8C72:
      jsr  W8A25                        
      bcc  W8C84                        
      lda  $02FA                        ; Not used
      cmp  $02FB                        ; Not used
      beq  W8CC0                        
      lda  #$0E                         
      jmp  W8568                        

W8C84:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$A1                         
      beq  W8C9F                        
      cmp  #$A2                         
      bne  W8CB7                        
      inc  $02FB                        ; Not used
      lda  $02FA                        ; Not used
      cmp  $02FB                        ; Not used
      bcs  W8CB7                        
      lda  #$09                         
      jmp  W8568                        

W8C9F:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
      ldy  #$00                         
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      sta  ($49),y                      ; Pointer: variable for the FOR..NEXT
      iny                               
      lda  $7B                          ; CHRGET (Introduce a char) subroutine
      sta  ($49),y                      ; Pointer: variable for the FOR..NEXT
      lda  #$FE                         
      sta  ($5F),y                      ; Scratch for numeric operation
      inc  $02FA                        ; Not used
W8CB7:
      jsr  $A909                        ; Routine: Search 0 char in string (position)
      jsr  $A8FB                        ; BASIC ROM
      jmp  W8C72                        

W8CC0:
      inc  $FD                          
      lda  #$5F                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $FD                          
      ora  #$30                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  W9484                        
      jsr  W92E7                        
      jsr  W8BC6                        
      jsr  $A68E                        ; Routine: Set program pointer to BASIC-start (loads $7A, $7B with $2B-1, $2C-1)
W8CDC:
      jsr  W93CD                        
W8CDF:
      lda  $02E7                        ; Not used
      beq  W8CFB                        
      lda  $02FA                        ; Not used
      cmp  $02FB                        ; Not used
      beq  W8CF3                        
      lda  $02F8                        ; Not used
      ora  #$02                         
      bne  W8CF8                        
W8CF3:
      lda  $02F8                        ; Not used
      and  #$01                         
W8CF8:
      sta  $02F8                        ; Not used
W8CFB:
      jsr  $A82C                        ; BASIC ROM
      jsr  W8A25                        
      bcc  W8D09                        
      jmp  W8A4C                        

W8D06:
      jmp  W8DC5                        

W8D09:
      lda  $02F8                        ; Not used
      bne  W8D40                        
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      pha                               
      lda  $7B                          ; CHRGET (Introduce a char) subroutine
      pha                               
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      tax                               
      pla                               
      sta  $7B                          ; CHRGET (Introduce a char) subroutine
      pla                               
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      cpx  #$A2                         
      beq  W8D40                        
      lda  $02FE                        ; Not used
      ldx  $02FF                        ; Not used
      sta  $5F                          ; Scratch for numeric operation
      stx  $60                          ; Scratch for numeric operation
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W8D37                        
      jmp  $E0F9                        

W8D37:
      jsr  W9A18                        
      jsr  W947C                        
      jsr  $FFCC                        ; Routine: Close the input and output channel
W8D40:
      ldy  #$01                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      bpl  W8D4F                        
      inc  $7A                          ; CHRGET (Introduce a char) subroutine
      bne  W8D4C                        
      inc  $7B                          ; CHRGET (Introduce a char) subroutine
W8D4C:
      jmp  W8DDC                        

W8D4F:
      cmp  #$3B                         
      beq  W8D06                        
      cmp  #$20                         
      beq  W8D99                        
      lda  $FD                          
      cmp  #$01                         
      beq  W8D6C                        
W8D5D:
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W8DB1                        
      cmp  #$20                         
      bne  W8D5D                        
      jsr  $A8FB                        ; BASIC ROM
      jmp  W8D9C                        

W8D6C:
      lda  #$FF                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
      lda  #$20                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
      bcs  W8D89                        
      lda  $4A                          ; Pointer: variable for the FOR..NEXT
      cmp  #$C0                         
      bcc  W8D84                        
      bne  W8D89                        
W8D84:
      lda  #$0A                         
      jmp  W8568                        

W8D89:
      lda  $FC                          
      ldx  $FB                          ; Free 0 page for user program
      jsr  W92D5                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      tax                               
      bne  W8D9F                        
      jmp  W8DBC                        

W8D99:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W8D9C:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
W8D9F:
      jsr  W9281                        
W8DA2:
      ldy  #$00                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W8DBC                        
      cmp  #$3B                         
      beq  W8DBC                        
      lda  #$03                         
      jmp  W8568                        

W8DB1:
      lda  $02F8                        ; Not used
      bne  W8DB9                        
      jsr  W9489                        
W8DB9:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W8DBC:
      jsr  $A909                        ; Routine: Search 0 char in string (position)
      jsr  $A8FB                        ; BASIC ROM
      jmp  W8CDC                        

W8DC5:
      lda  $02F8                        ; Not used
      bne  W8DB9                        
      jsr  W9489                        
      jsr  W93D3                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W8DD3:
      jsr  $A909                        ; Routine: Search 0 char in string (position)
      jsr  $A8FB                        ; BASIC ROM
      jmp  W8CDF                        

W8DDC:
      tax                               
      and  #$E0                         
      cmp  #$A0                         
      bne  W8DEA                        
      txa                               
      and  #$1F                         
      cmp  #$19                         
      bcc  W8DEF                        
W8DEA:
      lda  #$01                         
      jmp  W8568                        

W8DEF:
      asl                               
      tax                               
      lda  $827B,x                      
      pha                               
      lda  V_PSEUDOOPS,x                      
      pha                               
      cpx  #$12                         
      bcs  W8E01                        
      cpx  #$04                         
      bcs  W8E09                        
W8E01:
      lda  $02F8                        ; Not used
      bne  W8E09                        
      jmp  W9489                        

W8E09:
      rts                               

      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
      lda  #$FF                         
      ldy  #$01                         
      sta  ($5F),y                      ; Scratch for numeric operation
W8E16:
      jmp  W8E1F                        

      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
W8E1F:
      lda  #$3D                         
      jsr  $AEFF                        ; Routine: Verify if A is in current char (Syntax)
      lda  $4A                          ; Pointer: variable for the FOR..NEXT
      pha                               
      lda  $49                          ; Pointer: variable for the FOR..NEXT
      pha                               
      jsr  W924F                        
      pla                               
      sta  $49                          ; Pointer: variable for the FOR..NEXT
      pla                               
      sta  $4A                          ; Pointer: variable for the FOR..NEXT
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      ldx  $3C                          ; BASIC precedent line number
      jsr  W92D5                        
      lda  $02F8                        ; Not used
      bne  W8E67                        
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W8E4A                        
      jmp  $E0F9                        

W8E4A:
      ldx  #$07                         
      jsr  W9496                        
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W8E5A                        
      jmp  $E0F9                        

W8E5A:
      lda  #$23                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$3D                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  W93AB                        
W8E67:
      jmp  W8DA2                        

      jsr  W924C                        
      sta  $02ED                        ; Not used
      sty  $02EC                        ; Not used
      jsr  W92CA                        
      jmp  W8DA2                        

      lda  $02ED                        ; Not used
      cmp  #$FF                         
      bne  W8E85                        
      lda  #$10                         
      jmp  W8568                        

W8E85:
      jsr  W924C                        
      tax                               
      tya                               
      sec                               
      sbc  $02EC                        ; Not used
      sta  $02EE                        ; Not used
      txa                               
      sbc  $02ED                        ; Not used
      sta  $02EF                        ; Not used
      jmp  W8DA2                        

      lda  #$01                         
      sta  $42                          ; Pointer: DATA current element address
W8E9F:
      jsr  W925A                        
      sta  $3B                          ; BASIC precedent line number
      jsr  W8F6E                        
      jsr  W91E5                        
      lda  $02F8                        ; Not used
      bne  W8EB7                        
      jsr  W93D3                        
      lda  #$80                         
      sta  $02F8                        ; Not used
W8EB7:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      beq  W8E9F                        
      jmp  W8F54                        

      ldy  #$00                         
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$22                         
      beq  W8ECD                        
W8ECA:
      jmp  $AF08                        ; Routine: Write (SYNTAX) error

W8ECD:
      ldx  #$00                         
W8ECF:
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W8F07                        
      cmp  #$5F                         
      bne  W8EDA                        
      lda  #$0D                         
W8EDA:
      cmp  #$22                         
      beq  W8F01                        
      sta  $3B,x                        ; BASIC precedent line number
      inx                               
      cpx  #$03                         
      bne  W8ECF                        
      stx  $42                          ; Pointer: DATA current element address
      tya                               
      pha                               
      jsr  W8F6E                        
      jsr  W91E5                        
      lda  $02F8                        ; Not used
      bne  W8EFC                        
      jsr  W93D3                        
      lda  #$80                         
      sta  $02F8                        ; Not used
W8EFC:
      pla                               
      tay                               
      jmp  W8ECD                        

W8F01:
      iny                               
      lda  #$00                         
      sta  $3B,x                        ; BASIC precedent line number
      inx                               
W8F07:
      txa                               
      bne  W8F10                        
      cpy  #$01                         
      beq  W8ECA                        
      bne  W8F29                        
W8F10:
      stx  $42                          ; Pointer: DATA current element address
      tya                               
      pha                               
      jsr  W8F6E                        
      jsr  W91E5                        
      lda  $02F8                        ; Not used
      bne  W8F27                        
      jsr  W93D3                        
      lda  #$80                         
      sta  $02F8                        ; Not used
W8F27:
      pla                               
      tay                               
W8F29:
      jsr  $A8FB                        ; BASIC ROM
W8F2C:
      jmp  W8F54                        

      lda  #$02                         
      sta  $42                          ; Pointer: DATA current element address
W8F33:
      jsr  W924C                        
      sta  $3C                          ; BASIC precedent line number
      sty  $3B                          ; BASIC precedent line number
      jsr  W8F6E                        
      jsr  W91E5                        
      lda  $02F8                        ; Not used
      bne  W8F4D                        
      jsr  W93D3                        
      lda  #$80                         
      sta  $02F8                        ; Not used
W8F4D:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      beq  W8F33                        
W8F54:
      lda  $02F8                        ; Not used
      and  #$01                         
      sta  $02F8                        ; Not used
      ldy  #$00                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W8F6B                        
      cmp  #$3B                         
      beq  W8F6B                        
      lda  #$03                         
      jmp  W8568                        

W8F6B:
      jmp  W8DD3                        

W8F6E:
      ldx  $FD                          
      dex                               
      beq  W8F9E                        
      lda  $02F8                        ; Not used
      bne  W8F7B                        
      jmp  W9390                        

W8F7B:
      bpl  W8F9E                        
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W8F88                        
      jmp  $E0F9                        

W8F88:
      ldx  #$08                         
      jsr  W9496                        
      jsr  W9390                        
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W8F9B                        
      jmp  $E0F9                        

W8F9B:
      jmp  W93EB                        

W8F9E:
      rts                               

      jsr  W924C                        
      cmp  #$00                         
      beq  W8FA9                        
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

W8FA9:
      sta  $42                          ; Pointer: DATA current element address
      cpy  #$00                         
      beq  W8F54                        
      sta  $3B                          ; BASIC precedent line number
      dey                               
      inc  $42                          ; Pointer: DATA current element address
      cpy  #$00                         
      beq  W8FC6                        
      sta  $3C                          ; BASIC precedent line number
      dey                               
      inc  $42                          ; Pointer: DATA current element address
      cpy  #$00                         
      beq  W8FC6                        
      sta  $3D                          ; Pointer: BASIC instruction for CONT
      dey                               
      inc  $42                          ; Pointer: DATA current element address
W8FC6:
      tya                               
      pha                               
      jsr  W8F6E                        
      jsr  W91E5                        
      lda  $02F8                        ; Not used
      bne  W8FDB                        
      jsr  W93D3                        
      lda  #$80                         
      sta  $02F8                        ; Not used
W8FDB:
      pla                               
      tay                               
      lda  #$00                         
      beq  W8FA9                        
      ldx  $FD                          
      dex                               
      bne  W8FED                        
      lda  #$80                         
      sta  $FE                          ; Free 0 page for user program
      jmp  W8DBC                        

W8FED:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      ldx  $7A                          ; CHRGET (Introduce a char) subroutine
      ldy  $7B                          ; CHRGET (Introduce a char) subroutine
      inx                               
      bne  W8FF8                        
      iny                               
W8FF8:
      stx  $BB                          ; Pointer: current file name
      sty  $BC                          ; Pointer: current file name
      ldy  #$00                         
W8FFE:
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      cmp  #$22                         
      bne  W8FFE                        
      dey                               
      sty  $B7                          ; Length of current file name
      lda  #$0E                         
      ldy  #$6E                         
      ldx  #$08                         
      jsr  $FFBA                        ; Routine: Set primary, secondary and logical addresses
      jsr  $FFC0                        ; Routine: Open a logical file
      bcc  W9019                        
      jmp  $E0F9                        

W9019:
      ldx  #$0E                         
      jsr  $FFC9                        ; Routine: Open an output canal
      lda  $FB                          ; Free 0 page for user program
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $FC                          
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  $FFCC                        ; Routine: Close the input and output channel
      lda  $90                          ; Statusbyte ST of I/O KERNAL
      bne  W9032                        
      jmp  W8DBC                        

W9032:
      jsr  $FFE7                        ; Routine: Close all canals and files
      lda  #$DE                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jmp  W9CCA                        

      lda  $FD                          
      cmp  #$02                         
      bcc  W9051                        
      lda  #$00                         
      sta  $FE                          ; Free 0 page for user program
      lda  #$0E                         
      jsr  $FFC3                        ; Routine: Close a specified logical file
      jsr  $FFCC                        ; Routine: Close the input and output channel
W9051:
      jmp  W8DBC                        

W9054:
      jsr  $A909                        ; Routine: Search 0 char in string (position)
      jsr  $A8FB                        ; BASIC ROM
      ldy  #$04                         
      jsr  $A8FB                        ; BASIC ROM
      lda  $7B                          ; CHRGET (Introduce a char) subroutine
      cmp  $2E                          ; Pointer: BASIC starting variables
      bcc  W9070                        
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      cmp  $2D                          ; Pointer: BASIC starting variables
      bcc  W9070                        
      lda  #$0E                         
      jmp  W8568                        

W9070:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$A2                         
      bne  W9054                        
      jmp  W8DBC                        

      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W907D:
      inc  $02FA                        ; Not used
      lda  $8B                          ; Real value of the RND seed
      ora  $8C                          ; Real value of the RND seed
      sta  $8F                          ; Real value of the RND seed
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
      lda  $7B                          ; CHRGET (Introduce a char) subroutine
      pha                               
      sta  $02FD                        ; Not used
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      pha                               
      sta  $02FC                        ; Not used
      lda  #$36                         
      sta  $01                          ; 6510 I/O register
      pha                               
      ldy  #$01                         
      lda  ($49),y                      ; Pointer: variable for the FOR..NEXT
      sta  $7B                          ; CHRGET (Introduce a char) subroutine
      dey                               
      lda  ($49),y                      ; Pointer: variable for the FOR..NEXT
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      lda  #$37                         
      sta  $01                          ; 6510 I/O register
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$A0                         
      bne  W9119                        
      jsr  W91D0                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$A0                         
      bne  W911F                        
W90BD:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$22                         
      bne  W90CD                        
      jsr  W926A                        
      lda  #$00                         
      sta  $3D                          ; Pointer: BASIC instruction for CONT
      beq  W90D0                        
W90CD:
      jsr  W924F                        
W90D0:
      jsr  W91D0                        
      jsr  W9124                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  INSYMTBL                        
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      ldx  $3C                          ; BASIC precedent line number
      jsr  W92D5                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$00                         
      beq  W9102                        
      cmp  #$3B                         
      beq  W9102                        
      cmp  #$2C                         
      bne  W911F                        
      jsr  W91D0                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$2C                         
      bne  W911F                        
      jsr  W913F                        
      jmp  W90BD                        

W9102:
      jsr  W91D0                        
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      cmp  #$00                         
      beq  W9110                        
      cmp  #$3B                         
      bne  W911F                        
W9110:
      jsr  W91D0                        
W9113:
      jsr  W9138                        
      jmp  W8DBC                        

W9119:
      jsr  W9124                        
      jmp  W9113                        

W911F:
      lda  #$0B                         
      jmp  W8568                        

W9124:
      lda  $8F                          ; Real value of the RND seed
      bne  W9131                        
      lda  $8D                          ; Real value of the RND seed
      ldx  $8E                          ; Real value of the RND seed
      sta  $8B                          ; Real value of the RND seed
      stx  $8C                          ; Real value of the RND seed
      rts                               

W9131:
      inc  $8B                          ; Real value of the RND seed
      bne  W9137                        
      inc  $8C                          ; Real value of the RND seed
W9137:
      rts                               

W9138:
      inc  $8D                          ; Real value of the RND seed
      bne  W913E                        
      inc  $8E                          ; Real value of the RND seed
W913E:
      rts                               

W913F:
      lda  $8F                          ; Real value of the RND seed
      bne  W914A                        
      lda  #$00                         
      sta  $8B                          ; Real value of the RND seed
      sta  $8C                          ; Real value of the RND seed
      rts                               

W914A:
      lda  $8B                          ; Real value of the RND seed
      bne  W9150                        
      dec  $8C                          ; Real value of the RND seed
W9150:
      dec  $8B                          ; Real value of the RND seed
      rts                               

      inc  $02FB                        ; Not used
      pla                               
      cmp  #$36                         
      bne  W9176                        
      pla                               
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      pla                               
      sta  $7B                          ; CHRGET (Introduce a char) subroutine
      pla                               
      pha                               
      cmp  #$36                         
      bne  W916D                        
      jsr  W914A                        
      jmp  W8DD3                        

W916D:
      lda  #$00                         
      sta  $8B                          ; Real value of the RND seed
      sta  $8C                          ; Real value of the RND seed
      jmp  W8DD3                        

W9176:
      lda  #$0C                         
W9178:
      jmp  W8568                        

      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $AD8A                        ; Routine: FRMNUM: Get expression (FRMEVL) and check, if numeric
      lda  $61                          ; Floating point accumulator #1: Exponent
      beq  W9191                        
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  W924F                        
W918B:
      jsr  $A8A3                        ; BASIC ROM
      jmp  W8CDC                        

W9191:
      jmp  W8DBC                        

      jsr  W924C                        
      jmp  W918B                        

      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $AD8A                        ; Routine: FRMNUM: Get expression (FRMEVL) and check, if numeric
      lda  $61                          ; Floating point accumulator #1: Exponent
      beq  W91A7                        
      jmp  W8DA2                        

W91A7:
      jsr  $A909                        ; Routine: Search 0 char in string (position)
      iny                               
      iny                               
      iny                               
      iny                               
      jsr  $A8FB                        ; BASIC ROM
      lda  $7B                          ; CHRGET (Introduce a char) subroutine
      cmp  $2E                          ; Pointer: BASIC starting variables
      bcc  W91C2                        
      lda  $7A                          ; CHRGET (Introduce a char) subroutine
      cmp  $2D                          ; Pointer: BASIC starting variables
      bcc  W91C2                        
      lda  #$0F                         
      jmp  W8568                        

W91C2:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$B0                         
      beq  W91CD                        
      cmp  #$AF                         
      bne  W91A7                        
W91CD:
      jmp  W8DBC                        

W91D0:
      ldx  $7A                          ; CHRGET (Introduce a char) subroutine
      ldy  $7B                          ; CHRGET (Introduce a char) subroutine
      lda  $02FC                        ; Not used
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      lda  $02FD                        ; Not used
      sta  $7B                          ; CHRGET (Introduce a char) subroutine
      stx  $02FC                        ; Not used
      sty  $02FD                        ; Not used
      rts                               

W91E5:
      ldx  $FD                          
      dex                               
      beq  W9217                        
      lda  $FE                          ; Free 0 page for user program
      bmi  W9223                        
      bne  W9217                        
      lda  $FB                          ; Free 0 page for user program
      clc                               
      adc  $02EE                        ; Not used
      sta  $22                          ; Utility programs pointers area
      lda  $FC                          
      adc  $02EF                        ; Not used
      sta  $23                          ; Utility programs pointers area
      ldx  $42                          ; Pointer: DATA current element address
      ldy  #$00                         
      lda  $3B                          ; BASIC precedent line number
      sta  ($22),y                      ; Utility programs pointers area
      dex                               
      beq  W9217                        
      iny                               
      lda  $3C                          ; BASIC precedent line number
      sta  ($22),y                      ; Utility programs pointers area
      dex                               
      beq  W9217                        
      iny                               
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      sta  ($22),y                      ; Utility programs pointers area
W9217:
      lda  $42                          ; Pointer: DATA current element address
      clc                               
      adc  $FB                          ; Free 0 page for user program
      sta  $FB                          ; Free 0 page for user program
      bcc  W9222                        
      inc  $FC                          
W9222:
      rts                               

W9223:
      ldx  #$0E                         
      jsr  $FFC9                        ; Routine: Open an output canal
      ldx  $42                          ; Pointer: DATA current element address
      lda  $3B                          ; BASIC precedent line number
      jsr  $FFD2                        ; Routine: Send a char in the channel
      dex                               
      beq  W923F                        
      lda  $3C                          ; BASIC precedent line number
W9234:
      jsr  $FFD2                        ; Routine: Send a char in the channel
      dex                               
      beq  W923F                        
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      jmp  W9234                        

W923F:
      jsr  $FFCC                        ; Routine: Close the input and output channel
      lda  $90                          ; Statusbyte ST of I/O KERNAL
      beq  W9249                        
      jmp  W9032                        

W9249:
      jmp  W9217                        

W924C:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W924F:
      jsr  $AD8A                        ; Routine: FRMNUM: Get expression (FRMEVL) and check, if numeric
      jsr  $B7F7                        ; Routine: Converts FAC in 2-byte integer to $14, $15 and YA
      sta  $3D                          ; Pointer: BASIC instruction for CONT
      sty  $3C                          ; BASIC precedent line number
      rts                               

W925A:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      cmp  #$22                         
      beq  W926A                        
      jsr  W924F                        
      cmp  #$00                         
      bne  W927E                        
      tya                               
      rts                               

W926A:
      ldy  #$01                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      sta  $3C                          ; BASIC precedent line number
      iny                               
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      cmp  #$22                         
      bne  W927E                        
      iny                               
      jsr  $A8FB                        ; BASIC ROM
      lda  $3C                          ; BASIC precedent line number
      rts                               

W927E:
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

W9281:
      ldy  #$00                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      cmp  #$C0                         
      bcs  W92A1                        
      cmp  #$80                         
      bcs  W929A                        
      pla                               
      pla                               
      lda  $02F8                        ; Not used
      bne  W9297                        
      jsr  W9489                        
W9297:
      jmp  W907D                        

W929A:
      tax                               
      pla                               
      pla                               
      txa                               
      jmp  W8DDC                        

W92A1:
      cmp  #$F8                         
      bcc  W92AA                        
      lda  #$01                         
      jmp  W8568                        

W92AA:
      and  #$3F                         
      sta  $41                          ; Pointer: DATA current element address
      tax                               
      lda  T_OPCODES,x                      
      sta  $3B                          ; BASIC precedent line number
      jsr  W86A6                        
      jsr  W8781                        
      ldx  $FD                          
      dex                               
      beq  W92C7                        
      lda  $02F8                        ; Not used
      bne  W92C7                        
      jsr  W9390                        
W92C7:
      jmp  W91E5                        

W92CA:
      lda  $02EC                        ; Not used
      ldx  $02ED                        ; Not used
      sta  $FB                          ; Free 0 page for user program
      stx  $FC                          
      rts                               

W92D5:
      ldy  #$01                         
      sta  ($49),y                      ; Pointer: variable for the FOR..NEXT
      dey                               
      txa                               
      sta  ($49),y                      ; Pointer: variable for the FOR..NEXT
      sty  $70                          ; Lo Byte #1 (rounding)
      rts                               

W92E0:
      lda  #$65                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
W92E7:
      jsr  W9484                        
      lda  $DC08                        ; Day time clock #1: 1/10 second
      lda  $DC0B                        ; Day time clock #1: Hour+[indicator AM/PM]
      lda  $DC0A                        ; Day time clock #1: Minutes
      jsr  W9307                        
      lda  $DC09                        ; Day time clock #1: Second
      jsr  W9307                        
      lda  $DC08                        ; Day time clock #1: 1/10 second
      ora  #$30                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  $AAD7                        ; Routine: Write return effect and/or advancement

W9307:
      tax                               
      lsr                               
      lsr                               
      lsr                               
      lsr                               
      ora  #$30                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      txa                               
      and  #$0F                         
      ora  #$30                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$3A                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      rts                               

W931F:
      lda  $FB                          ; Free 0 page for user program
      clc                               
      adc  $02EE                        ; Not used
      tax                               
      lda  $FC                          
      adc  $02EF                        ; Not used
      jsr  W9379                        
      txa                               
      jmp  W9379                        

      ldx  $FD                          
      dex                               
      beq  W9364                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $E219                        
      lda  $B8                          ; Current logical file number
      sta  $02F7                        ; Not used
      jsr  $FFC0                        ; Routine: Open a logical file
      bcs  W9367                        
      lda  #$00                         
      sta  $02F8                        ; Not used
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W9357                        
      jmp  $E0F9                        

W9357:
      lda  #$8D                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      jsr  $FFCC                        ; Routine: Close the input and output channel
W9364:
      jmp  W8DD3                        

W9367:
      jmp  $E0F9                        

      jsr  W924C                        
      tya                               
      ora  #$80                         
      sta  $02E8                        ; Not used
      sta  $02E9                        ; Not used
      jmp  W8DA2                        

W9379:
      pha                               
      lsr                               
      lsr                               
      lsr                               
      lsr                               
      jsr  W9384                        
      pla                               
      and  #$0F                         
W9384:
      clc                               
      adc  #$F6                         
      bcc  W938B                        
      adc  #$06                         
W938B:
      adc  #$3A                         
      jmp  $FFD2                        ; Routine: Send a char in the channel

W9390:
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W939B                        
      jmp  $E0F9                        

W939B:
      jsr  W931F                        
      jsr  W947C                        
      ldx  $42                          ; Pointer: DATA current element address
      lda  $3B                          ; BASIC precedent line number
      jsr  W9379                        
      dex                               
      beq  W93C1                        
W93AB:
      jsr  W9484                        
      lda  $3C                          ; BASIC precedent line number
      jsr  W9379                        
      dex                               
      beq  W93C4                        
      jsr  W9484                        
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      jsr  W9379                        
      jmp  W93C7                        

W93C1:
      jsr  W947C                        
W93C4:
      jsr  W947C                        
W93C7:
      jsr  W947C                        
      jmp  $FFCC                        ; Routine: Close the input and output channel

W93CD:
      lda  $02F8                        ; Not used
      beq  W93D3                        
      rts                               

W93D3:
      lda  $02FE                        ; Not used
      ldx  $02FF                        ; Not used
      sta  $5F                          ; Scratch for numeric operation
      stx  $60                          ; Scratch for numeric operation
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W93E8                        
      jmp  $E0F9                        

W93E8:
      jsr  W9A47                        
W93EB:
      bit  $02E8                        ; Not used
      bpl  W941C                        
      dec  $02E9                        ; Not used
      bmi  W941C                        
      lda  $02E8                        ; Not used
      sta  $02E9                        ; Not used
      lda  $BA                          ; Current device number
      cmp  #$03                         
      bne  W9410                        
      jsr  W9B1F                        
      lda  #$00                         
      sta  $D4                          ; Flag: Editor mode "quotation", case 0x00=NO
      lda  #$93                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  W9415                        

W9410:
      lda  #$0C                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
W9415:
      lda  #$8D                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
W941C:
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      jmp  $FFCC                        ; Routine: Close the input and output channel

      ldx  $FD                          
      dex                               
      beq  W943C                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $E219                        
      ldx  #$05                         
W942F:
      lda  $B7,x                        ; Length of current file name
      sta  $02F0,x                      ; Not used
      dex                               
      bpl  W942F                        
      lda  #$FF                         
      sta  $02F9                        ; Not used
W943C:
      jmp  W8DBC                        

W943F:
      jmp  $E0F9                        

W9442:
      ldx  #$05                         
W9444:
      lda  $02F0,x                      ; Not used
      sta  $B7,x                        ; Length of current file name
      dex                               
      bpl  W9444                        
      jsr  $FFC0                        ; Routine: Open a logical file
      bcs  W943F                        
      ldx  $02F1                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcs  W943F                        
      lda  #$AC                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jsr  W9D1F                        
      lda  $02F1                        ; Not used
      jsr  $FFC3                        ; Routine: Close a specified logical file
      jmp  $FFCC                        ; Routine: Close the input and output channel

      ldx  $FD                          
      dex                               
      beq  W943C                        
      jsr  W93CD                        
      lda  #$0E                         
      jsr  $FFC3                        ; Routine: Close a specified logical file
      jmp  W8A4C                        

W947C:
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  $FFD2                        ; Routine: Send a char in the channel
W9484:
      lda  #$20                         
      jmp  $FFD2                        ; Routine: Send a char in the channel

W9489:
      ldx  $02F7                        ; Not used
      jsr  $FFC9                        ; Routine: Open an output canal
      bcc  W9494                        
      jmp  $E0F9                        

W9494:
      ldx  #$12                         
W9496:
      jsr  W9484                        
      dex                               
      bne  W9496                        
      jmp  $FFCC                        ; Routine: Close the input and output channel

      lda  #$01                         
      sta  $FE                          ; Free 0 page for user program
      jmp  W8DBC                        

      jsr  W924C                        
      sta  $02EB                        ; Not used
      sty  $02EA                        ; Not used
      jmp  W8DA2                        

      lda  #$01                         
      sta  $02E7                        ; Not used
      jmp  W8DBC                        

W94BA:
      lda  #$00                         
      sta  $02                          
      rts                               

W94BF:
      lda  #$FF                         
      sta  $3A                          ; BASIC current line number
      ldx  $02                          
      beq  W94D3                        
      bmi  W953E                        
      dex                               
      dex                               
      beq  W9535                        
      dex                               
      beq  W9538                        
      dex                               
      beq  W953B                        
W94D3:
      jsr  $A560                        ; Routine: Read 89 input chars of BASIC command
      stx  $7A                          ; CHRGET (Introduce a char) subroutine
      sty  $7B                          ; CHRGET (Introduce a char) subroutine
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      tax                               
      beq  W94D3                        
      cmp  #$2F                         
      bne  W94ED                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $AAA0                        ; Routine: PRINT instruction
      jmp  W8537                        

W94ED:
      cmp  #$5F                         
      bne  W9515                        
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
W94F4:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      bcs  W94FC                        
      jmp  W98E2                        

W94FC:
      ldx  #$15                         
W94FE:
      cmp  T_EDICMD,x                      
      beq  W9509                        
      dex                               
      bpl  W94FE                        
      jmp  $AF08                        ; Routine: Write (SYNTAX) error

W9509:
      txa                               
      asl                               
      tax                               
      lda  $82C2,x                      
      pha                               
      lda  V_EDICMD,x                      
      pha                               
      rts                               

W9515:
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      bcs  W951D                        
      jmp  W95C0                        

W951D:
      cmp  #$40                         
      beq  W94F4                        
      cmp  #$3F                         
      beq  W952F                        
      ldy  #$01                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W94F4                        
      cmp  #$41                         
      bcc  W94F4                        
W952F:
      jsr  $A579                        ; Routine: BASIC Text "tokenized" (from case 0x0304)
      jmp  $A7E1                        ; BASIC ROM

W9535:
      jmp  W854A                        

W9538:
      jmp  W96E8                        

W953B:
      jmp  W9B8E                        

W953E:
      lda  $02                          
      and  #$7F                         
      sta  $02                          
      lda  #$91                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      ldx  #$00                         
W954B:
      jsr  W9484                        
      inx                               
      cpx  #$27                         
      bcc  W954B                        
      lda  #$00                         
      sta  $D3                          ; Column of cursor on the current line
      jsr  W9A44                        
      dec  $D6                          ; Cursor line number
      lda  $D5                          ; Fisical screen line length
      pha                               
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      pla                               
      cmp  #$27                         
      bne  W956A                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
W956A:
      jmp  W94BF                        

W956D:
      tya                               
      pha                               
      ldy  #$00                         
      sty  $41                          ; Pointer: DATA current element address
W9573:
      txa                               
      pha                               
W9575:
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      bpl  W958A                        
      and  #$7F                         
      sta  $3B                          ; BASIC precedent line number
      lda  T_PSEUDOOPS,y                      
      and  #$7F                         
      cmp  $3B                          ; BASIC precedent line number
      bne  W959C                        
      beq  W95AF                        
W958A:
      lda  T_PSEUDOOPS,y                      
      php                               
      and  #$7F                         
      cmp  $0200,x                      ; INPUT buffer of BASIC
      bne  W959B                        
      plp                               
      bmi  W95AF                        
      iny                               
      bne  W9575                        
W959B:
      plp                               
W959C:
      lda  T_PSEUDOOPS,y                      
      bmi  W95A4                        
      iny                               
      bne  W959C                        
W95A4:
      pla                               
      tax                               
      iny                               
      cpy  #$73                         
      bcs  W95B9                        
      inc  $41                          ; Pointer: DATA current element address
      bne  W9573                        
W95AF:
      pla                               
      pla                               
      tay                               
      lda  $41                          ; Pointer: DATA current element address
      ora  #$A0                         
      jmp  W969F                        

W95B9:
      pla                               
      tay                               
      lda  #$2E                         
      jmp  W969F                        

W95C0:
      ldx  #$FF                         
      stx  $81                          ; CHRGET (Introduce a char) subroutine
      jsr  $A96B                        ; Routine: Get decimal number (0...63999, usually a line number) from basic text into $14/$15
W95C7:
      lda  #$20                         
      sta  $81                          ; CHRGET (Introduce a char) subroutine
      ldx  $7A                          ; CHRGET (Introduce a char) subroutine
      ldy  #$05                         
      lda  $0200,x                      ; INPUT buffer of BASIC
      sta  $01FB,y                      ; CPU stack
      bpl  W95E4                        
      and  #$BF                         
      ora  #$20                         
      jmp  W969F                        

W95DE:
      lda  $0200,x                      ; INPUT buffer of BASIC
      sta  $01FB,y                      ; CPU stack
W95E4:
      bne  W95E9                        
      jmp  W96D5                        

W95E9:
      cmp  #$3B                         
      bne  W95F0                        
      jmp  W96B8                        

W95F0:
      cmp  #$2E                         
      beq  W961C                        
      cmp  #$20                         
      beq  W95FC                        
      inx                               
      iny                               
      bne  W95DE                        
W95FC:
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      bne  W9605                        
      jmp  W96D1                        

W9605:
      cmp  #$20                         
      beq  W95FC                        
      cmp  #$22                         
      bne  W9610                        
      jmp  W96C0                        

W9610:
      cmp  #$3B                         
      bne  W9617                        
      jmp  W96B4                        

W9617:
      cmp  #$2E                         
      bne  W961F                        
      iny                               
W961C:
      jmp  W956D                        

W961F:
      tya                               
      pha                               
      ldy  #$00                         
      sty  $41                          ; Pointer: DATA current element address
W9625:
      lda  $0200,x                      ; INPUT buffer of BASIC
      cmp  T_MNEMONIC,y                      
      bne  W963D                        
      lda  $0201,x                      ; INPUT buffer of BASIC
      cmp  $800C,y                      
      bne  W963D                        
      lda  $0202,x                      ; INPUT buffer of BASIC
      cmp  $800D,y                      
      beq  W9696                        
W963D:
      inc  $41                          ; Pointer: DATA current element address
      iny                               
      iny                               
      iny                               
      cpy  #$A8                         
      bne  W9625                        
      pla                               
      tay                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      cmp  #$A1                         
      bcs  W966C                        
      cmp  #$A0                         
      bne  W968F                        
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      dex                               
      jsr  $B113                        ; Routine: Verify if the char in A is in 'A'..'Z'
      lda  #$A0                         
      bcc  W969C                        
      bcs  W968F                        
W9661:
      lda  $0200,x                      ; INPUT buffer of BASIC
      beq  W96D1                        
      bpl  W9674                        
      cmp  #$A0                         
      beq  W969E                        
W966C:
      cmp  #$FF                         
      bne  W969C                        
      lda  #$DE                         
      bmi  W969E                        
W9674:
      cmp  #$3B                         
      beq  W96B4                        
      cmp  #$20                         
      bne  W968F                        
W967C:
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      beq  W96D1                        
      cmp  #$20                         
      beq  W967C                        
      cmp  #$3B                         
      beq  W96B4                        
      dex                               
      lda  #$A0                         
      bmi  W969E                        
W968F:
      iny                               
      sta  $01FB,y                      ; CPU stack
      inx                               
      bne  W9661                        
W9696:
      pla                               
      tay                               
      inx                               
      inx                               
      lda  $41                          ; Pointer: DATA current element address
W969C:
      ora  #$C0                         
W969E:
      iny                               
W969F:
      sta  $01FB,y                      ; CPU stack
W96A2:
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      beq  W96D1                        
      cmp  #$20                         
      beq  W96A2                        
      cmp  #$22                         
      beq  W96C0                        
      cmp  #$3B                         
      bne  W969E                        
W96B4:
      iny                               
      sta  $01FB,y                      ; CPU stack
W96B8:
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      bne  W96B4                        
      beq  W96D1                        
W96C0:
      iny                               
      sta  $01FB,y                      ; CPU stack
      inx                               
      lda  $0200,x                      ; INPUT buffer of BASIC
      beq  W96D1                        
      cmp  #$22                         
      bne  W96C0                        
      jmp  W969E                        

W96D1:
      iny                               
      sta  $01FB,y                      ; CPU stack
W96D5:
      dec  $7B                          ; CHRGET (Introduce a char) subroutine
      lda  #$FF                         
      sta  $7A                          ; CHRGET (Introduce a char) subroutine
      cpy  #$06                         
      bcc  W96E5                        
      lda  $02                          
      ora  #$80                         
      sta  $02                          
W96E5:
      jmp  $A4A2                        ; BASIC ROM

W96E8:
      lda  $033D                        ; Tape I/O buffer
      beq  W9749                        
      lda  $02FE                        ; Not used
      cmp  $033B                        ; Not used
      beq  W9702                        
      lda  $033C                        ; Tape I/O buffer
      sta  $62                          ; Floating point accumulator #1: Mantissa
      lda  $033B                        ; Not used
      sta  $63                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
W9702:
      ldx  #$05                         
      sei                               
W9705:
      lda  $0276,x                      ; KERNAL table: Secondary address for each files
      sta  $027B,x                      ; Keyboard buffer queue (FIFO)
      dex                               
      bne  W9705                        
      stx  $D3                          ; Column of cursor on the current line
      ldy  #$04                         
      lda  #$1D                         
W9714:
      sta  $0277,y                      ; Keyboard buffer queue (FIFO)
      inc  $C6                          ; Number of char in keyboard buffer
      dey                               
      bpl  W9714                        
      cli                               
      jsr  $A560                        ; Routine: Read 89 input chars of BASIC command
      stx  $7A                          ; CHRGET (Introduce a char) subroutine
      sty  $7B                          ; CHRGET (Introduce a char) subroutine
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bcs  W9749                        
      ldx  #$FF                         
      stx  $81                          ; CHRGET (Introduce a char) subroutine
      jsr  $A96B                        ; Routine: Get decimal number (0...63999, usually a line number) from basic text into $14/$15
      lda  $033B                        ; Not used
      sta  $02FE                        ; Not used
      cmp  $14                          ; Transient: integer value
      bne  W9746                        
      clc                               
      adc  $033D                        ; Tape I/O buffer
      sta  $033B                        ; Not used
      bcc  W9746                        
      inc  $033C                        ; Tape I/O buffer
W9746:
      jmp  W95C7                        

W9749:
      jsr  W94BA                        
      jmp  W94BF                        

      inc  $7A                          ; CHRGET (Introduce a char) subroutine
      ldy  #$00                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W976C                        
      jsr  $B7EB                        ; Routine: GETADR and GETBYT: Get 16-bit integer (to $14, $15) and an 8 bit value (X)-parameter for WAIT and POKE
      lda  $14                          ; Transient: integer value
      ldy  $15                          ; Transient: integer value
      sta  $033B                        ; Not used
      sty  $033C                        ; Tape I/O buffer
      stx  $033D                        ; Tape I/O buffer
      tax                               
      inx                               
      stx  $02FE                        ; Not used
W976C:
      lda  #$03                         
      sta  $02                          
      jmp  W94BF                        

W9773:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      bcc  W9785                        
      beq  W9785                        
      cmp  #$2D                         
      beq  W9785                        
      cmp  #$AB                         
      beq  W9785                        
W9782:
      jmp  $AF08                        ; Routine: Write (SYNTAX) error

W9785:
      jsr  $A96B                        ; Routine: Get decimal number (0...63999, usually a line number) from basic text into $14/$15
      jsr  $A613                        ; Routine: Calculate start adress of a program line
      jsr  $0079                        ; CHRGET (Introduce a char) subroutine
      beq  W97A0                        
      cmp  #$2D                         
      beq  W9798                        
      cmp  #$AB                         
      bne  W9782                        
W9798:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      jsr  $A96B                        ; Routine: Get decimal number (0...63999, usually a line number) from basic text into $14/$15
      bne  W9782                        
W97A0:
      lda  $14                          ; Transient: integer value
      ora  $15                          ; Transient: integer value
      bne  W97AC                        
      lda  #$FF                         
      sta  $14                          ; Transient: integer value
      sta  $15                          ; Transient: integer value
W97AC:
      rts                               

W97AD:
      ldy  #$01                         
      lda  ($5F),y                      ; Scratch for numeric operation
      beq  $97C6                        
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      tax                               
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      cmp  $15                          ; Transient: integer value
      bne  W97C2                        
      cpx  $14                          ; Transient: integer value
      beq  W97C4                        
W97C2:
      bcs  W97C7                        
W97C4:
      clc                               
      bit  $38                          ; Pointer: BASIC ending memory
W97C7:
      rts                               

W97C8:
      ldy  #$00                         
      lda  ($5F),y                      ; Scratch for numeric operation
      tax                               
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $60                          ; Scratch for numeric operation
      stx  $5F                          ; Scratch for numeric operation
      rts                               

W97D5:
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

      lda  #$BC                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$00                         
      sta  $CC                          ; Flash state: 0=flashing
W97E3:
      lda  $C6                          ; Number of char in keyboard buffer
      beq  W97E3                        
      lda  #$01                         
      sta  $CC                          ; Flash state: 0=flashing
      lda  #$00                         
      sta  $C6                          ; Number of char in keyboard buffer
      lda  $0277                        ; Keyboard buffer queue (FIFO)
      cmp  #$59                         
      beq  W97FE                        
      lda  #$4E                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  W8537                        

W97FE:
      lda  #$59                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  W9773                        
      lda  $5F                          ; Scratch for numeric operation
      ldx  $60                          ; Scratch for numeric operation
      sta  $19                          ; Transient strings stack
      stx  $1A                          
      lda  $14                          ; Transient: integer value
      and  $15                          ; Transient: integer value
      tax                               
      inx                               
      beq  W981C                        
      inc  $14                          ; Transient: integer value
      bne  W981C                        
      inc  $15                          ; Transient: integer value
W981C:
      jsr  $A613                        ; Routine: Calculate start adress of a program line
      lda  $5F                          ; Scratch for numeric operation
      ldx  $60                          ; Scratch for numeric operation
      sta  $24                          ; Utility programs pointers area
      stx  $25                          ; Utility programs pointers area
      sec                               
      sbc  $19                          ; Transient strings stack
      txa                               
      sbc  $1A                          
      bcc  W97D5                        
      lda  $2D                          ; Pointer: BASIC starting variables
      sbc  $24                          ; Utility programs pointers area
      sta  $5F                          ; Scratch for numeric operation
      lda  $2E                          ; Pointer: BASIC starting variables
      sbc  $25                          ; Utility programs pointers area
      sta  $60                          ; Scratch for numeric operation
      clc                               
      lda  $19                          ; Transient strings stack
      adc  $5F                          ; Scratch for numeric operation
      sta  $2D                          ; Pointer: BASIC starting variables
      lda  $1A                          
      adc  $60                          ; Scratch for numeric operation
      sta  $2E                          ; Pointer: BASIC starting variables
      ldy  #$00                         
W984A:
      lda  $5F                          ; Scratch for numeric operation
      sec                               
      sbc  #$01                         
      sta  $5F                          ; Scratch for numeric operation
      lda  $60                          ; Scratch for numeric operation
      sbc  #$00                         
      sta  $60                          ; Scratch for numeric operation
      bmi  W986B                        
      lda  ($24),y                      ; Utility programs pointers area
      sta  ($19),y                      ; Transient strings stack
      inc  $19                          ; Transient strings stack
      bne  W9863                        
      inc  $1A                          
W9863:
      inc  $24                          ; Utility programs pointers area
      bne  W9869                        
      inc  $25                          ; Utility programs pointers area
W9869:
      bne  W984A                        
W986B:
      jsr  $A659                        ; BASIC ROM
      jsr  $A533                        ; Routine: Relink BASIC program
      jmp  W8537                        

W9874:
      jsr  W9773                        
W9877:
      jsr  $A82C                        ; BASIC ROM
      jsr  W97AD                        
      bcs  W988B                        
      jsr  W9A44                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      jsr  W97C8                        
      jmp  W9877                        

W988B:
      jmp  W8537                        

      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      jsr  W98D9                        
      stx  $FE                          ; Free 0 page for user program
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  $AD8A                        ; Routine: FRMNUM: Get expression (FRMEVL) and check, if numeric
      jsr  $B7F7                        ; Routine: Converts FAC in 2-byte integer to $14, $15 and YA
      ldx  $FE                          ; Free 0 page for user program
      sta  $0350,x                      ; Tape I/O buffer
      tya                               
      sta  $0340,x                      ; Tape I/O buffer
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  $AD8A                        ; Routine: FRMNUM: Get expression (FRMEVL) and check, if numeric
      jsr  $B7F7                        ; Routine: Converts FAC in 2-byte integer to $14, $15 and YA
      ldx  $FE                          ; Free 0 page for user program
      sta  $0370,x                      ; Tape I/O buffer
      tya                               
      sta  $0360,x                      ; Tape I/O buffer
      jmp  W8537                        

W98BD:
      lda  $0340,x                      ; Tape I/O buffer
      sta  $14                          ; Transient: integer value
      lda  $0350,x                      ; Tape I/O buffer
      sta  $15                          ; Transient: integer value
      stx  $FE                          ; Free 0 page for user program
      jsr  $A613                        ; Routine: Calculate start adress of a program line
      ldx  $FE                          ; Free 0 page for user program
      lda  $0360,x                      ; Tape I/O buffer
      sta  $14                          ; Transient: integer value
      lda  $0370,x                      ; Tape I/O buffer
      sta  $15                          ; Transient: integer value
W98D8:
      rts                               

W98D9:
      cpx  #$10                         
      bcc  W98D8                        
      pla                               
      pla                               
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

W98E2:
      jsr  $B79E                        ; BASIC ROM
      jsr  W98D9                        
      jsr  W98BD                        
      lda  #$93                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  W9877                        

      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      jsr  W98D9                        
      stx  $FE                          ; Free 0 page for user program
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  $B7EB                        ; Routine: GETADR and GETBYT: Get 16-bit integer (to $14, $15) and an 8 bit value (X)-parameter for WAIT and POKE
      stx  $033D                        ; Tape I/O buffer
      lda  $14                          ; Transient: integer value
      ldx  $15                          ; Transient: integer value
      sta  $033B                        ; Not used
      stx  $033C                        ; Tape I/O buffer
      ldx  $FE                          ; Free 0 page for user program
      jsr  W98BD                        
      lda  $033B                        ; Not used
      sta  $0340,x                      ; Tape I/O buffer
      lda  $033C                        ; Tape I/O buffer
      sta  $0350,x                      ; Tape I/O buffer
W991F:
      jsr  W97AD                        
      bcs  W9941                        
      lda  $033C                        ; Tape I/O buffer
      sta  ($5F),y                      ; Scratch for numeric operation
      dey                               
      lda  $033B                        ; Not used
      sta  ($5F),y                      ; Scratch for numeric operation
      clc                               
      adc  $033D                        ; Tape I/O buffer
      sta  $033B                        ; Not used
      bcc  W993B                        
      inc  $033C                        ; Tape I/O buffer
W993B:
      jsr  W97C8                        
      jmp  W991F                        

W9941:
      lda  $033B                        ; Not used
      sec                               
      sbc  $033D                        ; Tape I/O buffer
      ldx  $FE                          ; Free 0 page for user program
      sta  $0360,x                      ; Tape I/O buffer
      bcs  W9952                        
      dec  $033C                        ; Tape I/O buffer
W9952:
      lda  $033C                        ; Tape I/O buffer
      sta  $0370,x                      ; Tape I/O buffer
W9958:
      jmp  W8537                        

      lda  #$02                         
      sta  $02                          
      lda  $2B                          ; Pointer: BASIC starting programs
      ldx  $2C                          ; Pointer: BASIC starting programs
      sta  $033B                        ; Not used
      stx  $033C                        ; Tape I/O buffer
      lda  #$FF                         
      sta  $14                          ; Transient: integer value
      sta  $15                          ; Transient: integer value
      jsr  $A613                        ; Routine: Calculate start adress of a program line
      lda  $5F                          ; Scratch for numeric operation
      ldx  $60                          ; Scratch for numeric operation
      sta  $2B                          ; Pointer: BASIC starting programs
      stx  $2C                          ; Pointer: BASIC starting programs
      jmp  W9992+1                      

W997D:
      jsr  $0073                        ; CHRGET (Introduce a char) subroutine
      beq  W9986                        
      cmp  #$22                         
      bne  W997D                        
W9986:
      jsr  $E257                        
      ldy  #$00                         
      ldx  #$08                         
W998D:
      jmp  $FFBA                        ; Routine: Set primary, secondary and logical addresses

      lda  #$01                         
W9992:
      bit.a  $00A9                      ; RS-232 indicator: Control of starting bit
      sta  $0A                          ; Flag: 0=LOAD, 1=VERIFY
      jsr  W997D                        
      jsr  $E16F                        
W999D:
      jmp  W8537                        

      jsr  W997D                        
      jsr  $E159                        
      jmp  W8537                        

      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      jsr  W98D9                        
      jsr  W98BD                        
W99B2:
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  W9BEE                        
W99B8:
      jsr  W97AD                        
      bcc  W99C0                        
      jmp  W8537                        

W99C0:
      ldx  $60                          ; Scratch for numeric operation
      lda  $5F                          ; Scratch for numeric operation
      clc                               
      adc  #$04                         
      sta  $5D                          ; Scratch for numeric operation
      bcc  W99CC                        
      inx                               
W99CC:
      stx  $5E                          ; Scratch for numeric operation
      lda  #$03                         
      sta  $45                          ; BASIC current variable name
W99D2:
      ldy  #$FF                         
      inc  $45                          ; BASIC current variable name
W99D6:
      iny                               
      lda  ($5D),y                      ; Scratch for numeric operation
      beq  W99FF                        
      cpy  $BA                          ; Current device number
      beq  W9A03                        
      cmp  $03D8,y                      ; Tape I/O buffer
      beq  W99D6                        
      lda  $03D8,y                      ; Tape I/O buffer
      cmp  #$3F                         
      beq  W99D6                        
W99EB:
      iny                               
      lda  ($5D),y                      ; Scratch for numeric operation
      bne  W99EB                        
      cpy  $BA                          ; Current device number
      beq  W9A12                        
      bcc  W9A12                        
      inc  $5D                          ; Scratch for numeric operation
      bne  W99FC                        
      inc  $5E                          ; Scratch for numeric operation
W99FC:
      jmp  W99D2                        

W99FF:
      cpy  $BA                          ; Current device number
      bne  W9A12                        
W9A03:
      lda  $02                          
      cmp  #$04                         
      bne  W9A0C                        
      jmp  W9B49                        

W9A0C:
      jsr  W9A44                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
W9A12:
      jsr  W97C8                        
      jmp  W99B8                        

W9A18:
      ldy  #$02                         
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $63                          ; Floating point accumulator #1: Mantissa
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $62                          ; Floating point accumulator #1: Mantissa
W9A23:
      ldx  #$90                         
      sec                               
      jsr  $BC49                        ; BASIC ROM
      jsr  $BDDF                        ; BASIC ROM
      jsr  $B487                        ; Routine: Get string, pointer in YA
      jsr  $B6A6                        ; BASIC ROM
      sta  $B4                          ; RS-232 output bits counter/Tape timer
      tax                               
W9A35:
      cpx  #$05                         
      bcs  W9A3F                        
      jsr  W9484                        
      inx                               
      bne  W9A35                        
W9A3F:
      ldx  $B4                          ; RS-232 output bits counter/Tape timer
      jmp  $AB25                        ; BASIC ROM

W9A44:
      jsr  W9A18                        
W9A47:
      ldx  #$00                         
      ldy  #$04                         
      lda  ($5F),y                      ; Scratch for numeric operation
      bmi  W9A91                        
      cmp  #$3B                         
      bne  W9A56                        
      jmp  W9B09                        

W9A56:
      lda  ($5F),y                      ; Scratch for numeric operation
      bne  W9A5D                        
      jmp  W9B13                        

W9A5D:
      cmp  #$20                         
      beq  W9A68                        
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      iny                               
      bne  W9A56                        
W9A68:
      jsr  $FFD2                        ; Routine: Send a char in the channel
      iny                               
W9A6C:
      inx                               
      cpx  $033E                        ; Tape I/O buffer
      bcs  W9A78                        
      jsr  W9484                        
      jmp  W9A6C                        

W9A78:
      lda  ($5F),y                      ; Scratch for numeric operation
      bmi  W9A89                        
      beq  W9AEF                        
      cmp  #$3B                         
      beq  W9AFE                        
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      iny                               
      bne  W9A78                        
W9A89:
      cmp  #$F8                         
      bcs  W9AEF                        
      cmp  #$C0                         
      bcs  W9AC9                        
W9A91:
      and  #$1F                         
      beq  W9AEF                        
      cmp  #$19                         
      bcc  W9A9D                        
      lda  #$3F                         
      bne  W9AF7                        
W9A9D:
      sta  $22                          ; Utility programs pointers area
      lda  #$2E                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      tya                               
      pha                               
      ldy  #$00                         
W9AA9:
      dec  $22                          ; Utility programs pointers area
      bmi  W9AB6                        
W9AAD:
      iny                               
      lda  T_PSEUDOOPS,y                      
      bpl  W9AAD                        
      iny                               
      bne  W9AA9                        
W9AB6:
      lda  T_PSEUDOOPS,y                      
      php                               
      and  #$7F                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      iny                               
      plp                               
      bpl  W9AB6                        
      pla                               
      tay                               
      iny                               
      bne  W9AEB                        
W9AC9:
      and  #$3F                         
      sta  $22                          ; Utility programs pointers area
      asl                               
      adc  $22                          ; Utility programs pointers area
      sty  $22                          ; Utility programs pointers area
      tay                               
      lda  T_MNEMONIC,y                      
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      lda  $800C,y                      
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      lda  $800D,y                      
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      ldy  $22                          ; Utility programs pointers area
      iny                               
W9AEB:
      jsr  W9484                        
      inx                               
W9AEF:
      lda  ($5F),y                      ; Scratch for numeric operation
      beq  W9B13                        
      cmp  #$3B                         
      beq  W9AFE                        
W9AF7:
      jsr  $FFD2                        ; Routine: Send a char in the channel
      inx                               
      iny                               
      bne  W9AEF                        
W9AFE:
      cpx  $033F                        ; Tape I/O buffer
      bcs  W9B09                        
      jsr  W9484                        
      inx                               
      bne  W9AFE                        
W9B09:
      lda  ($5F),y                      ; Scratch for numeric operation
      beq  W9B13                        
      jsr  $FFD2                        ; Routine: Send a char in the channel
      iny                               
      bne  W9B09                        
W9B13:
      lda  $CB                          ; Flag: write chars with SHIFT pressed
      cmp  #$3C                         
      bne  W9B32                        
W9B19:
      lda  $CB                          ; Flag: write chars with SHIFT pressed
      cmp  #$40                         
      bne  W9B19                        
W9B1F:
      jsr  $A82C                        ; BASIC ROM
      lda  $CB                          ; Flag: write chars with SHIFT pressed
      cmp  #$3C                         
      bne  W9B1F                        
W9B28:
      lda  $CB                          ; Flag: write chars with SHIFT pressed
      cmp  #$40                         
      bne  W9B28                        
      lda  #$00                         
      sta  $C6                          ; Number of char in keyboard buffer
W9B32:
      rts                               

      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      jsr  W98D9                        
      jsr  W98BD                        
      jsr  $AEFD                        ; Routine: Verify if there's ',' in current char (Syntax)
      jsr  W9BCC                        
      lda  #$04                         
      sta  $02                          
      jmp  W99B2                        

W9B49:
      lda  $14                          ; Transient: integer value
      pha                               
      lda  $15                          ; Transient: integer value
      pha                               
      ldx  #$05                         
      ldy  #$02                         
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $14                          ; Transient: integer value
      iny                               
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $15                          ; Transient: integer value
W9B5C:
      iny                               
      cpy  $45                          ; BASIC current variable name
      beq  W9B6A                        
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $01FB,x                      ; CPU stack
      inx                               
      jmp  W9B5C                        

W9B6A:
      ldy  #$00                         
W9B6C:
      cpy  $B7                          ; Length of current file name
      beq  W9B7B                        
      lda  $03B0,y                      ; Tape I/O buffer
      sta  $01FB,x                      ; CPU stack
      inx                               
      iny                               
      jmp  W9B6C                        

W9B7B:
      ldy  $BA                          ; Current device number
W9B7D:
      lda  ($5D),y                      ; Scratch for numeric operation
      sta  $01FB,x                      ; CPU stack
      beq  W9B89                        
      inx                               
      iny                               
      jmp  W9B7D                        

W9B89:
      txa                               
      tay                               
      jmp  $A4A2                        ; BASIC ROM

W9B8E:
      jsr  $A613                        ; Routine: Calculate start adress of a program line
      jsr  W9A44                        
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      pla                               
      sta  $15                          ; Transient: integer value
      pla                               
      sta  $14                          ; Transient: integer value
      ldx  $FE                          ; Free 0 page for user program
      lda  $0360,x                      ; Tape I/O buffer
      sta  $14                          ; Transient: integer value
      lda  $0370,x                      ; Tape I/O buffer
      sta  $15                          ; Transient: integer value
      lda  $45                          ; BASIC current variable name
      clc                               
      adc  $B7                          ; Length of current file name
      tay                               
      lda  ($5F),y                      ; Scratch for numeric operation
      beq  W9BC6                        
      ldx  $60                          ; Scratch for numeric operation
      tya                               
      clc                               
      adc  $5F                          ; Scratch for numeric operation
      sta  $5D                          ; Scratch for numeric operation
      bcc  W9BBE                        
      inx                               
W9BBE:
      stx  $5E                          ; Scratch for numeric operation
      dey                               
      sty  $45                          ; BASIC current variable name
      jmp  W99D2                        

W9BC6:
      jmp  W9A12                        

W9BC9:
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

W9BCC:
      jsr  $AD9E                        ; Routine: FRMEVL: Analyzes any formula expression and shows syntax errors. Set type flag $0D (Number: $00, string $FF). Set integer flag $0E (float: $00, integer: $80) puts values in FAC
      jsr  $B782                        ; Routine: Get string parameter (length in Y), switch to numeric
      cmp  #$26                         
      bcs  W9BC9                        
      sta  $B7                          ; Length of current file name
      ldx  #$00                         
      ldy  #$00                         
W9BDC:
      jsr  W9C12                        
      bcc  W9BE5                        
      dec  $B7                          ; Length of current file name
      dec  $B7                          ; Length of current file name
W9BE5:
      sta  $03B0,x                      ; Tape I/O buffer
      inx                               
      cpx  $B7                          ; Length of current file name
      bcc  W9BDC                        
      rts                               

W9BEE:
      jsr  $AD9E                        ; Routine: FRMEVL: Analyzes any formula expression and shows syntax errors. Set type flag $0D (Number: $00, string $FF). Set integer flag $0E (float: $00, integer: $80) puts values in FAC
      jsr  $B782                        ; Routine: Get string parameter (length in Y), switch to numeric
      beq  W9BC9                        
      cmp  #$26                         
      bcs  W9BC9                        
      sta  $BA                          ; Current device number
      ldx  #$00                         
      ldy  #$00                         
W9C00:
      jsr  W9C12                        
      bcc  W9C09                        
      dec  $BA                          ; Current device number
      dec  $BA                          ; Current device number
W9C09:
      sta  $03D8,x                      ; Tape I/O buffer
      inx                               
      cpx  $BA                          ; Current device number
      bcc  W9C00                        
      rts                               

W9C12:
      txa                               
      pha                               
      ldx  #$00                         
      stx  $41                          ; Pointer: DATA current element address
W9C18:
      lda  ($22),y                      ; Utility programs pointers area
      cmp  T_MNEMONIC,x                      
      bne  W9C31                        
      iny                               
      lda  ($22),y                      ; Utility programs pointers area
      cmp  $800C,x                      
      bne  W9C30                        
      iny                               
      lda  ($22),y                      ; Utility programs pointers area
      cmp  $800D,x                      
      beq  W9C41                        
      dey                               
W9C30:
      dey                               
W9C31:
      inx                               
      inx                               
      inx                               
      inc  $41                          ; Pointer: DATA current element address
      cpx  #$A8                         
      bne  W9C18                        
      pla                               
      tax                               
      lda  ($22),y                      ; Utility programs pointers area
      iny                               
      clc                               
      rts                               

W9C41:
      pla                               
      tax                               
      lda  $41                          ; Pointer: DATA current element address
      ora  #$C0                         
      iny                               
      sec                               
      rts                               

      lda  #$24                         
W9C4C:
      sta  $0100                        ; CPU stack/Tape error/Floating conversion area
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  #$01                         
      tay                               
      ldx  #$00                         
      jsr  $FFBD                        ; Routine: Set file name
      ldx  #$08                         
      ldy  #$60                         
      jsr  $FFBA                        ; Routine: Set primary, secondary and logical addresses
      jsr  $F3D5                        ; Routine: Sends file name on serial bus
      lda  $BA                          ; Current device number
      jsr  $FFB4                        ; Routine: Set to Trasmit the serial bus device
      lda  $B9                          ; Current secondary address
      jsr  $FF96                        ; Routine: Send secondary address after Trasmit
      lda  #$00                         
      sta  $90                          ; Statusbyte ST of I/O KERNAL
      ldy  #$03                         
W9C74:
      sty  $FF                          ; Transient data area of BASIC
      jsr  $FFA5                        ; Routine: Acept a byte from serial port
      sta  $FE                          ; Free 0 page for user program
      ldy  $90                          ; Statusbyte ST of I/O KERNAL
      bne  W9CB8                        
      jsr  $FFA5                        ; Routine: Acept a byte from serial port
      ldy  $90                          ; Statusbyte ST of I/O KERNAL
      bne  W9CB8                        
      ldy  $FF                          ; Transient data area of BASIC
      dey                               
      bne  W9C74                        
      ldx  $FE                          ; Free 0 page for user program
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      jsr  W9484                        
W9C93:
      jsr  $FFA5                        ; Routine: Acept a byte from serial port
      ldx  $90                          ; Statusbyte ST of I/O KERNAL
      bne  W9CB8                        
      tay                               
      tax                               
      beq  W9CA4                        
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jmp  W9C93                        

W9CA4:
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  $DC01                        ; Data port B #1: keyboard, joystick, paddle
      and  #$10                         
      beq  W9CB8                        
      lda  $DC01                        ; Data port B #1: keyboard, joystick, paddle
      bpl  W9CB8                        
      ldy  #$02                         
      jmp  W9C74                        

W9CB8:
      jsr  $F642                        ; Routine: Addresses the device with the command
      jmp  W9CCA                        

      ldy  #$01                         
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W9CCA                        
      cmp  #$24                         
      beq  W9C4C                        
      bne  W9CEC                        
W9CCA:
      lda  #$08                         
      sta  $BA                          ; Current device number
      jsr  $FFB4                        ; Routine: Set to Trasmit the serial bus device
      lda  #$6F                         
      sta  $B9                          ; Current secondary address
      jsr  $FF96                        ; Routine: Send secondary address after Trasmit
W9CD8:
      jsr  $FFA5                        ; Routine: Acept a byte from serial port
      jsr  $FFD2                        ; Routine: Send a char in the channel
      cmp  #$0D                         
      bne  W9CD8                        
      jsr  $FFAB                        ; Routine: Set serial bus to Not-trasmit
      lda  #$00                         
      sta  $C6                          ; Number of char in keyboard buffer
      jmp  W8537                        

W9CEC:
      lda  #$08                         
      sta  $BA                          ; Current device number
      jsr  $FFB1                        ; Routine: Set to Receive the devices to the serial bus
      lda  #$6F                         
      sta  $B9                          ; Current secondary address
      jsr  $FF93                        ; Routine: Send secondary address after Receive
W9CFA:
      lda  ($7A),y                      ; CHRGET (Introduce a char) subroutine
      beq  W9D04                        
      jsr  $FFA8                        ; Routine: Send a byte to serial port
      iny                               
      bne  W9CFA                        
W9D04:
      jsr  $FFAE                        ; Routine: Set serial bus to Not-Receive
      jmp  W8537                        

      lda  $30                          ; Pointer: BASIC starting arrays
      cmp  $2E                          ; Pointer: BASIC starting variables
      bne  W9D16                        
      lda  $2F                          ; Pointer: BASIC starting arrays
      cmp  $2D                          ; Pointer: BASIC starting variables
      beq  W9D19                        
W9D16:
      jsr  W9D1F                        
W9D19:
      jmp  W8537                        

W9D1C:
      jmp  $AAD7                        ; Routine: Write return effect and/or advancement

W9D1F:
      lda  #$F9                         
      ldx  #$BF                         
W9D23:
      stx  $46                          ; BASIC current variable name
      sta  $45                          ; BASIC current variable name
      cpx  $30                          ; Pointer: BASIC starting arrays
      bcc  W9D1C                        
      bne  W9D31                        
      cmp  $2F                          ; Pointer: BASIC starting arrays
      bcc  W9D1C                        
W9D31:
      jsr  $A82C                        ; BASIC ROM
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  #$59                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$36                         
      sta  $01                          ; 6510 I/O register
      ldy  #$06                         
      lda  ($45),y                      ; BASIC current variable name
      sta  $3D                          ; Pointer: BASIC instruction for CONT
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $3C                          ; BASIC precedent line number
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $23                          ; Utility programs pointers area
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $22                          ; Utility programs pointers area
      lda  #$37                         
      sta  $01                          ; 6510 I/O register
      lda  $2B                          ; Pointer: BASIC starting programs
      ldx  $2C                          ; Pointer: BASIC starting programs
      sta  $5F                          ; Scratch for numeric operation
      stx  $60                          ; Scratch for numeric operation
W9D63:
      ldy  #$01                         
      lda  ($5F),y                      ; Scratch for numeric operation
      tax                               
      dey                               
      cmp  $23                          ; Utility programs pointers area
      bcc  W9D77                        
      beq  W9D71                        
      bcs  W9D7F                        
W9D71:
      lda  ($5F),y                      ; Scratch for numeric operation
      cmp  $22                          ; Utility programs pointers area
      bcs  W9D7F                        
W9D77:
      lda  ($5F),y                      ; Scratch for numeric operation
      sta  $5F                          ; Scratch for numeric operation
      stx  $60                          ; Scratch for numeric operation
      bcc  W9D63                        
W9D7F:
      jsr  W9A18                        
      lda  #$3A                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      jsr  W9484                        
      lda  #$36                         
      sta  $01                          ; 6510 I/O register
      ldy  #$04                         
      lda  ($45),y                      ; BASIC current variable name
      sta  $23                          ; Utility programs pointers area
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $22                          ; Utility programs pointers area
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $47                          ; Pointer: BASIC current variable data
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $62                          ; Floating point accumulator #1: Mantissa
      dey                               
      lda  ($45),y                      ; BASIC current variable name
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  #$37                         
      sta  $01                          ; 6510 I/O register
      ldx  #$00                         
W9DAE:
      lda  ($22),y                      ; Utility programs pointers area
      jsr  $FFD2                        ; Routine: Send a char in the channel
      iny                               
      inx                               
      cpx  #$0E                         
      bcs  W9DC7                        
      cpy  $47                          ; Pointer: BASIC current variable data
      bcc  W9DAE                        
W9DBD:
      inx                               
      lda  #$2E                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      cpx  #$0E                         
      bcc  W9DBD                        
W9DC7:
      lda  #$2E                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$24                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $3D                          ; Pointer: BASIC instruction for CONT
      jsr  W9379                        
      lda  $3C                          ; BASIC precedent line number
      jsr  W9379                        
      jsr  W9484                        
      ldx  $62                          ; Floating point accumulator #1: Mantissa
      bpl  W9DF9                        
      inx                               
      bne  W9DEF                        
      lda  #$EC                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jmp  W9DFF                        

W9DEF:
      lda  #$F3                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jmp  W9DFF                        

W9DF9:
      jsr  W9484                        
      jsr  W9A23                        
W9DFF:
      jsr  W9B13                        
      lda  $45                          ; BASIC current variable name
      ldx  $46                          ; BASIC current variable name
      sec                               
      sbc  #$07                         
      bcs  W9E0C                        
      dex                               
W9E0C:
      ldy  #$37                         
      sty  $01                          ; 6510 I/O register
W9E10:
      jmp  W9D23                        

      lda  #$FF                         
      ldy  #$01                         
      sta  ($2B),y                      ; Pointer: BASIC starting programs
      jsr  $A533                        ; Routine: Relink BASIC program
      lda  $22                          ; Utility programs pointers area
      ldy  $23                          ; Utility programs pointers area
      clc                               
      adc  #$02                         
      bcc  W9E26                        
      iny                               
W9E26:
      sta  $2D                          ; Pointer: BASIC starting variables
      sty  $2E                          ; Pointer: BASIC starting variables
      jsr  $A660                        ; BASIC ROM
W9E2D:
      lda  #$3B                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $0282                        ; Pointer: Memory base for Operative System
      jsr  W9379                        
      lda  $0281                        ; Pointer: Memory base for Operative System
      jsr  W9379                        
      lda  #$88                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $38                          ; Pointer: BASIC ending memory
      jsr  W9379                        
      lda  $37                          ; Pointer: BASIC ending memory
      jsr  W9379                        
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$28                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $37                          ; Pointer: BASIC ending memory
      sec                               
      sbc  $0281                        ; Pointer: Memory base for Operative System
      lda  $38                          ; Pointer: BASIC ending memory
      sbc  $0282                        ; Pointer: Memory base for Operative System
      lsr                               
      lsr                               
      tax                               
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$65                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  #$50                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $2C                          ; Pointer: BASIC starting programs
      jsr  W9379                        
      lda  $2B                          ; Pointer: BASIC starting programs
      jsr  W9379                        
      lda  #$88                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $2E                          ; Pointer: BASIC starting variables
      jsr  W9379                        
      lda  $2D                          ; Pointer: BASIC starting variables
      jsr  W9379                        
      lda  #$20                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  #$28                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      lda  $2D                          ; Pointer: BASIC starting variables
      sec                               
      sbc  $2B                          ; Pointer: BASIC starting programs
      lda  $2E                          ; Pointer: BASIC starting variables
      sbc  $2C                          ; Pointer: BASIC starting programs
      lsr                               
      lsr                               
      tax                               
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$65                         
      ldy  #$84                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      lda  $2D                          ; Pointer: BASIC starting variables
      sec                               
      sbc  $2B                          ; Pointer: BASIC starting programs
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  $2E                          ; Pointer: BASIC starting variables
      sbc  $2C                          ; Pointer: BASIC starting programs
      sta  $62                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
      lda  #$CC                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      lda  $37                          ; Pointer: BASIC ending memory
      sec                               
      sbc  $2D                          ; Pointer: BASIC starting variables
      sta  $63                          ; Floating point accumulator #1: Mantissa
      lda  $38                          ; Pointer: BASIC ending memory
      sbc  $2E                          ; Pointer: BASIC starting variables
      sta  $62                          ; Floating point accumulator #1: Mantissa
      jsr  W9A23                        
      lda  #$66                         
      ldy  #$E4                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      jmp  W8537                        

      jsr  W924C                        
      lda  $15                          ; Transient: integer value
      bmi  W9F21                        
      inc  $14                          ; Transient: integer value
      bne  W9EFA                        
      inc  $15                          ; Transient: integer value
W9EFA:
      lda  $14                          ; Transient: integer value
      sta  $37                          ; Pointer: BASIC ending memory
      lda  $15                          ; Transient: integer value
      sta  $38                          ; Pointer: BASIC ending memory
      jsr  $A644                        ; BASIC ROM
      jmp  W9E2D                        

      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      cmp  #$2C                         
      bne  W9F21                        
      cpx  #$02                         
      bcs  W9F21                        
      stx  $21                          ; Transient strings stack
      jsr  $B79B                        ; Routine: Holt Byte: Read and evaluate expression from BASIC text; the 1 byte value is then stored in X and in FAC+4
      ldy  $21                          ; Transient strings stack
      txa                               
      sta  $033E,y                      ; Tape I/O buffer
      jmp  W8537                        

W9F21:
      jmp  $B248                        ; Routine: Write error message (ILLEGAL QUANTITY)

      ldx  #$00                         
W9F26:
      txa                               
      pha                               
      lda  #$51                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      pla                               
      tax                               
      pha                               
      cpx  #$0A                         
      bcs  W9F39                        
      jsr  W9484                        
W9F39:
      lda  #$00                         
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$57                         
      ldy  #$83                         
      jsr  $AB1E                        ; Routine: Output string, which is indicated by YA, until 0 byte or quote is found
      pla                               
      tay                               
      pha                               
      lda  $0340,y                      ; Tape I/O buffer
      tax                               
      lda  $0350,y                      ; Tape I/O buffer
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      lda  #$2D                         
      jsr  $FFD2                        ; Routine: Send a char in the channel
      pla                               
      tay                               
      pha                               
      lda  $0360,y                      ; Tape I/O buffer
      tax                               
      lda  $0370,y                      ; Tape I/O buffer
      jsr  $BDCD                        ; Routine: Write (AX) integer correspondenting string
      jsr  $AAD7                        ; Routine: Write return effect and/or advancement
      pla                               
      tax                               
      inx                               
      cpx  #$10                         
      bcc  W9F26                        
      jmp  W8537                        

      lda  #$9F                         
      pha                               
      lda  #$80                         
      pha                               
      lda  #$00                         
      pha                               
      pha                               
      pha                               
      pha                               
      jmp  $EA31                        ; Default hardware interrupt (IRQ)

      ldx  $C6                          ; Number of char in keyboard buffer
      beq  W9FA3                        
      dex                               
      lda  $0277,x                      ; Keyboard buffer queue (FIFO)
      cmp  #$85                         
      bcc  W9FA3                        
      cmp  #$8D                         
      bcs  W9FA3                        
      sec                               
      sbc  #$85                         
      tay                               
      lda  W9FA6,y                      
      sta  $0277,x                      ; Keyboard buffer queue (FIFO)
      inx                               
      lda  #$0D                         
      sta  $0277,x                      ; Keyboard buffer queue (FIFO)
      inx                               
      stx  $C6                          ; Number of char in keyboard buffer
W9FA3:
      jmp  $EA81                        

W9FA6:
      eor  $58                          ; Scratch for numeric operation
      jmp  $4249                        

      eor  $404F,y                   
NMI:      
      jsr  $F6BC                        ; Routine: Verifies pressure STOP/RVS keys
      jsr  $FFE1                        ; Routine: Terminate the keyboard scan
      beq  W9FB9                        
      jmp  $FEBC                        ; Restores cpu registers and goes out from interrupt

W9FB9:
      jsr  $FD15                        ; Routine RESTOR of KERNAL
      lda  #$71                         
      ldx  #$9F                         
      stx  $0315                        ; Vector: Hardware Interrupt (IRQ)
      sta  $0314                        ; Vector: Hardware Interrupt (IRQ)
      lda  $DD00                        ; Data port A #2: serial bus, RS-232, VIC memory
      pha                               
      jsr  $FDA3                        
      pla                               
      sta  $DD00                        ; Data port A #2: serial bus, RS-232, VIC memory
      lda  $D018                        ; VIC memory control register
      pha                               
      jsr  $E5A0                        ; Routine: Set VIC6567 registers and devices
      pla                               
      sta  $D018                        ; VIC memory control register
      lda  #$00                         
      sta  $0291                        ; Flag: mode SHIFT: 00=Disable case 0x80=Enable
      sta  $CF                          ; Flag: Last cursore state (Flash/fixed)
      lda  #$48                         
      ldx  #$EB                         
      stx  $0290                        ; Vector: keyboard table preparation
      sta  $028F                        ; Vector: keyboard table preparation
      lda  #$04                         
      sta  $028B                        ; Repeat velocity counter
      lda  #$0A                         
      sta  $0289                        ; Keyboard buffer misure
      sta  $028C                        ; Repeat delay counter
      jsr  $E534                        
      jmp  ($A002)                      ; Routine Vector: close canal, start up
