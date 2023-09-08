# Paths
SRCDIR := src
OBJDIR := dist
SRC := $(SRCDIR)/giga-ass.asm
PRG := $(OBJDIR)/giga-ass.prg
CRT := $(OBJDIR)/giga-ass.crt
LST := $(OBJDIR)/giga-ass.lst

# Commands
MKPRG = dasm $(SRC) -l$(LST) -o$(PRG)
MKCRT = cartconv -t normal -i $(PRG) -o $(CRT)
RM := rm -rf
MKDIR := mkdir -p
X64 := x64sc

# Targets
.PHONY: all clean run

all: $(CRT)

$(PRG): | $(OBJDIR)

$(OBJDIR):
	$(MKDIR) $(OBJDIR)

clean:
	$(RM) $(OBJDIR)

run: $(CRT)
	$(X64) $(CRT)

# Rules
$(PRG): $(SRC)
	$(MKPRG)

$(CRT): $(PRG)
	$(MKCRT)
