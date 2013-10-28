#-include config.make

TARGET := floppy.img

DEBUG := true
#RELEASE := true

CC ?= gcc
LD ?= ld

.SUFFIXES: .c .d .o .h

CFLAGS += -I.

#ifneq ($(findstring $(DEBUG), 1 true yes),)
#CFLAGS += -DDEBUG
#CFLAGS += -g
#endif

#ifneq ($(findstring $(RELEASE), 1 true yes),)
#CFLAGS += -O3
#CFLAGS += -DRELEASE
#else
#CFLAGS += -O0
#endif

CFLAGS += -fno-stack-protector -fno-leading-underscore -ffreestanding 
#CFLAGS += -save-temps
CFLAGS += -std=gnu99
CFLAGS += -Wall
CFLAGS += -Werror-implicit-function-declaration
CFLAGS += -Wimplicit-int
CFLAGS += -Wsequence-point

LDFLAGS += $(LIBS)

# note: start.asm be first in list
SRCS :=
SRCS += start.asm main.c tty.c int.c ports.c
SRCS += stdlib.c lisp.c bios.c

DEPS := $(filter %.d, $(SRCS:.c=.d) $(SRCS:.m4c=.d) $(SRCS:.cc=.d))
OBJS := $(filter %.o, $(SRCS:.asm=.o) $(SRCS:.c=.o) $(SRCS:.m4c=.o) $(SRCS:.cc=.o))

.PHONY: clean depend $(TARGET) test

# next is required to preseve chained intermediates
#.PRECIOUS: %_ecc.cc

Q ?= @

all: $(TARGET)

depend: $(DEPS)

# include header dependency
ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include $(DEPS)
endif

makeboot: makeboot.c
	$(Q)$(CC) makeboot.c -o makeboot

floppy.img: bootsector.bin int_bios.bin kernel.bin makeboot
	@echo '===> LD   $@'
	@./makeboot a.img bootsector.bin int_bios.bin kernel.bin
	@dd if=/dev/zero count=1 bs=1474560 of=floppy.img
	@dd if=a.img conv=notrunc of=floppy.img
	@rm -f a.img


bootsector.bin: bootsector.asm
	@echo '===> NASM bootsector.asm'
	@nasm -f bin bootsector.asm -o bootsector.bin

int_bios.bin: int_bios.asm
	@echo '===> NASM int_bios.asm'
	@nasm -f bin int_bios.asm -o int_bios.bin

kernel.bin: link.ld $(OBJS)
	@echo '===> LD   $@'
	$(Q)$(LD) -T link.ld -o $@ $(OBJS)

clean:
	@echo '===> CLEAN'
	$(Q)rm -f $(DEPS) $(OBJS) floppy.img bootsector.bin kernel.bin makeboot

run: $(TARGET)
	./run-qemu
#	cd $(shell dirname $(TARGET)) && ./$(shell basename $(TARGET))


#%.c: %.m4c
#	@echo '===> M4   $<'
#	./m4parse $< > $@

.c.o:
	@echo '===> CC   $<'
	$(Q)$(CC) $(CFLAGS) -c $< -o $@


%.o: %.asm
	@echo '===> NASM $<'
	$(Q)nasm -f elf -o $@ $<

%.d: %.c
	@echo '===> DEP  $<'
	$(Q)$(CC) $(CFLAGS) -MM $< | sed 's#^$(@F:%.d=%.o):#$@ $(@:%.d=%.o):#' > $@
