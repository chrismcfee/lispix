#ifndef INT_H
#define INT_H

#include "common.h"

#define IDT_TABLE         MEM_IDT
#define IDT_REG           (IDT_TABLE+2048)
#define SYS_CODE_SELECTOR 0x8

void int_enable();
void int_disable();
void int_install(unsigned char vector, void (*func)(), unsigned char type);

/* registers pushed on the stack by exceptions or interrupts that
switch from user privilege to kernel privilege
	*** WARNING ***
The layout of this struct must agree with the order in which
registers are pushed and popped in the assembly-language
interrupt handler code. */
typedef struct {
  uint8_t stack[64];
  uint16_t ds, es, fs, gs;
  uint16_t di, si, bp, sp;
  union { uint16_t bx; struct {uint8_t bl, bh; }; };
  union { uint16_t dx; struct {uint8_t dl, dh; }; };
  union { uint16_t cx; struct {uint8_t cl, ch; }; };
  union { uint16_t ax; struct {uint8_t al, ah; }; };
  //uint16_t bx, dx, cx, ax;
} INT_Regs16;

extern INT_Regs16 *int_bios_regs;

void int_bios(int irq_num);
void int_init();


#endif
