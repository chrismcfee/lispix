#ifndef PORT_H
#define PORT_H

#include "common.h"

static inline unsigned char in(unsigned short _port) {
	// "=a" (result) means: put AL register in variable result when finished
	// "d" (_port) means: load EDX with _port
	unsigned char result;
	__asm__  ("in %%dx, %%al" : "=a" (result) : "d" (_port));
	return result;
}

static inline void out(unsigned short _port, unsigned char _data) {
	// "a" (_data) means: load EAX with _data
	// "d" (_port) means: load EDX with _port
	__asm__ ("out %%al, %%dx" : :"a" (_data), "d" (_port));
}

// Forces the CPU to wait for an I/O operation to complete.
// only use this when there's nothing like a status register
// or an IRQ to tell you the info has been received. 
//void io_wait(void) {
//  __asm__ volatile ("jmp 1f;1:jmp 1f;1:");
//}

// alternatively, you may use another I/O cycle on an
// 'unused' port (which has the nice property of being
// CPU-speed independent):
static inline void io_wait(void) {
  asm volatile("outb %%al, $0x80" : : "a"(0));
  // port 0x80 is used for 'checkpoints' during POST.
  // linux kernel seems to think it's free for use :-/
}

static __inline__ int irqEnabled() {
  int f;
  asm volatile ("pushf;popl %0":"=g" (f));
  return f & (1<<9);
}

// sends the PIC chip (8259a) that we're ready for more
// interrupts. as pics are cascaded for int 8-15, if no>=8,
// we will have to send a 'clearance code' for both master
// and slave PICs. 
static __inline__ void irqUnlock(int no) {
  /*             Val, Port */
  if(no>7) out(0x20,0xa0);
  out(0x20,0x20);
}

// invalidates the TLB (Translation Lookaside Buffer) for one
// specific virtual address (next memory reference for the
// page will be forced to re-read PDE and PTE from main memory.
// Must be issued every time you update one of those tables).
// m points to a logical address, not a physical or virtual one:
// an offset for your ds segment. Note *m is used, not just m:
// if you use m here, you invalidate the address of the m
// variable (not what you want!). 
static __inline__ void flush_one_tlb(void *m) {
  asm volatile("invlpg %0"::"m" (*(char*)m));
}

#endif

