#include "tty.h"
#include "int.h"
#include "ports.h"

#define PIC_1ST_COMMAND  0x20
#define PIC_1ST_DATA     0x21
#define PIC_2ND_COMMAND  0xA0
#define PIC_2ND_DATA     0xA1

#define ICW1_ICW4        0x01 // ICW4 included
#define ICW1_SINGLE      0x02 // Single (cascade) mode
#define ICW1_INTERVAL4   0x04 // Call address interval 4 (8)
#define ICW1_LEVEL       0x08 // Level triggered (edge) mode
#define ICW1_INIT        0x10 // Initialization (always set)
/* hight ICW1 bits are unused in x86 */

/* ICW2 selects int offset */

/* ICW3 selects  the IRQ line on the master controller
 * to which the slave is connecte */

// ICW4 send if ICW1_ICW4 is set
#define ICW4_8086        0x01 // 8086/88 (MCS-80/85) mode
#define ICW4_AUTO        0x02 // Auto (normal) EOI
#define ICW4_BUF_SLAVE   0x08 // Buffered mode/slave
#define ICW4_BUF_MASTER  0x0C // Buffered mode/master
#define ICW4_SFNM        0x10 // Special fully nested (not)


void int_install(unsigned char vector, void (*func)(), unsigned char type)  {
	uint8_t* idt_table= (uint8_t*)IDT_TABLE;
	uint8_t b[8];
	int i;

	b[0]= (uint32_t)func & 0x000000FF;
	b[1]=((uint32_t)func & 0x0000FF00) >> 8;
	b[2]=SYS_CODE_SELECTOR;
	b[3]=0;
	b[4]=0;
	b[5]=type;
	b[6]=((uint32_t)func & 0x00FF0000) >> 16;
	b[7]=((uint32_t)func & 0xFF000000) >> 24;

	for(i = 0; i < 8; i++)
		*(idt_table+vector*8+i)=b[i];
}

/*
arguments:
	offset1 - vector offset for master PIC
		vectors on the master become offset1..offset1+7
	offset2 - same for slave PIC: offset2..offset2+7
*/
void int_remap(int offset1, int offset2) {
  int a1 = in(PIC_1ST_DATA); // save masks
  int a2 = in(PIC_2ND_DATA); // save masks

  out(PIC_1ST_COMMAND, ICW1_INIT+ICW1_ICW4);
  out(PIC_2ND_COMMAND, ICW1_INIT+ICW1_ICW4);
  out(PIC_1ST_DATA,offset1);
  out(PIC_2ND_DATA,offset2);
  out(PIC_1ST_DATA,0x04); // irq 2 is connected to a slave
  out(PIC_2ND_DATA,0x02);

  out(PIC_1ST_DATA,ICW4_8086);
  out(PIC_2ND_DATA,ICW4_8086);

  out(PIC_1ST_DATA, a1);   // restore saved masks.
  out(PIC_2ND_DATA, a2);   // restore saved masks.
}

// loads IDT reg
void int_setup() {
	unsigned short *table_limit = (unsigned short*)IDT_REG;
	unsigned int *table_address = (unsigned int*)(IDT_REG+2);

	*table_limit = 256*8 - 1;
	*table_address = IDT_TABLE;

	int_remap(0x20, 0x28);
	__asm__("lidt 0(,%0,)"::"a"(IDT_REG));
}


void int_enable() {
	__asm__("sti");
}

void int_disable() {
	__asm__("cli");
}


char scancodes[] = {
  0,
  0, //ESC
  '1','2', '3', '4', '5', '6', '7', '8', '9', '0',
  '-', '=', 
  8, //BACKSPACE
  '\t',//TAB
  'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']',
  '\n', //ENTER
  0, //CTRL
  'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
  0, //LEFT SHIFT,
  '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',
  0, //RIGHT SHIFT,
  '*', //NUMPAD
  0, //ALT
  ' ', //SPACE
  0, //CAPSLOCK
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //F1 - F10
  0, //NUMLOCK
  0, //SCROLLOCK
  0, //HOME
  0, 
  0, //PAGE UP
  '-', //NUMPAD
  0, 0,
  0, //(r)
  '+', //NUMPAD
  0, //END
  0, 
  0, //PAGE DOWN
  0, //INS
  0, //DEL
  0, //SYS RQ
  0, 
  0, 0, //F11-F12
  0,
  0, 0, 0, //F13-F15
  0, 0, 0, 0, 0, 0, 0, 0, 0, //F16-F24
  0, 0, 0, 0, 0, 0, 0, 0
};

char scancodes_shifted[] = {
  0,
  0, //ESC
  '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
  '_', '+', 
  8, //BACKSPACE
  '\t',//TAB
  'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}',
  '\n', //ENTER
  0, //CTRL
  'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~',
  0, //LEFT SHIFT,
  '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',
  0, //RIGHT SHIFT,
  '*', //NUMPAD
  0, //ALT
  ' ', //SPACE
  0, //CAPSLOCK
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //F1 - F10
  0, //NUMLOCK
  0, //SCROLLOCK
  0, //HOME
  0, 
  0, //PAGE UP
  '-', //NUMPAD
  0, 0,
  0, //(r)
  '+', //NUMPAD
  0, //END
  0, 
  0, //PAGE DOWN
  0, //INS
  0, //DEL
  0, //SYS RQ
  0, 
  0, 0, //F11-F12
  0,
  0, 0, 0, //F13-F15
  0, 0, 0, 0, 0, 0, 0, 0, 0, //F16-F24
  0, 0, 0, 0, 0, 0, 0, 0
};
#define IRQ_HANDLER(func) \
	void func (void);\
	asm(#func ": pusha \n call _" #func " \n movb $0x20, %al \n outb %al, $0x20 \n popa \n iret \n");\
	void _ ## func(void)


// NOTE: initialized global variable before handler leads to strange crashes.
//       linker settings should be investigated and tuned.


IRQ_HANDLER(exception_stub) {
	printf("exception stub\n");
	for(;;);
}

IRQ_HANDLER(exception_divide) {
	printf("divide error\n");
	for(;;);
}

IRQ_HANDLER(irq_stub) {
	printf("irq stub\n");
}

IRQ_HANDLER(irq_timer) {
	//printf("timer\n");
}

IRQ_HANDLER(irq_rtc) {
	//printf("rtc\n");
}

int shift;
IRQ_HANDLER(irq_keyboard) {
	unsigned char scancode, key;
	unsigned char creg;

	//printf("keyboard\n");

	scancode = in(0x60); // get scancode

	switch (scancode) {
	case 0x36:
	case 0x2A:
		// shift pushed
		shift = 1;
		break;

	case 0x36 | 0x80:
	case 0x2A | 0x80:
		// shift released
		shift = 0;
		break;

	default:
		if(scancode >= 0x80) { // key released?
			// do nothing

		} else { // key pushed
			// check for shift...
			if(shift) key = scancodes_shifted[scancode];
			else key = scancodes[scancode];
			if(key) // printable
			  tty_input(key);
		}
		break;
	}

	creg = in(0x61); // get keyboard control register

	creg |= 1;

	out(0x61, creg);
}

#if 0
void cause_triple_fault() {
	__asm__(
		"lidt 0\n"
		"int $3\n"
	);
}
#endif

INT_Regs16 *int_bios_regs = (INT_Regs16*)(MEM_16BIT_STACK-sizeof(INT_Regs16));

void int_init() {
	int i;

	for(i = 0; i < 32; i++)
		int_install(i, &exception_stub, 0x8e);

	for(i = 32; i < 256; i++)
		int_install(i, &irq_stub, 0x8e);


	int_install(0x0, &exception_divide, 0x8e);
	int_install(0x20, &irq_timer, 0x8e);
	int_install(0x21, &irq_keyboard, 0x8e);
	//irq_install(0x21, &exception_stub, 0x8e);
	int_install(0x28, &irq_rtc, 0x8e);
	int_setup();
	//__asm__("int $3");
	//__asm__("int $0x1");
	int_enable();
}

void int_bios(int irq_num) {
  void (*pf)(void *, int) = (void*)MEM_INT_BIOS_CODE;
  int_disable();
  int_remap(0x08,0x70); // set realmode default mapping
  //printf("0x%x\n", (char*)regs+sizeof(*regs));
  int_bios_regs->ds = 0;
  int_bios_regs->sp = (uint32_t)int_bios_regs+sizeof(INT_Regs16);
  pf((char*)(uint32_t)int_bios_regs->sp-24, irq_num);
  int_setup();
  int_remap(0x20,0x28); // restore our setup
  int_enable();
}


/* IRQ0 – Intel 8253 or Intel 8254 Programmable Interval Timer, aka the system timer */
/* IRQ1 – Intel 8042 keyboard controller */
/* IRQ2 – not assigned in PC/XT; cascaded to slave 8259 INT line in PC/AT */
/* IRQ3 – 8250 UART serial port COM2 and COM4 */
/* IRQ4 – 8250 UART serial port COM1 and COM3 */
/* IRQ5 – hard disk controller in PC/XT; Intel 8255 parallel port LPT2 in PC/AT */
/* IRQ6 – Intel 82072A floppy disk controller */
/* IRQ7 – Intel 8255 parallel port LPT1 / spurious interrupt */

/* Slave 8259 (PC/AT and later only) */
/* IRQ8 – real-time clock (RTC) */
/* IRQ9 – no common assignment */
/* IRQ10 – no common assignment */
/* IRQ11 – no common assignment */
/* IRQ12 – Intel 8042 PS/2 mouse controller */
/* IRQ13 – math coprocessor */
/* IRQ14 – hard disk controller 1 */
/* IRQ15 – hard disk controller 2 */

/* Initially IRQ7 was a common choice for the use of a sound card, */
/* but later IRQ5 was used when it was found that IRQ7 would */
/* interfere with the printer port (LPT1). The serial ports are */
/* frequently disabled to free an IRQ line for another device. */

/* IRQ2/9 is the traditional interrupt line for an MPU-401 MIDI port, */
/* but this conflicts with the ACPI system control interrupt (SCI is */
/* hardwired to IRQ9 on Intel chipsets); this means ISA MPU-401 */
/* cards with a hardwired IRQ 2/9, and MPU-401 device drivers with a */
/* hardcoded IRQ 2/9, cannot be used in interrupt-driven mode on a */
/* system with ACPI enabled. */
