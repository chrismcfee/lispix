#ifndef COMMON_H
#define COMMON_H

#include "stdint.h"

#define NULL ((void*)0)

typedef uint32_t size_t;

// Memory Lauout:
// 0     - 3FF      RAM    Real mode, IVT (Interrupt Vector Table)
// 400   - 4FF      RAM    BDA (BIOS data area)
// 500   - 9FFFF	RAM	   Free memory, 7C00 used for boot sector
// A0000 - BFFFF	VRAM   Video memory
// C0000 - C7FFF	VROM   Video BIOS
// C8000 - EFFFF	?	   BIOS shadow area
// F0000 - FFFFF	ROM    System BIOS

// 0xA0000 is the pointer address to the Graphical Mode
// 0xB8000 is the pointer address to the Color Text Mode
// 0xB0000 is the pointer to the Monochrome text Mode.

#define MEM_16BIT_STACK      0x001000
#define MEM_INT_BIOS_CODE    0x002000
#define MEM_16BIT_IO         0x010000
#define MEM_KERNEL           0x020000
#define MEM_DISK_BUF         ((uint8_t*)0x040000)
#define MEM_16BIT_HEAP       ((uint8_t*)0x048000)
#define MEM_IDT              0x100000
#define MEM_HEAP             0x200000

#define MEM_16BIT_HEAP_SIZE  0x100000

#endif
