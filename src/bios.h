#ifndef BIOS_H
#define BIOS_H

#include "common.h"

#define DISK_FD0 0x00
#define DISK_FD1 0x01
#define DISK_HD0 0x80
#define DISK_HD1 0x81

void disk_select(int disk);
int disk_read(void *dst, uint32_t src, int size);
int disk_write(uint32_t dst, void *src, int size);

#endif
