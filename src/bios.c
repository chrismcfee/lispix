#include "bios.h"
#include "stdlib.h"
#include "tty.h"
#include "int.h"
#include "ports.h"

#define regs (*int_bios_regs)

#define SECTOR_SIZE 512
#define SECTORS 18 /* sectors per track */
#define HEADS 2
#define CYLINDERS 80

static int current_disk; // currently selected disk drive
static int sector_size;
static int heads_per_drive;
static int tracks_per_head;
static int sectors_per_track;

static uint64_t total_sectors;

typedef struct EDD_Address {
  uint8_t size; // packet size
  uint8_t reserved1;
  uint8_t nblocks; // number of blocks to transfer (upto 127)
  uint8_t reserved2;
  uint16_t buffer_off;
  uint16_t buffer_seg;
  uint64_t sector;
} EDD_Address;

typedef struct EDD_Result {
  uint16_t size; // packet size
  uint16_t flags; // feature flags
  uint32_t ncyls;  
  uint32_t nheads;
  uint32_t nsects;
  uint64_t total_sectors;
  uint16_t sector_size;
  uint32_t edd_config_params_ptr;
} EDD_Result;

static int edd_version;
static int edd_features;


static void lba2chs(uint32_t lba, uint32_t *c, uint32_t *h, uint32_t *s) {
  *c = lba / (sectors_per_track * heads_per_drive);
  *h = (lba / sectors_per_track) % heads_per_drive;
  *s = lba % sectors_per_track + 1;
}

static char *disk_name(int disk) {
  if(disk&0x80) return (disk&1) ? "hdb" : "hda";
  return (disk&1) ? "fdb" : "fda";

}

static void edd_select() {
  EDD_Result *pres;

  regs.dl = current_disk;
  regs.ah = 0x41; // CHECK-EXTENSIONS-PRESENT
  regs.bx = 0x55AA;
  int_bios(0x13);
  if(regs.bx != 0xAA55) {
	printf("No EDD support\n", regs.ah>>4, regs.ah&0xf);
	edd_version = 0;
	return;
  }

  printf("EDD v%d.%d supported\n", regs.ah>>4, regs.ah&0xf);
  edd_version = regs.ah;
  edd_features = regs.cx;

  pres = (EDD_Result*)0x500;
  memset(pres, 0, sizeof(EDD_Result));
  pres->size = sizeof(EDD_Result);

  regs.es = 2;
  regs.dl = current_disk;
  regs.ah = 0x48; // GET-DRIVE-PARAMETERS
  regs.ds = 0;
  regs.si = 0x500;

  int_bios(0x13);

  sector_size = pres->sector_size;
  total_sectors = pres->total_sectors;

  printf("EDD total: %dkb\n",
		 (uint32_t)(total_sectors*sector_size)/1024);
}

static int edd_read_sector(void *buffer, uint64_t sector) {
  uint32_t off = (uint32_t)buffer&0xf;
  uint32_t seg = ((uint32_t)buffer&0xffff0)>>4;
  EDD_Address *padr = (EDD_Address*)0x500;
  memset(padr, 0, sizeof(EDD_Address));
  padr->size = sizeof(EDD_Address);
  padr->nblocks = 1;
  padr->buffer_off = off;
  padr->buffer_seg = seg;
  padr->sector = sector;

  regs.ah = 0x42; // EDD-READ
  regs.dl = current_disk;
  regs.ds = 0;
  regs.si = 0x500;
  int_bios(0x13);
  return regs.ah ? -1 : 1;
}

static int edd_write_sector(void *buffer, uint64_t sector) {
  uint32_t off = (uint32_t)buffer&0xf;
  uint32_t seg = ((uint32_t)buffer&0xffff0)>>4;
  EDD_Address *padr = (EDD_Address*)0x500;
  memset(padr, 0, sizeof(EDD_Address));
  padr->size = sizeof(EDD_Address);
  padr->nblocks = 1;
  padr->buffer_off = off;
  padr->buffer_seg = seg;
  padr->sector = sector;

  regs.al = 0;
  regs.ah = 0x43; // EDD-WRITE
  regs.dl = current_disk;
  regs.ds = 0;
  regs.si = 0x500;
  int_bios(0x13);
  return regs.ah ? -1 : 1;
}

void disk_select(int disk) {
  current_disk = disk;

  regs.dl = current_disk;
  regs.ah = 0x08; // READ-DRIVE-PARAMETERS
  int_bios(0x13);
  sector_size = 512;
  heads_per_drive = regs.dh + 1;
  tracks_per_head =  ((((int)regs.cl & 0xc0)<<2) | regs.ch) + 1;
  sectors_per_track = regs.cl & 0x3f;
  total_sectors = heads_per_drive * tracks_per_head * sectors_per_track;
  printf("disk:%s heads:%d tracks:%d sectors:%d total:%dkb\n",
		 disk_name(current_disk),
		 heads_per_drive, tracks_per_head, sectors_per_track,
		 (total_sectors*sector_size)/1024);

  edd_select();


  printf("\n");
  //abort();
}

//static void disk_stop_drive_motor() {
//  out(0x03F2, 0);
//}

static int old_reset() {
  regs.ah = 0;
  regs.dl = current_disk;
  int_bios(0x13);
  return regs.ah;
}

static int old_read_sectors(void *dst, uint32_t src, int nsectors) {
  int off = (uint32_t)dst&0xf;
  int seg = ((uint32_t)dst&0xffff0)>>4;
  uint32_t c,h,s;

  regs.ah = 0x02; // READ-SECTORS command
  regs.es = seg;
  regs.bx = off;
  regs.al = nsectors; // number of sectors to read

  regs.dl = current_disk; // drive number: 0x0 and 0x1 for floppies;
                          // 0x80 and 0x81 for hdds.

  lba2chs(src, &c, &h, &s);
  regs.dh = h; // head
  regs.ch = c; // cylinder (track)
  regs.cl = s; // sector
  int_bios(0x13);

  // NOTE: At most 1024 cylinders (numbered 0-1023), 256 heads
  // (numbered 0-255), 63 sectors/track (numbered 1-63) for a
  // maximum total capacity of 8455716864 bytes (8.5 GB). This is
  // a serious limitation today. It means that DOS cannot use
  // present day large disks. 

  return regs.ah;
}

static int old_read_sector(void *buffer, uint32_t sector) {
  int ntries = 5;

  while(ntries--) {
	if(old_reset()) continue;
	if(old_read_sectors(buffer, sector, 1)) continue;
	return 1;
  }

  return -1;
}

static int old_write_sector(void *buffer, uint32_t sector) {
  return -1;
}

#define min(a, b) ((a) < (b) ? (a) : (b))

int disk_read(void *dst, uint32_t src, int size) {
  uint8_t *p = dst;

  while(size) {
	int r;
	int sector = src / sector_size;
	int secoff = src % sector_size;
	int rdsz = min(sector_size-secoff, size);

	if(edd_version) edd_read_sector(MEM_DISK_BUF, sector);
	else r = old_read_sector(MEM_DISK_BUF, sector);

	if(r < 0) break;

	memcpy(p, MEM_DISK_BUF+secoff, rdsz);

	size -= rdsz;
	p += rdsz;
	src += rdsz;
  }

  return p - (uint8_t*)dst;
}

int disk_write(uint32_t dst, void *src, int size) {
  uint8_t *p = src;

  while(size) {
	int r;
	int sector = dst / sector_size;
	int secoff = dst % sector_size;
	int wrsz = min(sector_size-secoff, size);

	if(edd_version) edd_read_sector(MEM_DISK_BUF, sector);
	else r = old_read_sector(MEM_DISK_BUF, sector);
	if(r < 0) break;

	memcpy(MEM_DISK_BUF+secoff, p, wrsz);

	if(edd_version) edd_write_sector(MEM_DISK_BUF, sector);
	else r = old_write_sector(MEM_DISK_BUF, sector);
	if(r < 0) break;

	size -= wrsz;
	p += wrsz;
	src += wrsz;
  }

  return p - (uint8_t*)src;
}
