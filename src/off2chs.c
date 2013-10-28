#include <stdio.h>
#include <stdlib.h>


#define SECTOR_SIZE 512
#define SECTORS 18 /* sectors per track */
#define HEADS 2
#define CYLINDERS 80

int main(int argc, char **argv) {
	int lba, c,h,s;
	if(argc != 2) {
		printf("Usage: off2chs <offset>\n");
		exit(1);
	}
	lba = atoi(argv[1]);
	c = lba / (SECTORS * HEADS);
	h = (lba / SECTORS) % HEADS;
	s = lba % SECTORS + 1;
	printf("c,h,s = %d,%d,%d\n", c,h,s);
	return 0;
}

