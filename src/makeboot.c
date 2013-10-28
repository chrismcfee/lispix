#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argnr, char *args[])
{
  FILE *output, *input;
  int i, bytes_read, sectors_read, bytes_from_file;
  char buffer[512];

  if (argnr < 4) {
    printf("Invalid number of parameters.\n\n");
    printf("USAGE: %s [output] [input 1] [input 2] ... [input n]\n", args[0]);
    printf("Example: %s a.img bootsect.bin kernel.bin\n", args[0]);
    exit(0);
  }

  output = fopen(args[1], "r");

  if (output != NULL) {
    buffer[0] = 0;
    //while (buffer[0] != 'y' && buffer[0] != 'Y') {
    //  printf("%s already exists. Overwrite(y/n)?\n ", args[1]);
    //  scanf("%s", buffer);
    //  if (buffer[0] == 'n' || buffer[0] == 'N')
    //    exit(0);
    //}
  	fclose(output);
  }
  output = fopen(args[1], "wb");

  for (i = 2; i < argnr; i++) {
    input = fopen(args[i], "rb");

    if (input == NULL) {
      printf("Missing input file %s. Aborting operation...\n", args[i]);
      fclose(output);
      exit(1);
    }

    bytes_read = 512;
    bytes_from_file = 0;
    sectors_read = 0;

    while(bytes_read == 512 && !feof(input)) {
      bytes_read = fread(buffer, 1, 512, input);

      if (bytes_read == 0)
        break;

      if (bytes_read != 512)
        memset(buffer+bytes_read, 0, 512-bytes_read);

      sectors_read++;
      fwrite(buffer, 1, 512, output);
      bytes_from_file += bytes_read;
    }

    printf("%d sectors, %d bytes read from file %s...\n", sectors_read, bytes_from_file, args[i]);

    fclose(input);
  }

  fclose(output);
  return 0;
}
