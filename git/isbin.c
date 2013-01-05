#include <stdio.h>
#include <sys/mman.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <strings.h>

#include <sys/stat.h>
#include <unistd.h>

#define CHECKSIZE (1*1024)
#define MIN(x,y) ((x)>(y)?(y):(x))

int checkbuf[256];
static int my_isprint(unsigned char c)
{
    return isprint(c) || isspace(c);
}

void x()
{
    printf("%c %c %c %c %c %c %c %c\n", 194, 174, 181, 226, 128, 153, 162, 132);
}

int main(int argc, char **argv)
{
    int fd;
    unsigned char *c;
    int i;
    struct stat sb;

    fd = open(argv[1], O_RDONLY);
    printf("%s\n", argv[1]);
    if (fd < 0) {
        fprintf(stderr, "failed to open %s\n",argv[1]);
        exit(1);
    }
    if (stat(argv[1], &sb) < 0) {
        fprintf(stderr, "failed to stat %s\n", argv[1]);
        exit(1);
    }

    c = mmap(0, CHECKSIZE, PROT_READ, MAP_PRIVATE, fd, 0);
    if (c == 0) {
        fprintf(stderr, "failed to map\n");
        exit(1);
    }

    for (i = 0; i < MIN(CHECKSIZE, sb.st_size); i++) {
        if (!(my_isprint(c[i]))) {
            if (i > 0 && (c[i-1]==0xc2 || c[i-1]==0xc3)) {
                continue;
            }
//            fprintf(stderr, "not printable %d @ %d %s\n", c[i], i, argv[1]);
            {
                char buf[2048];
                sprintf(buf, argv[1], argv[2]);
                printf("exe: <%s>\n", buf);
//                system(buf);
                printf("bin %s %d %d\n", argv[1], i, c[i]);
                exit(1);
            }
        }
    }
    printf("text %s\n", argv[1]);
}
