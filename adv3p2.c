#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define openSquare '.'
#define tree '#'
struct Row {
        char data[256];
        size_t len;
};

// as in good ol' C64 games, they wrap around horizontally...
static char row_at(const struct Row *r, size_t i)
{
        return r->data[i % r->len];
}

// count # of trees met with the given dx/dy
static size_t trees_encountered(const struct Row *grid, size_t nrows, size_t dx, size_t dy)
{
        size_t ntrees = 0, x = 0, y = 0;
        while (y < nrows) {
                if (row_at(&grid[y], x) == tree)
                        ntrees++;
                x += dx;
                y += dy;
        }
        return ntrees;
}

int main(int argc, char *argv[])
{
        FILE *in = stdin;
        switch(argc) {
        case 1:
                break;
        case 2:
                if (!(in = fopen(argv[1], "rb"))) {
                        perror(argv[1]);
                        return 111;
                }
                break;
        default:
                fprintf(stderr, "usage: $0 [inputfile]\n");
                return 100;
        }
        struct Row *grid = NULL;
        size_t nrows = 0;
        for(;;) {
                struct Row dummy;
                if (!fgets(dummy.data, sizeof dummy.data, in)) {
                        break;
                }
                dummy.data[strspn(dummy.data, ".#")] = '\0';
                dummy.len = strlen(dummy.data);
                nrows++;
                if (dummy.len < 1) { // ever confused strcspn/strspn?
                        fprintf(stderr, "empty row in line %zu?\n", nrows);
                        return 123;
                }
                if (!(grid = reallocarray(grid, nrows, sizeof(grid[0])))) {
                        perror("realloc");
                        return 111;
                }
                grid[nrows-1] = dummy;
        }
        struct { size_t dx,dy; } slope[] = {
                {1,1},
                {3,1},
                {5,1},
                {7,1},
                {1,2}
        };
        size_t product = 1;
        for (size_t i = 0; i < sizeof(slope) / sizeof(slope[0]); i++) {
                size_t n = trees_encountered(grid, nrows, slope[i].dx, slope[i].dy);
                printf("%zu\n", n);
                product *= n;
        }
        printf("product = %zu\n", product);
        return 0;
}
