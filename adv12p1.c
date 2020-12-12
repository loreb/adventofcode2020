// I wanted to try SML, but it will be some other day...
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* F10
 * N3
 * F7
 * R90
 * F11
 * => east 10, north 3, east 7, turn right=>south, forward=>south11
 * => end in east 17, south 8 => 17+8 = 25 ("Manhattan distance")
 */

#ifdef NDEBUG
#define dbg(...)
#else
#define dbg(...) do { \
        fprintf(stderr, "line %u: ", __LINE__); \
        fprintf(stderr, __VA_ARGS__); \
} while(0)
#endif

static void chomp(char *s)
{
        size_t len = strlen(s);
        while (len > 0 && s[len - 1] == '\n')
                len--;
        s[len] = '\0';
}

enum Direction {
        N = 'N',                //Action N means to move north by the given value.
        S = 'S',                //Action S means to move south by the given value.
        E = 'E',                //Action E means to move east by the given value.
        W = 'W',                //Action W means to move west by the given value.
        L = 'L',                //Action L means to turn left the given number of degrees.
        R = 'R',                //Action R means to turn right the given number of degrees.
        F = 'F',                //Action F means to move forward by the given value in the direction the ship is currently facing.
};
static const enum Direction directions[] = {
        N, E, S, W              // +1 => turn right, -1 => turn left
};

struct Pos {
        int x, y;
};

static void move(struct Pos *p, unsigned int delta, enum Direction d)
{
        dbg("move %d,%d by %u in direction %c\n", p->x, p->y, delta, d);
        switch (d) {
        case N:
                p->y += delta;
                break;
        case E:
                p->x += delta;
                break;
        case S:
                p->y -= delta;
                break;
        case W:
                p->x -= delta;
                break;
        default:
                fprintf(stderr, "move %c???\n", d);
                exit(100);
        }
}

static void part1(const char *filename)
{
        FILE *fp = fopen(filename, "rb");
        if (!fp) {
                perror(filename);
                exit(111);
        }
        int diridx = 0;
        // we start east
        while (directions[diridx] != E) {
                diridx++;
                diridx&=3;
        }
        enum Direction dir = directions[diridx];
        struct Pos pos = { 0, 0 };
        char line[123];
        while (fgets(line, sizeof line, fp)) {
                if (strlen(line) < 2) {
                        fprintf(stderr, "dafuk %s?\n", line);
                        exit(100);
                }
                chomp(line);
                unsigned mov = 0;
                char dummy = '!';
                if (sscanf(&line[1], "%u%c", &mov, &dummy) != 1) {
                        fprintf(stderr, "bad line(%s)\n", line);
                        exit(100);
                }
                if (mov > 0x1234) {
                        fprintf(stderr, "move too big in %s!\n", line);
                        exit(100);
                }
                int turn = 1;
                // goto case N would make things easier...
                switch (line[0]) {
                case N:
                case E:
                case S:
                case W:
                        move(&pos, mov, line[0]);
                        break;
                case L:
                        // L,R: L90 means "change left 90 degrees"
                        turn = -1; // fallthru
                case R:
                        if (mov%90) {
                                fprintf(stderr, "turn %c by %u degrees?\n", line[0], mov);
                                exit(100);
                        }
                        while(mov > 0) {
                                mov -= 90;
                                diridx += turn;
                                diridx &= 3;
                        }
                        dir = directions[diridx];
                        //move(&pos, mov, dir);
                        dbg("%s => direction is %c now\n", line, dir);
                        break;
                case F:
                        move(&pos, mov, dir);
                        break;
                default:
                        fprintf(stderr, "unknown direction(%s)\n", line);
                        exit(123);
                }
                dbg("%d,%d\n", pos.x, pos.y);
        }
        fclose(fp);
        printf("x=%d,y=%d => Manhattan dist is %d\n", pos.x, pos.y,
               abs(pos.x) + abs(pos.y));
}

int main(int argc, char *argv[])
{
        if (argc < 2) {
                fprintf(stderr, "usage: $0 file...\n");
                return 100;
        }
        for (int i = 1; i < argc; i++)
                part1(argv[i]);
        return 0;
}
