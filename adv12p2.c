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

/*
--- Part Two ---
Before you can give the destination to the captain, you realize that the actual action meanings were printed on the back of the instructions the whole time.

Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:

Action N means to move the waypoint north by the given value.
Action S means to move the waypoint south by the given value.
Action E means to move the waypoint east by the given value.
Action W means to move the waypoint west by the given value.
Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
Action F means to move forward to the waypoint a number of times equal to the given value.
The waypoint starts 10 units east and 1 unit north relative to the ship. The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
*/
enum Direction {
        N = 'N',
        S = 'S',
        E = 'E',
        W = 'W',
        L = 'L',
        R = 'R',
        F = 'F',
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

// waypoint = "unit vector" => move by it n times;
// eg wp=(1,2), n=3 => move by (3,6)
static void move2waypoint(struct Pos *p, unsigned int n,
                          struct Pos waypoint)
{
        enum Direction x = waypoint.x > 0 ? E : W;
        enum Direction y = waypoint.y > 0 ? N : S;
        int dx = abs(waypoint.x);
        int dy = abs(waypoint.y);
        while (n-- > 0) {
                move(p, dx, x);
                move(p, dy, y);
        }
}

static struct Pos rotate(struct Pos waypoint, int clockwise)
{
        if (!clockwise) {
                for (int i = 0; i < 3; i++)
                        waypoint = rotate(waypoint, 1);
                return waypoint;
        } else {
                // matrix multiplication:
                // newx = x * cos - y * sin
                // newy = x * sin + y * cos
                //
                // clockwise => sin = -1, cos = 0
                struct Pos old = waypoint;
                waypoint.x = old.y;
                waypoint.y = 0 - old.x;
                return waypoint;
        }
}

static void part1(const char *filename)
{
        FILE *fp = fopen(filename, "rb");
        if (!fp) {
                perror(filename);
                exit(111);
        }
        /*
           int diridx = 0;
           // we start east
           while (directions[diridx] != E) {
           diridx++;
           diridx &= 3;
           }
           enum Direction dir = directions[diridx];
         */
        struct Pos waypoint = { 10, 1 };
        struct Pos ship = { 0, 0 };
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
                int clockwise = 1;
                // goto case N would make things easier...
                switch (line[0]) {
                case N:
                case E:
                case S:
                case W:
                        move(&waypoint, mov, line[0]);
                        break;
                case L:
                        // L,R: L90 means "change left 90 degrees"
                        // now ROTATE THE WAYPOINT!
                        clockwise = 0;  // fallthru
                case R:
                        if (mov % 90) {
                                fprintf(stderr, "turn %c by %u degrees?\n",
                                        line[0], mov);
                                exit(100);
                        }
                        while (mov > 0) {
                                mov -= 90;
                                waypoint = rotate(waypoint, clockwise);
                        }
                        dbg("%s => waypoint rotated to %d,%d\n", line,
                            waypoint.x, waypoint.y);
                        break;
                case F:
                        //Action F means to move forward to the waypoint a number of times equal to the given value.
                        move2waypoint(&ship, mov, waypoint);
                        break;
                default:
                        fprintf(stderr, "unknown direction(%s)\n", line);
                        exit(123);
                }
                dbg("%d,%d\n", ship.x, ship.y);
        }
        fclose(fp);
        printf("x=%d,y=%d => Manhattan dist is %d\n", ship.x, ship.y,
               abs(ship.x) + abs(ship.y));
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
