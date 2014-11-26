import curses
import time
import random

class Blocks():
    LN_BLOCK = (
        ((0,1,0,0),
         (0,1,0,0),
         (0,1,0,0),
         (0,1,0,0)),
        ((0,0,0,0),
         (1,1,1,1),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (0,1,0,0),
         (0,1,0,0),
         (0,1,0,0)),
        ((0,0,0,0),
         (1,1,1,1),
         (0,0,0,0),
         (0,0,0,0))
        )
    L_BLOCK = (
        ((0,1,0,0),
         (0,1,0,0),
         (0,1,1,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (1,1,1,0),
         (1,0,0,0),
         (0,0,0,0)),
        ((1,1,0,0),
         (0,1,0,0),
         (0,1,0,0),
         (0,0,0,0)),
        ((0,0,1,0),
         (1,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    RL_BLOCK = (
        ((1,0,0,0),
         (1,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,1,0),
         (0,1,0,0),
         (0,1,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (1,1,1,0),
         (0,0,1,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (0,1,0,0),
         (1,1,0,0),
         (0,0,0,0)),
        )
    S_BLOCK = (
        ((0,0,0,0),
         (0,1,1,0),
         (1,1,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (0,1,1,0),
         (0,0,1,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,1,1,0),
         (1,1,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (0,1,1,0),
         (0,0,1,0),
         (0,0,0,0)),
        )
    RS_BLOCK = (
        ((0,0,0,0),
         (1,1,0,0),
         (0,1,1,0),
         (0,0,0,0)),
        ((0,0,1,0),
         (0,1,1,0),
         (0,1,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (1,1,0,0),
         (0,1,1,0),
         (0,0,0,0)),
        ((0,0,1,0),
         (0,1,1,0),
         (0,1,0,0),
         (0,0,0,0)),
        )
    T_BLOCK = (
        ((1,1,1,0),
         (0,1,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (1,1,0,0),
         (0,1,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (1,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,0,0),
         (0,1,1,0),
         (0,1,0,0),
         (0,0,0,0)),
        )
    SQ_BLOCK = (
        ((0,1,1,0),
         (0,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,1,0),
         (0,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,1,0),
         (0,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,1,1,0),
         (0,1,1,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    COLORS = {
        LN_BLOCK    : 7,
        L_BLOCK     : 2,
        RL_BLOCK    : 3,
        S_BLOCK     : 4,
        RS_BLOCK    : 5,
        T_BLOCK     : 6,
        SQ_BLOCK    : 7,
    }
    

class Tetris():
    def __init__(self, box):
        self.box = box
        self.grid       = [[0 for _ in range(10)] for _ in range(20)]
        self.colors     = [[8 for _ in range(10)] for _ in range(20)]
        # your actual falling piece
        self.falling_piece = Blocks.L_BLOCK
        self.check_falling = None
        self.rot, self.fy, self.fx = (0, 0, 0)
        # shodaw of your piece
        self.sy = 0
        # lines to remove next tick
        self.toremove = []

        

    def new_piece(self):
        self.rot = 0
        self.fy = 0
        self.fx = 5 - 2
        # new falling piece and only the pairs you have to check
        self.falling_piece = Blocks.COLORS.keys()[random.randint(0,6)]
        self.check_falling = [(y, x) for x in range(4) for y in range(4)]
            # filter(lambda t: self.falling_piece[t[0]][t[1]] == 1, 
                # [(y, x) for x in range(4) for y in range(4)])

    def calc_shadow(self):
        old_fy = self.fy
        while self.check_down():
            self.fy += 1
        self.sy = self.fy
        self.fy = old_fy

    def check_rot(self):
        for y, x in self.check_falling:
            ay = self.fy + y
            ax = self.fx + x
            # not inside box OR collision
            if (self.falling_piece[(self.rot + 1) % 4][y][x] == 1
                and 
                ((ax < 0 or ax >= 10) or 
                (ay >= 0 and ay < 20 and
                 ax >= 0 and ax < 10
                 and self.grid[ay][ax] == 1))):
                return False
        return True

    def check_side(self, side):
        for y, x in self.check_falling:
            ay = self.fy + y
            ax = self.fx + x + side
            # not inside box OR collision
            if (self.falling_piece[self.rot % 4][y][x] == 1
                and 
                ((ax < 0 or ax >= 10) or 
                (ay >= 0 and ay < 20 and
                 ax >= 0 and ax < 10
                 and self.grid[ay][ax] == 1))):
                return False
        return True

    def check_down(self):
        for y, x in self.check_falling:
            ay = self.fy + y + 1
            ax = self.fx + x
            if (self.falling_piece[self.rot % 4][y][x] == 1
                and 
                (ay >= 20 or 
                (ay >= 0 and ay < 20 and
                 ax >= 0 and ax < 10
                 and self.grid[ay][ax] == 1))):
                return False
        return True

    def commit_piece(self):
        for y, x in self.check_falling:
            ay = self.fy + y
            ax = self.fx + x
            if (ay >= 0 and ay < 20 and
                ax >= 0 and ax < 10):
                if self.falling_piece[self.rot % 4][y][x]:
                    self.grid[ay][ax] = self.falling_piece[self.rot % 4][y][x]
                    self.colors[ay][ax] = Blocks.COLORS[self.falling_piece]
        for y in range(20):
            line = True
            for x in range(10):
                if self.grid[y][x] == 0:
                    line = False
                    break
            if line:
                self.toremove.append(y)
                self.colors[y] = [1] * 10
    
    def remove_lines(self):
        for y in self.toremove:
            self.colors.remove(self.colors[y])
            self.colors.insert(0, [8] * 10)
            self.grid.remove(self.grid[y])
            self.grid.insert(0, [0] * 10)
        self.toremove = []

    
    def draw(self):
        self.draw_grid(self.box)
        self.draw_shadow(self.box)
        self.draw_falling_piece(self.box)
        self.box.refresh()

    def draw_grid(self, box):
        box.box()
        for y in range(20):
            for x in range(10):
                paint = self.colors[y][x] if self.grid[y][x] == 1 else 8
                box.addstr(1 + y, 1 + x*2, '  ', curses.color_pair(paint))

    def draw_falling_piece(self, box):
        for y, x in self.check_falling:
            ay = self.fy + y
            ax = self.fx + x
            if (ay >= 0 and ay < 20 and
                ax >= 0 and ax < 10):
                if self.falling_piece[self.rot % 4][y][x] == 1:
                    box.addstr(1 + ay, 1 + ax*2, '  ', 
                        curses.color_pair(Blocks.COLORS[self.falling_piece]))

    def draw_shadow(self, box):
        for y, x in self.check_falling:
            ay = self.sy + y
            ax = self.fx + x
            if (ay >= 0 and ay < 20 and
                ax >= 0 and ax < 10):
                if self.falling_piece[self.rot % 4][y][x] == 1:
                    box.addstr(1 + ay, 1 + ax*2, '[]', curses.color_pair(8))



intro_str = [\
    r'_____    _        _      ',
    '|_   _|  | |      (_)    ',
    '  | | ___| |_ _ __ _ ___ ',
    '  | |/ _ \ __| |__| / __|',
    '  | |  __/ |_| |  | \__ \\',
    '  \_/\___|\__|_|  |_|___/']

relx = lambda frac: int(curses.COLS * frac)
rely = lambda frac: int(curses.LINES * frac)

game = None
input = None

def intro(stdscr):
    curses.curs_set(0)
    curses.flash()
    stdscr.border()
    x, y = relx(.5) - len(intro_str[0])/2, rely(.5) - len(intro_str)/2 - 6
    for i in range(len(intro_str)):
        y += 1
        stdscr.addstr(y, x, intro_str[i], curses.color_pair(i + 2))

    curses.flash()
    stdscr.getch()
    stdscr.clear()
    stdscr.refresh()
    set_solid_colors()
    loop(stdscr)


def loop(stdscr):
    stdscr.nodelay(1)
    key = ''        
    box = curses.newwin(22, 22, rely(0.5) - 11, relx(0.5) - 11)
    game = Tetris(box)
    game.new_piece()
    last_update = time.time()

    while 1:
        key = stdscr.getch()
        dirty = False
        drop = False
        if key == ord('q'):
            return
        elif key == ord(' '):
            game.fy = game.sy
            dirty = True
            drop = True
        if key == curses.KEY_LEFT and game.check_side(-1):
            game.fx -= 1
            dirty = True
        elif key == curses.KEY_RIGHT and game.check_side(1): 
            game.fx += 1
            dirty = True
        elif key == curses.KEY_UP and game.check_rot(): 
            game.rot += 1
            dirty = True
        elif key == curses.KEY_UP and game.check_rot(): 
            game.rot += 1
            dirty = True


        if drop or time.time() - last_update > 0.2:
            last_update = time.time()
            game.remove_lines()
            if not game.check_down():
                game.commit_piece()
                game.new_piece()
            game.fy += 1
            dirty = True

        if dirty:
            game.calc_shadow()

        game.draw()

        time.sleep(0.01)

def set_normal_colors():
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_BLUE, curses.COLOR_BLACK)
    curses.init_pair(4, curses.COLOR_MAGENTA, curses.COLOR_BLACK)
    curses.init_pair(5, curses.COLOR_CYAN, curses.COLOR_BLACK)
    curses.init_pair(6, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(7, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    curses.init_pair(8, curses.COLOR_BLACK, curses.COLOR_WHITE)
def set_solid_colors():
    curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_WHITE)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_RED)
    curses.init_pair(3, curses.COLOR_BLACK, curses.COLOR_BLUE)
    curses.init_pair(4, curses.COLOR_BLACK, curses.COLOR_MAGENTA)
    curses.init_pair(5, curses.COLOR_BLACK, curses.COLOR_CYAN)
    curses.init_pair(6, curses.COLOR_BLACK, curses.COLOR_GREEN)
    curses.init_pair(7, curses.COLOR_BLACK, curses.COLOR_YELLOW)
    curses.init_pair(8, curses.COLOR_WHITE, curses.COLOR_BLACK)

def cli(stdscr):
    # curses.use_default_colors()
    set_normal_colors()
    intro(stdscr)

if __name__ == '__main__':
    stdscr = curses.wrapper(cli)

