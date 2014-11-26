import curses
import time
from threading import Thread

class Blocks():
    L_BLOCK = (
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
    RL_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    S_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    RS_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    T_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    LN_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )
    SQ_BLOCK = (
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        ((0,0,0,0),
         (0,0,0,0),
         (0,0,0,0),
         (0,0,0,0)),
        )

class Tetris():
    def __init__(self, box):
        self.box = box
        self.falling_piece = Blocks.L_BLOCK
        self.rot = 0
        self.fy = 0
        self.fx = 5 - 2
        self.grid       = [[0 for _ in range(10)] for _ in range(20)]
        self.colors     = [[4 for _ in range(10)] for _ in range(20)]

    def new_piece(self):
        self.falling_piece = Blocks.L_BLOCK
        self.rot = 0
        self.fy = 0
        self.fx = 5 - 2


    def check_rot(self):
        for y in range(4):
            for x in range(4):
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
        for y in range(4):
            for x in range(4):
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
        for y in range(4):
            for x in range(4):
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
        for y in range(4):
            for x in range(4):
                ay = self.fy + y
                ax = self.fx + x
                if (ay >= 0 and ay < 20 and
                    ax >= 0 and ax < 10):
                    self.grid[ay][ax] = min(self.grid[ay][ax] + 
                        self.falling_piece[self.rot % 4][y][x], 1)
    
    def draw(self):
        self.draw_grid(self.box)
        self.draw_falling_piece(self.box)
        self.box.refresh()


    def draw_grid(self, box):
        box.box()
        for y in range(20):
            for x in range(10):
                box.addstr(1 + y, 1 + x*2, 
                    '  ' if self.grid[y][x] == 0 else '[]',
                    curses.color_pair(self.colors[y][x]))

    def draw_falling_piece(self, box):
        for y in range(4):
            for x in range(4):
                ay = self.fy + y
                ax = self.fx + x
                if (ay >= 0 and ay < 20 and
                    ax >= 0 and ax < 10):
                    
                    if self.falling_piece[self.rot % 4][y][x] == 1:
                        box.addstr(1 + ay, 1 + ax*2, '[]', curses.color_pair(6))

class Input(Thread):
    def __init__(self, stdscr, game):
        Thread.__init__(self)
        self.stdscr = stdscr
        self.game = game
    
    def run(self):
        while 1:
            key = self.stdscr.getch()
            
            if key == ord('q'):
                return

            if key == curses.KEY_LEFT and self.game.check_side(-1):
                self.game.fx -= 1
            elif key == curses.KEY_RIGHT and self.game.check_side(1): 
                self.game.fx += 1
            elif key == curses.KEY_UP and self.game.check_rot(): 
                self.game.rot += 1


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
    loop(stdscr)


def loop(stdscr):
    stdscr.nodelay(1)
    key = ''        
    box = curses.newwin(22, 22, rely(0.5) - 11, relx(0.5) - 11)
    game = Tetris(box)

    # input = Input(stdscr, game)
    # input.daemon = True
    # input.start()

    last_update = time.time()
    while 1:
        key = stdscr.getch()
        if key == ord('q'):
            return
        if key == curses.KEY_LEFT and game.check_side(-1):
            game.fx -= 1
        elif key == curses.KEY_RIGHT and game.check_side(1): 
            game.fx += 1
        elif key == curses.KEY_UP and game.check_rot(): 
            game.rot += 1

        game.draw()

        if time.time() - last_update > 0.2:
            last_update = time.time()
            if not game.check_down():
                game.commit_piece()
                game.new_piece()
            game.fy += 1

        time.sleep(0.01)

def cli(stdscr):
    # curses.use_default_colors()
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_BLUE, curses.COLOR_BLACK)
    curses.init_pair(4, curses.COLOR_MAGENTA, curses.COLOR_BLACK)
    curses.init_pair(5, curses.COLOR_CYAN, curses.COLOR_BLACK)
    curses.init_pair(6, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(7, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    intro(stdscr)

if __name__ == '__main__':
    stdscr = curses.wrapper(cli)
    