import curses

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
    def __init__(self):
        self.falling_piece = Blocks.L_BLOCK
        self.rot = 0
        self.fy = 0
        self.fx = 5 - 2
        self.grid       = [[0 for _ in range(10)] for _ in range(20)]
        self.colors     = [[4 for _ in range(10)] for _ in range(20)]



relx = lambda frac: int(curses.COLS * frac)
rely = lambda frac: int(curses.LINES * frac)

game = Tetris()

def draw_grid(box):
    for y in range(20):
        for x in range(10):
            box.addstr(1 + y, 1 + x*2, '  ' if game.grid[y][x] == 0 else '[]',
                curses.color_pair(game.colors[y][x]))

def draw_falling_piece(box):
    for y in range(4):
        for x in range(4):
            ay = game.fy + y
            ax = game.fx + x
            if (ay >= 0 and ay < 20 and
                ax >= 0 and ax < 10):
                
                box.addstr(1 + ay, 1 + ax*2, '  ' if game.falling_piece[game.rot][y][x] == 0 else '[]',
                    curses.color_pair(6))
    

def intro(stdscr):
    curses.curs_set(0)
    curses.flash()
    stdscr.border()
    intro_str = [\
    r'_____    _        _      ',
    '|_   _|  | |      (_)    ',
    '  | | ___| |_ _ __ _ ___ ',
    '  | |/ _ \ __| |__| / __|',
    '  | |  __/ |_| |  | \__ \\',
    '  \_/\___|\__|_|  |_|___/']
    
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
    while 1:
        stdscr.clear()
        
        box = curses.newwin(22, 22, rely(0.5) - 11, relx(0.5) - 11)
        box.box()
        draw_grid(box)
        draw_falling_piece(box)
        box.refresh()
        pass

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
    curses.wrapper(cli)



