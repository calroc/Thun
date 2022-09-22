import multiprocessing as mp
from joy import (
    initialize,
    default_defs,
    text_to_expression,
    joy,
    )


if __name__ == '__main__':
    mp.set_start_method('fork')
