import multiprocessing as mp
from multiprocessing.connection import wait
from joy import (
    default_defs,
    initialize,
    joy,
    repl,
    get_n_items,
    isnt_stack,
    inscribe,
    )


class ForkException(Exception): pass


def fork_joy(send, stack, expr, dictionary):
    try:
        stack, dictionary = joy(stack, expr, dictionary)
        result, _ = get_n_items(1, stack)
    except Exception as err:
        send.send((True, repr(err)))
    else:
        send.send((False, result))


@inscribe
def fork(stack, expr, dictionary):
    '''
    Take two quoted programs from the stack and
    run them in parallel.

        fork ≡ [i] app2

    '''
    q, p, stack = get_n_items(2, stack)
    isnt_stack(q)
    isnt_stack(p)
    q_pipe_recv, q_pipe_send = mp.Pipe(False)
    p_pipe_recv, p_pipe_send = mp.Pipe(False)

    P = mp.Process(
        target=fork_joy,
        args=(p_pipe_send, stack, p, dictionary),
        )
    Q = mp.Process(
        target=fork_joy,
        args=(q_pipe_send, stack, q, dictionary),
        )
    P.start()
    Q.start()

    # Ensure "wait() will promptly report the readable end as being ready."
    # See docs for multiprocessing.connection.wait().
    q_pipe_send.close()
    p_pipe_send.close()

    readers = [q_pipe_recv, p_pipe_recv]
    ready = wait(readers)
    # We have one or two results or errors
    if len(ready) == 1:
        read_me = ready[0]
        order = read_me is q_pipe_recv
        wait_me = readers[order]
        wait_proc = (Q, P)[order]
        stack = one_result(stack, order, read_me, wait_me, wait_proc)
    else:  # both results/errors
        p_err, p_result = p_pipe_recv.recv()
        q_err, q_result = q_pipe_recv.recv()
        if p_err:
            raise ForkException(p_result)
        if q_err:
            raise ForkException(q_result)
        stack = (q_result, (p_result, stack))

    return stack, expr, dictionary


def one_result(stack, order, read_me, wait_me, wait_proc):
    err, result = read_me.recv()
    read_me.close()
    if err:
        wait_me.close()
        wait_proc.kill()
        raise ForkException(result)
    wait([wait_me])
    err, second_result = wait_me.recv()
    wait_me.close()
    if err:
        raise ForkException(second_result)
    if order:
        stack = (result, (second_result, stack))
    else:
        stack = (second_result, (result, stack))
    return stack


if __name__ == '__main__':
    mp.set_start_method('fork')

    dictionary = initialize()
    default_defs(dictionary)
    try:
        stack = repl(dictionary=dictionary)
    except SystemExit:
        pass
