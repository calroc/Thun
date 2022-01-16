

def step(stack, expression, dictionary):
    (program, (seq, stack)) = stack
    while seq:
        item, seq = seq
        stack = joy((item, stack), program, dictionary)[0]
    return stack, expression, dictionary


def step_jit(stack, expression, dictionary):
    (program, (seq, stack)) = stack
    try:
        p = joyc(program)
    except:
        return step(stack, expression, dictionary)
    while seq:
        item, seq = seq
        stack = p((item, stack), (), dictionary)[0]
    return stack, expression, dictionary


def step_exact_semantics(stack, expression, dictionary):
    (program, (seq, stack)) = stack
    if seq:
        item, seq = seq
        stack = item, stack
        expression = concat(program, (seq, (program, (expression))))
    return stack, expression, dictionary



# [+] step
def fn(stack, expression, dictionary):
    (s1, (i1, stack)) = stack
    while s1:
        (i2, s1) = s1
        i3 = i2 + i1
        (i1, stack) = (i3, stack)
    return (i1, stack), expression, dictionary
