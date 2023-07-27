

def f(string, start=0, acc=[]):
    if start >= len(string): return acc
    if '[' == string[start]: return [1] + f(string, start+1, acc)
    if ']' == string[start]: return [0] + f(string, start+1, acc)
    if ' ' == string[start]: return f(string, start+1, acc)
    symbol, n = bar(string, start, start)
    return [symbol] + f(string, n, acc)


def bar(string, start, end):
    if end >= len(string) or string[end] in '[] ':
        return string[start:end], end
    return bar(string, start, end+1)


print(f("1[2[] 3]4"))
