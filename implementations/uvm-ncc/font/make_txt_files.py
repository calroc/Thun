
txt = (
    'abcdefghijklmnopqrstuvwxyz'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  '''0123456789@#$&_~|`'"%^=-+*'''
   r'/\<>[]{}(),.;:!?'
    )

for i, ch in enumerate(txt):
    open(f'{i:02}.txt', 'w').write(ch)
