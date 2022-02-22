from tkinter import *

UTF_8 = 'UTF_8'
lorem = '''Dicta enim ducimus et ullam est culpa quasi. Quo placeat quis et alias pariatur cupiditate nemo. Quibusdam reiciendis deleniti aut distinctio ea. Natus voluptas aut soluta sed quos voluptate repudiandae. Ea enim veritatis nesciunt sit doloribus. At iure alias voluptatibus sed magnam cum non. Itaque occaecati cupiditate quia commodi in magni autem non. Qui odio accusantium aut qui quaerat. Dolores exercitationem aut consequatur qui dolorum corrupti et et. Hic id aut adipisci assumenda et non. Ipsam et qui reiciendis laborum modi aut aspernatur et. Dolore consequuntur voluptas deleniti omnis incidunt. Accusantium deleniti eum sed quibusdam. Temporibus voluptate nihil eveniet vel cupiditate est. Ut perferendis quis illo et ipsum. Libero ut necessitatibus quis soluta laudantium. Illum ad possimus fugit voluptatem. Quam aliquid aspernatur tenetur et voluptatem inventore tenetur. Temporibus qui et a reiciendis et tenetur saepe. Sit id et omnis ab. Itaque rerum maxime nostrum corporis eveniet dolor culpa. Provident itaque neque tenetur dicta aut deleniti omnis.'''
lorem = '''\
The swift brown fox
jumperd over the lazy dog.
'''
fn = 'lorem.txt'
with open(fn, mode='w', encoding=UTF_8) as F:
    F.write(lorem)

def get_file_text(offset, size):
    with open(fn, mode='rb') as F:
        F.seek(offset)
        return F.read(size).decode(UTF_8)

T = Text()
T.pack(expand=True, fill=BOTH)
T.insert(END, lorem)

get_text = lambda: T.get('1.0', END)[:-1]


def select_callback(event):
    sel = T.tag_ranges(SEL)
    if not sel:
        return
    begin, end = sel
    pre = T.get('1.0', begin)
    selection = T.get(begin, end)
    prelen = len(pre.encode(UTF_8))
    sellen = len(selection.encode(UTF_8))
    file_text = get_file_text(prelen, sellen)
    if selection == file_text:
        print(prelen, sellen, True)
    else:
        # end is <textindex object: '4.0'>
        # T.index(END) is '4.0'
        # end != T.index(END)
        # boo  (TODO: report bug)
        if T.index(end) == T.index(END):
            # Tk Text inserts an extra newline at
            # the end of its string.
            if selection[:-1] == file_text:
                assert selection[-1] == '\n', repr(selection[-1])
                print(prelen, sellen - 1, True, '!')
            else:
                # Selection is at the end of the string
                # But there's someting unexpected going on.
                print(
                    begin, end,
                    repr(selection),
                    repr(file_text),
                    )
                raise ValueError('1')
        else:
            # We're not at the end of the string,
            # But there's someting unexpected going on.
            print(
                begin, end,
                repr(selection),
                repr(file_text),
                )
            raise ValueError('2')

T.bind('<ButtonRelease-1>', select_callback)


##for i, ch in enumerate(get_text()):
##    tki = T.index('1.0 + %i chars' % i)
##    print(i, tki, repr(ch))
