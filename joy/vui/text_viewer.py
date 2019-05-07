# -*- coding: utf-8 -*-
#
#    Copyright © 2019 Simon Forman
#
#    This file is part of Thun
#
#    Thun is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Thun is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Thun.  If not see <http://www.gnu.org/licenses/>.
#
'''

Text Viewer
=================

'''
import string
import pygame
from joy.utils.stack import expression_to_string
from joy.vui.core import (
    ARROW_KEYS,
    BACKGROUND as BG,
    FOREGROUND as FG,
    CommandMessage,
    ModifyMessage,
    OpenMessage,
    SUCCESS,
    push,
    )
from joy.vui import viewer, font_data
#reload(viewer)


MenuViewer = viewer.MenuViewer


SELECTION_COLOR = 235, 255, 0, 32
SELECTION_KEYS = {
    pygame.K_F1,
    pygame.K_F2,
    pygame.K_F3,
    pygame.K_F4,
    }
STACK_CHATTER_KEYS = {
    pygame.K_F5,
    pygame.K_F6,
    pygame.K_F7,
    pygame.K_F8,
    }


def _is_command(display, word):
    return display.lookup(word) or word.isdigit() or all(
        not s or s.isdigit() for s in word.split('.', 1)
        ) and len(word) > 1


def format_stack_item(content):
    if isinstance(content, tuple):
        return '[%s]' % expression_to_string(content)
    return str(content)


class Font(object):

    IMAGE = pygame.image.load(font_data.data, 'Iosevka12.BMP')
    LOOKUP = (string.ascii_letters +
              string.digits +
              '''@#$&_~|`'"%^=-+*/\\<>[]{}(),.;:!?''')

    def __init__(self, char_w=8, char_h=19, line_h=19):
        self.char_w = char_w
        self.char_h = char_h
        self.line_h = line_h

    def size(self, text):
        return self.char_w * len(text), self.line_h

    def render(self, text):
        surface = pygame.Surface(self.size(text))
        surface.fill(BG)
        x = 0
        for ch in text:
            if not ch.isspace():
                try:
                    i = self.LOOKUP.index(ch)
                except ValueError:
                    # render a lil box...
                    r = (x + 1, self.line_h / 2 - 3,
                         self.char_w - 2, self.line_h / 2)
                    pygame.draw.rect(surface, FG, r, 1)
                else:
                    iy, ix = divmod(i, 26)
                    ix *= self.char_w
                    iy *= self.char_h
                    area = ix, iy, self.char_w, self.char_h
                    surface.blit(self.IMAGE, (x, 0), area)
            x += self.char_w
        return surface

    def __contains__(self, char):
        assert len(char) == 1, repr(char)
        return char in self.LOOKUP


FONT = Font()


class TextViewer(MenuViewer):

    MINIMUM_HEIGHT = FONT.line_h + 3
    CLOSE_TEXT = FONT.render('close')
    GROW_TEXT = FONT.render('grow')

    class Cursor(object):

        def __init__(self, viewer):
            self.v = viewer
            self.x = self.y = 0
            self.w, self.h = 2, FONT.line_h
            self.mem = pygame.Surface((self.w, self.h))
            self.can_fade = False

        def set_to(self, x, y):
            self.fade()
            self.x, self.y = x, y
            self.draw()

        def draw(self):
            r = self.x * FONT.char_w, self.screen_y(), self.w, self.h
            self.mem.blit(self.v.body_surface, (0, 0), r)
            self.v.body_surface.fill(FG, r)
            self.can_fade = True

        def fade(self):
            if self.can_fade:
                dest = self.x * FONT.char_w, self.screen_y()
                self.v.body_surface.blit(self.mem, dest)
                self.can_fade = False

        def screen_y(self, row=None):
            if row is None: row = self.y
            return (row - self.v.at_line) * FONT.line_h

        def up(self, _mod):
            if self.y:
                self.fade()
                self.y -= 1
                self.x = min(self.x, len(self.v.lines[self.y]))
                self.draw()

        def down(self, _mod):
            if self.y < len(self.v.lines) - 1:
                self.fade()
                self.y += 1
                self.x = min(self.x, len(self.v.lines[self.y]))
                self.draw()
                self._check_scroll()

        def left(self, _mod):
            if self.x:
                self.fade()
                self.x -= 1
                self.draw()
            elif self.y:
                self.fade()
                self.y -= 1
                self.x = len(self.v.lines[self.y])
                self.draw()
                self._check_scroll()

        def right(self, _mod):
            if self.x < len(self.v.lines[self.y]):
                self.fade()
                self.x += 1
                self.draw()
            elif self.y < len(self.v.lines) - 1:
                self.fade()
                self.y += 1
                self.x = 0
                self.draw()
                self._check_scroll()

        def _check_scroll(self):
            if self.y < self.v.at_line:
                self.v.scroll_down()
            elif self.y > self.v.at_line + self.v.h_in_lines:
                self.v.scroll_up()

    def __init__(self, surface):
        self.cursor = self.Cursor(self)
        MenuViewer.__init__(self, surface)
        self.lines = ['']
        self.content_id = None
        self.at_line = 0
        self.bg = BG
        self.command = self.command_rect = None
        self._sel_start = self._sel_end = None

    def resurface(self, surface):
        self.cursor.fade()
        MenuViewer.resurface(self, surface)

        w, h = self.CLOSE_TEXT.get_size()
        self.close_rect = pygame.rect.Rect(self.w - 2 - w, 1, w, h)
        w, h = self.GROW_TEXT.get_size()
        self.grow_rect = pygame.rect.Rect(1, 1, w, h)

        self.body_surface = surface.subsurface(self.body_rect)
        self.line_w = self.body_rect.w / FONT.char_w + 1
        self.h_in_lines = self.body_rect.h / FONT.line_h - 1
        self.command_rect = self.command = None
        self._sel_start = self._sel_end = None

    def handle(self, message):
        if super(TextViewer, self).handle(message):
            return
        if (isinstance(message, ModifyMessage)
            and message.subject is self.lines
            ):
            # TODO: check self.at_line
            self.draw_body()

    # Drawing

    def draw_menu(self):
        #MenuViewer.draw_menu(self)
        self.surface.blit(self.GROW_TEXT, (1, 1))
        self.surface.blit(self.CLOSE_TEXT,
                          (self.w - 2 - self.close_rect.w, 1))
        if self.content_id:
            self.surface.blit(FONT.render('| ' + self.content_id),
                          (self.grow_rect.w + FONT.char_w + 3, 1))
        self.surface.fill( # light grey background
            (196, 196, 196),
            (0, 0, self.w - 1, self.MINIMUM_HEIGHT),
            pygame.BLEND_MULT
            )

    def draw_body(self):
        MenuViewer.draw_body(self)
        ys = xrange(0, self.body_rect.height, FONT.line_h)
        ls = self.lines[self.at_line:self.at_line + self.h_in_lines + 2]
        for y, line in zip(ys, ls):
            self.draw_line(y, line)

    def draw_line(self, y, line):
        surface = FONT.render(line[:self.line_w])
        self.body_surface.blit(surface, (0, y))

    def _redraw_line(self, row):
        try: line = self.lines[row]
        except IndexError: line = ' ' * self.line_w
        else:
            n = self.line_w - len(line)
            if n > 0: line = line + ' ' * n
        self.draw_line(self.cursor.screen_y(row), line)

    # General Functionality

    def focus(self, display):
        self.cursor.v = self
        self.cursor.draw()

    def unfocus(self):
        self.cursor.fade()

    def scroll_up(self):
        if self.at_line < len(self.lines) - 1:
            self._fade_command()
            self._deselect()
            self._sel_start = self._sel_end = None
            self.at_line += 1
            self.body_surface.scroll(0, -FONT.line_h)
            row = self.h_in_lines + self.at_line
            self._redraw_line(row)
            self._redraw_line(row + 1)
            self.cursor.draw()

    def scroll_down(self):
        if self.at_line:
            self._fade_command()
            self._deselect()
            self._sel_start = self._sel_end = None
            self.at_line -= 1
            self.body_surface.scroll(0, FONT.line_h)
            self._redraw_line(self.at_line)
            self.cursor.draw()

    def command_down(self, display, x, y):
        if self.command_rect and self.command_rect.collidepoint(x, y):
            return
        self._fade_command()
        line, column, _row = self.at(x, y)
        word_start = line.rfind(' ', 0, column) + 1
        word_end = line.find(' ', column)
        if word_end == -1: word_end = len(line)
        word = line[word_start:word_end]
        if not _is_command(display, word):
            return
        r = self.command_rect = pygame.Rect(
            word_start * FONT.char_w, # x
            y / FONT.line_h * FONT.line_h, # y
            len(word) * FONT.char_w, # w
            FONT.line_h # h
            )
        pygame.draw.line(self.body_surface, FG, r.bottomleft, r.bottomright)
        self.command = word

    def command_up(self, display):
        if self.command:
            command = self.command
            self._fade_command()
            display.broadcast(CommandMessage(self, command))

    def _fade_command(self):
        self.command = None
        r, self.command_rect = self.command_rect, None
        if r:
            pygame.draw.line(self.body_surface, BG, r.bottomleft, r.bottomright)

    def at(self, x, y):
        '''
        Given screen coordinates return the line, row, and column of the
        character there.
        '''
        row = self.at_line + y / FONT.line_h
        try:
            line = self.lines[row]
        except IndexError:
            row = len(self.lines) - 1
            line = self.lines[row]
            column = len(line)
        else:
            column = min(x / FONT.char_w, len(line))
        return line, column, row

    # Event Processing

    def body_click(self, display, x, y, button):
        if button == 1:
            _line, column, row = self.at(x, y)
            self.cursor.set_to(column, row)
        elif button == 2:
            if pygame.KMOD_SHIFT & pygame.key.get_mods():
                self.scroll_up()
            else:
                self.scroll_down()
        elif button == 3:
            self.command_down(display, x, y)
        elif button == 4: self.scroll_down()
        elif button == 5: self.scroll_up()

    def menu_click(self, display, x, y, button):
        if MenuViewer.menu_click(self, display, x, y, button):
            return True

    def mouse_up(self, display, x, y, button):
        if MenuViewer.mouse_up(self, display, x, y, button):
            return True
        elif button == 3 and self.body_rect.collidepoint(x, y):
            self.command_up(display)

    def mouse_motion(self, display, x, y, rel_x, rel_y, button0, button1, button2):
        if MenuViewer.mouse_motion(self, display, x, y, rel_x, rel_y,
                                   button0, button1, button2):
            return True
        if (button0
            and display.focused_viewer is self
            and self.body_rect.collidepoint(x, y)
            ):
            bx, by = self.body_rect.topleft
            _line, column, row = self.at(x - bx, y - by)
            self.cursor.set_to(column, row)
        elif button2 and self.body_rect.collidepoint(x, y):
            bx, by = self.body_rect.topleft
            self.command_down(display, x - bx, y - by)

    def close(self):
        self._sel_start = self._sel_end = None

    def key_down(self, display, uch, key, mod):

        if key in SELECTION_KEYS:
            self._selection_key(display, key, mod)
            return
        if key in STACK_CHATTER_KEYS:
            self._stack_chatter_key(display, key, mod)
            return
        if key in ARROW_KEYS:
            self._arrow_key(key, mod)
            return

        line, i = self.lines[self.cursor.y], self.cursor.x
        modified = ()
        if key == pygame.K_RETURN:
            self._return_key(mod, line, i)
            modified = True
        elif key == pygame.K_BACKSPACE:
            modified = self._backspace_key(mod, line, i)
        elif key == pygame.K_DELETE:
            modified = self._delete_key(mod, line, i)
        elif key == pygame.K_INSERT:
            modified = self._insert_key(display, mod, line, i)
        elif uch and uch in FONT or uch == ' ':
            self._printable_key(uch, mod, line, i)
            modified = True
        else:
            print '%r %i %s' % (uch, key, bin(mod))

        if modified:
            # The selection is fragile.
            self._deselect()
            self._sel_start = self._sel_end = None
            message = ModifyMessage(
                self, self.lines, content_id=self.content_id)
            display.broadcast(message)

    def _stack_chatter_key(self, display, key, mod):
        if key == pygame.K_F5:
            if mod & pygame.KMOD_SHIFT:
                command = 'roll<'
            else:
                command = 'swap'
        elif key == pygame.K_F6:
            if mod & pygame.KMOD_SHIFT:
                command = 'roll>'
            else:
                command = 'dup'
        elif key == pygame.K_F7:
            if mod & pygame.KMOD_SHIFT:
                command = 'tuck'
            else:
                command = 'over'
##        elif key == pygame.K_F8:
##            if mod & pygame.KMOD_SHIFT:
##                command = ''
##            else:
##                command = ''
        else:
            return
        display.broadcast(CommandMessage(self, command))

    # Selection Handling

    def _selection_key(self, display, key, mod):
        self.cursor.fade()
        self._deselect()
        if key == pygame.K_F1: # set sel start
            self._sel_start = self.cursor.y, self.cursor.x
            self._update_selection()
        elif key == pygame.K_F2: # set sel end
            self._sel_end = self.cursor.y, self.cursor.x
            self._update_selection()
        elif key == pygame.K_F3: # copy
            if mod & pygame.KMOD_SHIFT:
                self._parse_selection(display)
            else:
                self._copy_selection(display)
            self._update_selection()
        elif key == pygame.K_F4: # cut or delete
            if mod & pygame.KMOD_SHIFT:
                self._delete_selection(display)
            else:
                self._cut_selection(display)
        self.cursor.draw()

    def _deselect(self):
        if self._has_selection():
            srow, erow = self._sel_start[0], self._sel_end[0]
            # Just erase the whole selection.
            for r in range(min(srow, erow), max(srow, erow) + 1):
                self._redraw_line(r)

    def _copy_selection(self, display):
        if push(self, self._get_selection(), display.broadcast) == SUCCESS:
            return True
##        om = OpenMessage(self, 'stack.pickle')
##        display.broadcast(om)
##        if om.status == SUCCESS:
##            selection = self._get_selection()
##            om.thing[0] = selection, om.thing[0]
##            display.broadcast(ModifyMessage(
##                self, om.thing, content_id=om.content_id))

    def _parse_selection(self, display):
        if self._has_selection():
            if self._copy_selection(display):
                display.broadcast(CommandMessage(self, 'parse'))

    def _cut_selection(self, display):
        if self._has_selection():
            if self._copy_selection(display):
                self._delete_selection(display)

    def _delete_selection(self, display):
        if not self._has_selection():
            return
        self.cursor.fade()
        srow, scolumn, erow, ecolumn = self._selection_coords()
        if srow == erow:
            line = self.lines[srow]
            self.lines[srow] = line[:scolumn] + line[ecolumn:]
        else:
            left = self.lines[srow][:scolumn]
            right = self.lines[erow][ecolumn:]
            self.lines[srow:erow + 1] = [left + right]
        self.draw_body()
        self.cursor.set_to(srow, scolumn)
        display.broadcast(ModifyMessage(
            self, self.lines, content_id=self.content_id))

    def _has_selection(self):
        return (self._sel_start
                and self._sel_end
                and self._sel_start != self._sel_end)

    def _get_selection(self):
        '''Return the current selection if any as a single string.'''
        if not self._has_selection():
            return ''
        srow, scolumn, erow, ecolumn = self._selection_coords()
        if srow == erow:
            return str(self.lines[srow][scolumn:ecolumn])
        lines = []
        assert srow < erow
        while srow <= erow:
            line = self.lines[srow]
            e = ecolumn if srow == erow else len(line)
            lines.append(line[scolumn:e])
            scolumn = 0
            srow += 1
        return str('\n'.join(lines))

    def _selection_coords(self):
        (srow, scolumn), (erow, ecolumn) = (
            min(self._sel_start, self._sel_end),
            max(self._sel_start, self._sel_end)
            )
        return srow, scolumn, erow, ecolumn

    def _update_selection(self):
        if self._sel_start is None and self._sel_end:
            self._sel_start = self._sel_end
        elif self._sel_end is None and self._sel_start:
            self._sel_end = self._sel_start
        assert self._sel_start and self._sel_end
        if self._sel_start != self._sel_end:
            for rect in self._iter_selection_rectangles():
                self.body_surface.fill(
                    SELECTION_COLOR,
                    rect,
                    pygame.BLEND_RGBA_MULT
                    )

    def _iter_selection_rectangles(self, ):
        srow, scolumn, erow, ecolumn = self._selection_coords()
        if srow == erow:
            yield (
                scolumn * FONT.char_w,
                self.cursor.screen_y(srow),
                (ecolumn - scolumn) * FONT.char_w,
                FONT.line_h
                )
            return
        lines = self.lines[srow:erow + 1]
        assert len(lines) >= 2
        first_line = lines[0]
        yield (
            scolumn * FONT.char_w,
            self.cursor.screen_y(srow),
            (len(first_line) - scolumn) * FONT.char_w,
            FONT.line_h
            )
        yield (
            0,
            self.cursor.screen_y(erow),
            ecolumn * FONT.char_w,
            FONT.line_h
            )
        if len(lines) > 2:
            for line in lines[1:-1]:
                srow += 1
                yield (
                    0,
                    self.cursor.screen_y(srow),
                    len(line) * FONT.char_w,
                    FONT.line_h
                    )

    # Key Handlers

    def _printable_key(self, uch, _mod, line, i):
        line = line[:i] + uch + line[i:]
        self.lines[self.cursor.y] = line
        self.cursor.fade()
        self.cursor.x += 1
        self.draw_line(self.cursor.screen_y(), line)
        self.cursor.draw()

    def _backspace_key(self, _mod, line, i):
        res = False
        if i:
            line = line[:i - 1] + line[i:]
            self.lines[self.cursor.y] = line
            self.cursor.fade()
            self.cursor.x -= 1
            self.draw_line(self.cursor.screen_y(), line + ' ')
            self.cursor.draw()
            res = True
        elif self.cursor.y:
            y = self.cursor.y
            left, right = self.lines[y - 1:y + 1]
            self.lines[y - 1:y + 1] = [left + right]
            self.cursor.x = len(left)
            self.cursor.y -= 1
            self.draw_body()
            self.cursor.draw()
            res = True
        return res

    def _delete_key(self, _mod, line, i):
        res = False
        if i < len(line):
            line = line[:i] + line[i + 1:]
            self.lines[self.cursor.y] = line
            self.cursor.fade()
            self.draw_line(self.cursor.screen_y(), line + ' ')
            self.cursor.draw()
            res = True
        elif self.cursor.y < len(self.lines) - 1:
            y = self.cursor.y
            left, right = self.lines[y:y + 2]
            self.lines[y:y + 2] = [left + right]
            self.draw_body()
            self.cursor.draw()
            res = True
        return res

    def _arrow_key(self, key, mod):
        if key == pygame.K_UP: self.cursor.up(mod)
        elif key == pygame.K_DOWN: self.cursor.down(mod)
        elif key == pygame.K_LEFT: self.cursor.left(mod)
        elif key == pygame.K_RIGHT: self.cursor.right(mod)

    def _return_key(self, _mod, line, i):
        self.cursor.fade()
        # Ignore the mods for now.
        n = self.cursor.y
        self.lines[n:n + 1] = [line[:i], line[i:]]
        self.cursor.y += 1
        self.cursor.x = 0
        if self.cursor.y > self.at_line + self.h_in_lines:
            self.scroll_up()
        else:
            self.draw_body()
            self.cursor.draw()

    def _insert_key(self, display, mod, _line, _i):
        om = OpenMessage(self, 'stack.pickle')
        display.broadcast(om)
        if om.status != SUCCESS:
            return
        stack = om.thing[0]
        if stack:
            content = format_stack_item(stack[0])
            if self.insert(content):
                if mod & pygame.KMOD_SHIFT:
                    display.broadcast(CommandMessage(self, 'pop'))
                return True

    def insert(self, content):
        assert isinstance(content, basestring), repr(content)
        if content:
            self.cursor.fade()
            row, column = self.cursor.y, self.cursor.x
            line = self.lines[row]
            lines = (line[:column] + content + line[column:]).splitlines()
            self.lines[row:row + 1] = lines
            self.draw_body()
            self.cursor.y = row + len(lines) - 1
            self.cursor.x = len(lines[-1]) - len(line) + column
            self.cursor.draw()
            return True

    def append(self, content):
        self.cursor.fade()
        self.cursor.y = len(self.lines) - 1
        self.cursor.x = len(self.lines[self.cursor.y])
        self.insert(content)
