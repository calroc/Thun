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
from StringIO import StringIO
import base64, zlib


def create(fn='Iosevka12.BMP'):
    with open(fn, 'rb') as f:
        data = f.read()
    return base64.encodestring(zlib.compress(data))


data = StringIO(zlib.decompress(base64.decodestring('''\
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
lnalXc/9SsNb2vUirzS8pV0v8gJv/w/2vRht''')))


if __name__ == '__main__':
    print create()
