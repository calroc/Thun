#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#    Copyright © 2020 Simon Forman
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
from setuptools import setup


setup(
	name='Xerblin',
	version='0.1.0',
	description='GUI for Joy',
	long_description='Experimental GUI for the Joy programming language.',
	author='Simon Forman',
	author_email='sforman@hushmail.com',
	url='https://joypy.osdn.io',
	license='GPLv3+',
	packages=['xerblin', 'xerblin.gui', 'xerblin.vui'],
	scripts = ['scripts/xerblin'],
	classifiers=[
		'Development Status :: 4 - Beta',
		'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
		'Programming Language :: Python :: 3',
		'Programming Language :: Other',
		'Topic :: Software Development :: Interpreters',
		],
	install_requires=[
		'Thun',
		'dulwich',
		'future',
		],
	extras_require={'build-docs': ['sphinx']}
	)
