#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2017, 2019 Simon Forman
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
from textwrap import dedent


setup(
	name='Thun',
	version='0.4.2',
	description='Python Implementation of Joy',
	long_description=dedent('''\
		Joy is a programming language created by Manfred von Thun that is easy to
		use and understand and has many other nice properties.  This Python
		package implements an interpreter for a dialect of Joy that attempts to
		stay very close to the spirit of Joy but does not precisely match the
		behaviour of the original version written in C.'''),
	author='Simon Forman',
	author_email='sforman@hushmail.com',
	url='https://joypy.osdn.io',
	license='GPLv3+',
	packages=['joy', 'joy.utils'],
	classifiers=[
		'Development Status :: 4 - Beta',
		'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
		'Programming Language :: Python :: 3',
		'Programming Language :: Other',
		'Topic :: Software Development :: Interpreters',
		],
	extras_require={
		'build-docs':  [
			'sphinx',
			'ipython',
			'nbconvert',
			],
		}
	)
