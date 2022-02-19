# Thun Documentation Documentation

Most of the docs take the form of some [Jupyter](https://jupyter.org/index.html) notebooks.

The notebooks are converted (using [nbconvert](https://nbconvert.readthedocs.io/en/latest/install.html)) to HTML, Markdown, and ReST files so you can view them without running Jupyter.

## Running the Notebooks with [Jupyter](https://jupyter.org/index.html)

In order to run the [Jupyter](https://jupyter.org/index.html) notebooks
you need [Jupyter](https://jupyter.org/index.html) (obviously):

[Installing the classic Jupyter Notebook interface](https://jupyter.readthedocs.io/en/latest/install/notebook-classic.html)

And, of course, you should install `Thun` (see the main project README or the
[online docs](https://joypy.osdn.io/#quick-start)).

Once that's done you should be able to start Jupyter Notebook server
in the `joypy/docs` directory and run the notebooks.


## ``notebook_preamble.py``

...something about the `notebook_preamble.py` file.


## Installing the [Joy Jupyter Kernel](https://osdn.net/projects/joypy/scm/git/Thun/tree/master/docs/jupyter_kernel/)

[Joy Jupyter Kernel](https://osdn.net/projects/joypy/scm/git/Thun/tree/master/docs/jupyter_kernel/)

Tracking down the deets:
- [Installing Kernels](https://jupyter.readthedocs.io/en/latest/install/kernels.html)
- [Kernels](https://jupyter.readthedocs.io/en/latest/projects/kernels.html#kernels-langs)
- [Jupyter kernels](https://github.com/ipython/ipython/wiki/IPython-kernels-for-other-languages)
- [Jupyter kernels](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels)
- [Creating new Jupyter kernels](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels#creating-new-jupyter-kernels)
- "[Making kernels for Jupyter](https://jupyter-client.readthedocs.io/en/latest/kernels.html) in the documentation."

From [Making simple Python wrapper kernels](https://jupyter-client.readthedocs.io/en/latest/wrapperkernels.html):

> Now create a JSON kernel spec file and install it using
  ``jupyter kernelspec install </path/to/kernel>``.
  Place your kernel module anywhere Python can import it
  (try current directory for testing). 

The current list of kernels:

    % jupyter kernelspec list
    Available kernels:
      python3    /usr/home/sforman/.local/share/jupyter/kernels/python3

> Place your kernel module anywhere Python can import it

Yah, okay.

    sforman@bock:~/src/Joypy/docs/jupyter_kernel % setenv PYTHONPATH `pwd`

Let's go!

    sforman@bock:~/src/Joypy/docs/jupyter_kernel % jupyter kernelspec install .
    [Errno 13] Permission denied: '/usr/local/share/jupyter'
    Perhaps you want to install with `sudo` or `--user`?

Okay

    sforman@bock:~/src/Joypy/docs/jupyter_kernel % jupyter kernelspec install --user .
    [InstallKernelSpec] Removing existing kernelspec in /usr/home/sforman/.local/share/jupyter/kernels/.
    Traceback (most recent call last):
      File "/home/sforman/.local/bin/jupyter-kernelspec", line 8, in <module>
        sys.exit(KernelSpecApp.launch_instance())
      File "/home/sforman/.local/lib/python3.7/site-packages/traitlets/config/application.py", line 846, in launch_instance
        app.start()
      File "/home/sforman/.local/lib/python3.7/site-packages/jupyter_client/kernelspecapp.py", line 323, in start
        return self.subapp.start()
      File "/home/sforman/.local/lib/python3.7/site-packages/jupyter_client/kernelspecapp.py", line 151, in start
        replace=self.replace,
      File "/home/sforman/.local/lib/python3.7/site-packages/jupyter_client/kernelspec.py", line 404, in install_kernel_spec
        shutil.rmtree(destination)
      File "/usr/local/lib/python3.7/shutil.py", line 498, in rmtree
        onerror(os.rmdir, path, sys.exc_info())
      File "/usr/local/lib/python3.7/shutil.py", line 496, in rmtree
        os.rmdir(path)
    OSError: [Errno 22] Invalid argument: '/usr/home/sforman/.local/share/jupyter/kernels/.'

__Looks at code__ Oh FFS

    cp -Rv /usr/home/sforman/src/Joypy/docs/jupyter_kernel /usr/home/sforman/.local/share/jupyter/kernels/thun

    /usr/home/sforman/src/Joypy/docs/jupyter_kernel -> /usr/home/sforman/.local/share/jupyter/kernels/thun
    /usr/home/sforman/src/Joypy/docs/jupyter_kernel/Try out the Joypy Jupyter Kernel.ipynb -> /usr/home/sforman/.local/share/jupyter/kernels/thun/Try out the Joypy Jupyter Kernel.ipynb
    /usr/home/sforman/src/Joypy/docs/jupyter_kernel/joy_kernel.py -> /usr/home/sforman/.local/share/jupyter/kernels/thun/joy_kernel.py
    /usr/home/sforman/src/Joypy/docs/jupyter_kernel/kernel.json -> /usr/home/sforman/.local/share/jupyter/kernels/thun/kernel.json

Done.  Can start joy kernal notebooks.

## Building and Uploading

This section is mostly for my own reference.


### `backup-and-remove-htdocs` script

On the OSDN server I have a little script I call `backup-and-remove-htdocs`
which does exactly what it says.  Here are the current contents:

    #!/usr/bin/env bash
    tar cvz --remove-files \
      -f $HOME/site-backup-$(date -u +'%F-%T').tgz \
      /home/groups/j/jo/joypy/htdocs/*

As you can see, all it does is move the existing site into a tarball (in case I want to refer to it later for some reason.)
Every once in a while they should be cleared out.

The main `Makefile` uses this script via ssh then uses rsync to upload the new version of the site.

## Table of Contents

- 1. Basic Use of Joy in a Notebook
- 2. Library Examples - Short examples of each word in the dictionary.
  Various formats.
- 3. Developing a Program - Working with the first problem from Project
  Euler, "Find the sum of all the multiples of 3 or 5 below 1000",
  several forms of the program are derived.
- 4. Replacing Functions in the Dictionary - Shows the basics of defining
  new "primitive" functions in Python or as definitions and adding them
  to the dictionary.
- Factorial Function and Paramorphisms - A basic pattern of recursive
  control-flow.
- Generator Programs - Using the x combinator to make generator programs
  which can be used to create unbounded streams of values.
- Hylo-, Ana-, Cata-morphisms - Some basic patterns of recursive
  control-flow structures.
- Quadratic - Not-so-annoying Quadratic Formula.
- Trees - Ordered Binary Trees in Joy and more recursion.
- Zipper - A preliminary examination of the idea of data-structure
  "zippers" for traversing datastructures.
- notebook_preamble.py - Imported into notebooks to simplify the preamble
  code.
- pe1.py pe1.txt - Set up and execute a Joy program for the first problem
  from Project Euler. The pe1.txt file is the trace.  It's 2.8M
  uncompressed.  Compressed with gzip it becomes just 0.12M.
- repl.py - Run this script to start a REPL.  Useful for e.g. running Joy
  code in a debugger.

## Notes

One of the things that interests me about Joy is how programming becomes
less about writing code and more about sound reasoning about simple
(almost geometric) programs.  Many of the notebooks in this collection
consist of several pages of discussion to arrive at a few lines of Joy
definitions.  I think this is a good thing.  This is "literate
programming".  The "programs" resemble mathematical proofs.  You aren't
implementing so much as deriving.  The structure of Joy seems to force
you to think clearly about the task in a way that is reliable but
extremely flexible.  It feels like a puzzle game, and the puzzles are
often simple, and the solutions build on each other.
