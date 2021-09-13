# ml2vim
Fork of ml2vim (found [here](https://library.wolfram.com/infocenter/MathSource/2584/) ) by Timo Felbinger.
I fixed some compatibility issues with modern MathLink installation and deprecated function calls.

Description taken from the original site:

This is a Mathematica front end built into Vim, which is a high-end programmer's editor, highly compatible to vi but with lots of additional features. The front end is just an add-on for the editor, which can still be used as a standard editor for all kinds of ASCII files.

The front end is a "text-mode' application which will run in any terminal (including, of course, xterms). The functionality is similar to the standard notebook interface, without graphics (there is, however, built-in support for external viewers like ghostview), but with greatly enhanced editing capabilities. A sample file can be found at http://www.quantum.physik.uni-potsdam.de/TF/samplevma.html The editor has extensive online documentation (covering the front end, too).

You need two packages: The editor Vim with the front end extensions (current version: vim-5.4g-mma0.10), and a small auxiliary program, ml2vim, which translates between Vim and the kernel (current version: ml2vim-0.99.8).

Both packages come as source code (precompiled packages for Linux/Intel and True64 Unix/Alpha are available, too) and can be copied freely. Vim is charityware (see README.txt for details), ml2vim is covered by the GNU General Public license.

Only UNIX(-like) systems are supported. So far, this front end has been successfully tested on Linux/Intel and True64 Unix/Alpha.
