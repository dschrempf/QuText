# QuText

Map a quantum text file to a fixed target.

QuText provides a simple framework to prepare similar but slightly different
text files for different targets. This may be config files that differ in some
lines depending on the host name or a welcome message that is adjusted
according to the user's class.

The idea is, that the text file can be edited in the respective mode (e.g.,
cabal mode for this file), but some lines will be changed by QuText according
to the need of the user. The changes should be specified within the comments
of the edited file (i.e., the command syntax for QuText depends on the comment
characters used by the text file).

Why do I write this? Because I use different computers, configuration files
are vastly similar but differ in a few lines. I always have to keep all
configuration files but would like to keep one file that stores all tweaks for
the different target systems.

Let those target systems be called targetA and targetB. The first version will
be a simple converter from *.XYZ for any target to *.XYZ for any other (or the
same) target.
