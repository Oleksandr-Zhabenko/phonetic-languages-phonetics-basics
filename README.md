The version 0.9.0.0 fixed the long existing issues with not working prepending needed concatenations for the text in the
module Data.Phonetic.Languages.PrepareText. Added also the possibility to not only prepend needed words, but also append.
This introduces breaking changes, so please, wait for update for all the dependent packages before starting the upgrade, also
check the code that uses library functions afterwards.

The version 0.7.0.0 introduces breaking changes, so the related code must be revised and probably rewritten.
The changes are related with the introducing the possibilities to represent also appearance of the sounds
in the written text being pronounced and the introduction more general concept of phonemes that creates
syllable instead of simpler vowel notion.

The executable pldPL is intended to use the functionality of the :

1) R programming language https://www.r-project.org/

2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html

3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html

For more information, please, see the documentation for them.

* Some examples.

Let in the file words.txt you have the (phonetic language) words and their durations in seconds as pairs separated with whitespace
at the lines.

Also let in the file controlData.txt you have control specifications, an example of syntaxis for which is in the file
controlDataExample.txt.

Also let in the file gwrsys.txt you have GWritingSystemPRPLX specifications, an example of syntaxis for which is in the
file gwrsysExample.txt.

Idea of Basic Usage
===================

* You can use the executable in the procedure of minimizing the sets of values in the following way.

The commands below can be variants of the usage of the pldPL executable.

1) pldPL 0.001 0.001 0.01 0.02 words.txt controlData.txt gwrsys.txt 0.3 0.2 0.02 0.06 1.8 -1 0.01 0.01 1.5 1.5 1.5 1.5 1.5 1.5 | R --quiet --no-save

This variant tries to minimize the duration of the only one element belonging to the special phonetic language representation
elements (-1 as the sixth parameter after the file names), tries
to reduce the influence of the two other elements (the seventh and the eighth parameters as 0.01) and tries to somewhat
make longer the 6 vowels (all the consonants have by that the default coefficients equal to 1.0) (a simple example, though
the program supports also the more complicated ones).

2) pldPL 0.001 0.001 0.01 0.02 words.txt controlData.txt gwrsys.txt 0.3 0.2 0.02 0.06 1.8 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save

The same as the 1), but with more prolongation of the vowels.

3) pldPL 0.001 0.001 0.01 0.02 words.txt controlData.txt gwrsys.txt 0.3 0.2 0.02 0.06 1.4 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save

If the 2) gives you the not NULL result, then it is recommended to minimize the first parameter after the next 4 parameters
after the gwrsys.txt.

4) pldPL 0.001 0.001 0.01 0.02 words.txt controlData.txt gwrsys.txt 0.3 0.2 0.02 0.06 1.6 -1 0.01 0.01 2.5 2.5 2.5 2.5 2.5 2.5 | R --quiet --no-save

If the previous gives NULL result, then try to somewhat increase the first parameter after the next 4 parameters
after the gwrsys.txt.

Therefore, using such a binary search for the single parameter above, try to find out the more precise value of the point where
the pldPL gives not NULL result and then use the obtained result as the durations for the system of phonetic languages representations.

It is needed to be said that if omitted, the durations coefficients are equal to the default 1.0.

An example of the pldPL functionality already in use is the pldUkr executable of the
r-glpk-phonetic-languages-ukrainian-durations package. For more information, please,
refer to: https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations

----------------------------------------------------------

In the file EnglishConcatenated.txt in the source tarball there are English words the translations of which are intended to be
concatenated to the next word in the phonetic language before applying sound processing so that the basic grammar is preserved.

Devotion
========

The author would like to devote this project to support the [Foundation
Gastrostars](https://gastrostars.nl).

On the 22/09/2023 there is Nathalie's Kok birthday, the mother of the founder of the Foundation,
[Emma Kok](https://www.emmakok.nl), and the member of the Foundation board.

On the 06/01/2024 there was Sophie's Kok, a sister of Emma Kok, 19th Birthday (she is 18). Therefore, the version 0.11.0.0 is additionally devoted also to her.

All support is welcome, including donations for the needs of the Ukrainian army, IDPs and refugees. 

If you would like to share some financial support with the Foundation Gastrostars, please, contact the mentioned foundation
using the URL:

[Contact Foundation GASTROSTARS](https://gastrostars.nl/hou-mij-op-de-hoogte)

or 

[Donation Page](https://gastrostars.nl/doneren)

