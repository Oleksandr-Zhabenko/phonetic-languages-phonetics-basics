# Revision history for phonetic-languages-phonetics-basics

## 0.1.0.0 -- 2021-04-19

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2021-04-19

* Second version. Added some optimizations for the Data.Phonetic.Languages.Base module functions. Added new functions to
the module.

## 0.2.1.0 -- 2021-04-20

* Second version revised A. Fixed issues with being not compiled for GHC versions less than 8.2.

## 0.2.2.0 -- 2021-04-20

* Second version revised B. Fixed issues with being not compiled because of no Ix instance specified for Array first argument.

## 0.3.0.0 -- 2021-04-20

* Third version. Added a new module Data.Phonetic.Languages.Syllables for the general syllable segmentation. Added new
lightweight dependencies of mmsyn5 and mmsyn2-array.

## 0.3.1.0 -- 2021-04-21

* Third version revised A. Changed some data types and type synonyms in the module Data.Phonetic.Languages.Syllables.
Some code optimizations in it. Some documentation improvements. Added new auxiliary functions to the module.

## 0.3.2.0 -- 2021-04-21

* Third version revised B. Fixed issue with semantics of the 'divCnsnts' function and related data types.

## 0.3.3.0 -- 2021-04-24

* Third version revised C. Some code improvements. Added classes and instances for classes. Some changes to imrove overall
representation possibilities.

## 0.3.4.0 -- 2021-04-26

* Third version revised D. Changed the data types representation in the Data.Phonetic.Languages.Syllables module. Used some
special function refactoring to the algebraic data type to represent a complex predicate of the special structure. Some code
improvements.

## 0.4.0.0 -- 2021-04-28

* Fourth version. Added a new module Numeric.Wrapper.R.GLPK.Phonetic.Languages.Durations to find out the approximations of
the phonetic languages representation element durations. For this added two new lightweight dependencies of lists-flines
and foldable-ix.

## 0.5.0.0 -- 2021-04-29

* Fifth version. Added two new modules Data.Phonetic.Languages.SpecificationsRead and Main. Added an executable pldPL (anologue
of the pldUkr from r-glpk-phonetic-languages-ukrainian-durations package).

## 0.5.1.0 -- 2021-04-30

* Fifth version revised A. Added two example files with data syntaxis, first of all useful for the pldPL executable. See README.md.
Added also README.md file with a brief description of the possible scenario of the usage for pldPL. Some code improvements.

## 0.6.0.0 -- 2021-05-03

* Sixth version. Added a module Data.Phonetic.Languages.PrepareText. Added a file EnglishConcatenated.txt that contains additional
list of the English words the corresponding translations of which are intended to be concatenated to the next phonetic language
words to preserve the basic grammar. Some code and documentation improvements.

## 0.6.0.1 -- 2021-05-04

* Sixth version revised A. Extended the data in the EnglishConcatenated.txt file. Fixed some issues with the README.md file.

## 0.6.1.0 -- 2021-05-07

* Sixth version revised B. Fixed issues with wrong order of the concatenations in the Data.Phonetic.Languages.PrepareText
functions. Added some new functions for this.

## 0.6.2.0 -- 2021-05-10

* Sixth version revised C. Some documentation improvements. Fixed issues with being not compiled because of 2 doc-style comments
(thanks to: Jonathan Ringer at notifications@github.com).

## 0.6.3.0 -- 2021-05-19

* Sixth version revised D. Added Read and added / changed Show instances to the data types in the SegmentRulesG data type in the
Data.Phonetic.Languages.Syllables module.

## 0.7.0.0 -- 2021-06-04

* Seventh version. Introduces breaking changes, so the related code must be revised and probably rewritten.
The changes are related with the introducing the possibilities to represent also appearance of the sounds
in the written text being pronounced and the introduction more general concept of phonemes that creates
syllable instead of simpler vowel notion.
Removed the module Data.Phonetic.Languages.Undefined as one being not necessary.

## 0.8.0.0 -- 2021-07-24

* Eigth version. Added new functions to the Data.Phonetic.Languages.PrepareText module to 'grow' the phonetic
languages text. Some code improvements in the module.

## 0.8.1.0 -- 2021-07-24

* Eigth version revised A. Changed the prepareGrowTextN function (and renamed it to prepareGrowTextMN) so that it
has some additional meaning.

## 0.8.2.0 -- 2021-08-30

* Eigth version revised B. Removed deprecated filtering from the Data.Phonetic.Languages.PrepareText module.

## 0.8.3.0 -- 2021-08-31

* Eigth version revised C. Fixed issue with splitLinesN function.

## 0.8.4.0 -- 2021-09-04

* Eigth version revised D. Added new 'tuned' functions to the Data.Phonetic.Languages.PrepareText module.

## 0.9.0.0 -- 2022-02-09

* Ninth version. Fixed the long existing issues with not working prepending needed concatenations for the text in the
module Data.Phonetic.Languages.PrepareText. Added also the possibility to not only prepend needed words, but also append.
This introduces breaking changes, so please, wait for update for all the dependent packages before starting the upgrade, also
check the code that uses library functions afterwards.

## 0.9.1.0 -- 2022-09-13

* Ninth version revised A. Updated the dependencies boundaries. Some minor documentation improvements.

## 0.10.0.0 -- 2023-02-03

* Tenth version. Switched to NoImplicitPrelude extension. Updated the metadata and dependencies boundaries.

## 0.10.0.1 -- 2023-02-18

* Tenth version revised A. Fixed issue with integer conversions that caused build to fail.

## 0.10.0.2 -- 2023-02-18

* Tenth version revised B. Fixed issue with integer conversions that caused build to fail.

## 0.10.1.0 -- 2023-09-22

* Tenth version revised C. Fixed issues with README.md file. Changed the data types inner
  representations where possible to use one less level of redirection for the compiler (experimental
and not well tested move). Added a devotion to the README.md file.

## 0.11.0.0 -- 2024-01-30

* Eleventh version. Removed deprecated dependencies, switched to more lightweight ones. Some minor code improvements. Added bug-tracker.

