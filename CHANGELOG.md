# Changelog

All changes that impact users of this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!---
This document is intended for the users of astrolisp. Changes to things
like tests should not be noted in this document.
When updating this file for a PR, add an entry for your change under Unreleased
and one of the following headings:

 - Added - for new features.
 - Changed - for changes in existing functionality.
 - Deprecated - for soon-to-be removed features.
 - Removed - for now removed features.
 - Fixed - for any bug fixes.
 - Security - in case of vulnerabilities.
-->

## Changes

### Changed
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

changed namespace from isistools to astrolisp as it reflects the overall nature
and intention of the package more clearly.

### Added
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

Added marci2isis, thm2isis, clem2isis, tgocassis2isis, pds2isis, caminfo, and
cathist to available functions.

### Changed
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

Updated README documentation to reflect package changes and added easier
installation directions. Changed a few descriptions.

### Added
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

Added skypt, csminit, and spicefit basic implementations.

### Changed
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

Changed project structure, going to hold different mission componenent, ale
components, spice components, isis components, etc. in their own respective
files(and folders in certain cases like ale, spice, and others) under /lisp
which will help keep changes more delineated as the project expands and
grows.

Structure is as follows:

astrolisp
├── CHANGELOG.md
├── LICENSE.md
├── README.org
├── astrolisp.el
└── lisp
    ├── UI.el
    ├── ale.el
    ├── isis3.el
    ├── keybinds.el
    ├── macros.el
    ├── menubar.el
    ├── metadata.el
    ├── missions
    │   ├── apollo.el
    │   ├── cassini.el
    │   ├── chandrayaan.el
    │   ├── clementine.el
    │   ├── clipper.el
    │   ├── dawn.el
    │   ├── galileo.el
    │   ├── hayabusa.el
    │   ├── juno.el
    │   ├── kaguya.el
    │   ├── lro.el
    │   ├── mariner.el
    │   ├── mer.el
    │   ├── messenger.el
    │   ├── mex.el
    │   ├── mgs.el
    │   ├── mro.el
    │   ├── near.el
    │   ├── newhorizons.el
    │   ├── odyssey.el
    │   ├── osirisrex.el
    │   ├── rolo.el
    │   ├── rosetta.el
    │   ├── socet.el
    │   ├── tgo.el
    │   ├── viking.el
    │   └── voyager.el
    ├── pixel.el
    ├── spice
    │   ├── spice.el
    └── usgscsm
        └── csm.el
