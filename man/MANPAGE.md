% atsfmt (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

atsfmt - a source code formatter for ATS

# SYNOPSIS

  atsfmt \<file\>

  atsfmt -i \<file\>

  atsfmt -\-default-config

  ac file.dats | atsfmt

  atsfmt --default-config

# DESCRIPTION

**atsfmt** is an opinionated formatter for that ATS2 language.

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-o** **-\-no-config**
:   Ignore configuration files in scope

**-i**
:   Modify a file in-place.

**-\-default-config**
:   Generate a default configuration file in the current directory

# CONFIGURATION

**atsfmt** is configured using a TOML file, by default .atsfmt.toml. You can
generate a default configuration with

```
atsfmt --default-config
```

To make **atsfmt** call clang-format on embedded C code, add the following to
your .atsfmt.toml

```
clang-format = true
```

# EDITOR INTEGRATION

Editor integration is available with the ATS vim plugin at:

https://github.com/vmchale/ats-vim

# COPYRIGHT

Copyright 2017. Vanessa McHale. All Rights Reserved.
