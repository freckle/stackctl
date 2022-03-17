% STACKCTL(1) User Manual
%
% March 2022

# NAME

stackctl - Manage CloudFormation Stacks through specifications

# SYNOPSIS

*stackctl* \<options\> \<command\> \<args>

# OPTIONS

**\--color** *\<auto|always|never\>*\

> When to colorize output. **auto** (the default) will colorize output when
> connected to a terminal.

**\-v**, **\--verbose**\

> Log more verbosely

**-d**, **\--directory** *\<PATH\>*\

> Where to find specifications. Default is **.**.

**\--filter** *\<PATTERN[,PATTERN]\>*\

> Restrict specifications to those whose paths match any of the given
> **PATTERN**s.

# COMMANDS

**cat**\

> Pretty-print specifications.

**capture**\

> Generate specifications from deployed Stacks.

**changes**\

> Show changes between specifications and deployed state.

**deploy**\

> Make deployed state match specifications.

**version**\

> Print the CLI's version.

See individual command man-pages for details.

# CONCEPTS

TODO: copy *STACK_SPEC.md*

# AUTHOR

Freckle Engineering <freckle-engineering@renaissance.com>

# ENVIRONMENT

**AWS_PROFILE** \

> If set, will be used as account name in subcommands that create new
> specifications.

# SEE ALSO

**stackctl-cat(1)**, **stackctl-capture(1)**, **stackctl-changes(1)**,
**stackctl-deploy(1)**, **stackctl-version(1)**.
