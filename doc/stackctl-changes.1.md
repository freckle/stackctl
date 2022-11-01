% STACKCTL-CHANGES(1) User Manual
%
% March 2022

# NAME

stackctl changes - Create and present Change Sets for stack specifications

# SYNOPSIS

*stackctl changes* \[options]

# DESCRIPTION

For each stack specification in the currently-active AWS Account and Region,
creates a Change Set and prints it. The Change Set is not removed after
successful operation.

# OPTIONS

**\-f**, **\--format** *\<FORMAT\>*\

> Output changes in **FORMAT**. See dedicated section.

**PATH**\

> Where to write the changes summary.
>
> This is a required option to make the interaction with logging explicit. You
> can pass */dev/stdout* if you want the changes written alongside any logging
> and don't mind interleaving or ordering problems that may occur.

# AVAILABLE FORMATS

**tty**\

> The default. Produces a simplified but colorized (unless redirected) listing.

**pr**\

> Produces markdown suitable to post as a comment to a GitHub Pull Request.

# STACKCTL

Part of the **stackctl(1)** suite
