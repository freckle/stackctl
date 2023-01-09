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

**\-p**, **\--parameter** *\<KEY=[VALUE]\>*\

> Override the given Parameter for this operation. Omitting *VALUE* will result
> in overriding the Parameter as an empty string. May be specified 0 or more
> times.

**\-t**, **\--tag** *\<KEY=[VALUE]\>*\

> Override the given Tag for this operation. Omitting *VALUE* will result in
> overriding the Tag as an empty string. May be specified 0 or more times.

**PATH**\

> Write changes to **PATH**, instead of printing them.

# AVAILABLE FORMATS

**tty**\

> The default. Produces a simplified but colorized (unless redirected) listing.

**pr**\

> Produces markdown suitable to post as a comment to a GitHub Pull Request.

# STACKCTL

Part of the **stackctl(1)** suite
