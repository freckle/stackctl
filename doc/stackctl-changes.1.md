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

**PATH**\

> Where to write the changes summary.
>
> This is a required option to make the interaction with logging explicit. In
> addition to a filepath, this option recognizes three special values:
>
> - **-**: send the changes to *stdout*. This can be useful if you want to pipe
>   the output somewhere and are either OK with interleaved logging output, or
>   have worked around it somehow (such as configuring logging to *stderr*)
> - **@log**, **@logger**: send the changes in the stream alongside other
>   logging. This is useful if you just want to view the changes and not capture
>   or process them. Using **@log** instead of **-** will keep things correctly
>   ordered and prevent interleaving.

# AVAILABLE FORMATS

**tty**\

> The default. Produces a simplified but colorized (unless redirected) listing.

**pr**\

> Produces markdown suitable to post as a comment to a GitHub Pull Request.

# STACKCTL

Part of the **stackctl(1)** suite
