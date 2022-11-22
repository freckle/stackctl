% STACKCTL-DEPLOY(1) User Manual
%
% March 2022

# NAME

stackctl deploy - deploy stack specifications

# SYNOPSIS

*stackctl deploy* \[options]

# DESCRIPTION

For each stack specification in the currently-active AWS Account and Region,
creates a Change Set and executes it after confirmation.

# OPTIONS

**\-p**, **\--parameter** *\<KEY=[VALUE]\>*\

> Override the given Parameter for this operation. Omitting *VALUE* will result
> in overriding the Parameter as an empty string. May be specified 0 or more
> times.

**\--save-change-sets** *\<PATH\>*\

> Save generated Change Sets to **PATH/STACK.json**

**\--no-confirm**\

> Don't confirm before deployment.

**\--clean**\

> If successful, remove all Change Sets from the deployed Stack.

# STACKCTL

Part of the **stackctl(1)** suite
