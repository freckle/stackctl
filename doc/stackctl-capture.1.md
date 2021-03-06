% STACKCTL-CAPTURE(1) User Manual
%
% March 2022

# NAME

stackctl capture - Generate stack specifications from deployed stacks

# SYNOPSIS

*stackctl capture* \[options]

# DESCRIPTION

Fetches the CloudFormation Template and currently supplied Parameters of a
deployed Stack and stores it as a stack specification under the
currently-authorized AWS Account and Region.

If files already exist at the inferred locations, they will be overwritten.

# OPTIONS

**\-n**, **\--account-name** *\<NAME\>*\

> Write specs paths to **stacks/{account-id}.NAME/...**. If not given, we will
> use **${AWS_PROFILE:-unknown}**.

**\-t**, **\--template-path** *\<PATH\>*\

> Relative path for template. Default is **${STACK}.yaml**.

**\-p**, **\--path** *\<PATH\>*\

> Relative path for specification. Default is **${STACK}.yaml**.

**STACK**\

> Name of Stack to capture.

# ENVIRONMENT

*AWS_PROFILE*\

> If set, will be used when defaulting **-n**.

# STACKCTL

Part of the **stackctl(1)** suite
