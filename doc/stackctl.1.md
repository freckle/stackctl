% STACKCTL(1) User Manual
%
% January 2023

# NAME

stackctl - manage CloudFormation Stacks through specifications

# SYNOPSIS

*stackctl* \[options] \<command> \<args>

# OPTIONS

**\-d**, **\--directory** *\<PATH>*\

> Where to find specifications. Default is **.**.

**\--filter** *\<PATTERN[,PATTERN]>*\

> Restrict specifications to those whose paths match any of the given
> **PATTERN**s.

**\--color** *\<auto|always|never>*\

> When to colorize output. **auto** (the default) will colorize output when
> connected to a terminal.

**\-v**, **\--verbose**\

> Log more verbosely

# COMMANDS

**cat**\

> Pretty-print specifications.

**capture**\

> Generate specifications for already-deployed Stacks.

**changes**\

> Show changes between on-disk specifications and their deployed state.

**deploy**\

> Make deployed state match on-disk specifications.

**version**\

> Print the CLI's version.

Run **man stackctl \<command\>** for more details.

# Stack Specifications

A *Stack Specification* is a file format and file-system structure used to fully
describe a deployed (or deployable) CloudFormation Stack. *stackctl* is your way
of creating, displaying, and using such files.

## Format

Specification files ("specs") have the following path structure:

> stacks/*{account-id}*.*{account-name}*/*{region}*/*{stack-name}*.yaml

Its constituent parts are used as follows:

*{account-id}*\

> The AWS Account Id in which to deploy this Stack.

*{account-name}*\

> A friendly name for this Account. This is never used logically and can be
  whatever you find useful for identifying this Account.

*{region}*\

> The AWS Region in which to deploy this Stack

*{stack-name}*\

> The name to use for this Stack.
>
>  *{stack/name}*.yaml is also supported so that directories can be used for
>  your own organization. Such paths will have directory-separators replaced by
>  hyphens when used.

These files' contents should be:

```
Description: <string>

Template: <path>

Depends:
  - <name>

Actions:
  - on: <event>
    run:
      <action>: <argument...>

Parameters: Object<string, string|number>

Capabilities:
  - <capability>

Tags: Object<string, string>
```

And these constituent parts are used as follows:

*{.Description}*\

> Optional. Set the Stack's description.
>
> This value will be inserted as the *Description* key in the template body on
> deployment, which becomes the deployed Stack's description. If the template
> already contains a description, the specification value will be ignored.

*{.Template}*\

> Required. The template to use when deploying this Stack. Must be a relative
> path under `templates/`.

*{.Depends}*\

> Optional. Other Stacks (by name) that should be ordered before this one if
> deployed together.

*{.Actions}*\

> Optional. Actions to run when certain Stack management events occur.

*{.Actions[].on}*\

> The event on which to perform the action:
>
> - **PostDeploy**: run the action after a successful deployment

*{.Actions[].run}*\

> The action to perform on the given event:
>
> - **InvokeLambdaByStackOutput**: *\<output name>*: invoke the function whose
>   name is found in the given Output of the deployed Stack
> - **InvokeLambdaByName**: *\<function name>*: invoke the given function

*{.Parameters}*\

> Optional. Parameters to use when deploying the Stack.
>
> The *Parameters* key can be specified in any of 3 forms:
>
> ```
> # Natural (recommended)
> Parameters:
>   Foo: Bar
>   Baz: Bat
>
> # CloudFormation
> Parameters:
>   - ParameterKey: Foo
>     ParameterValue: Bar
>   - ParameterKey: Baz
>     ParameterValue: Bat
>
> # CloudGenesis
> Parameters:
>   - Key: Foo
>     Value: Bar
>   - Key: Baz
>     Value: Bat
> ```

*{.Capabilities}*\

> Optional. Capabilities to use when deploying the Stack.
>
> Valid *Capabilities* are,
>
> **CAPABILITY_AUTO_EXPAND**,\
> **CAPABILITY_IAM**, and\
> **CAPABILITY_NAMED_IAM**

*{.Tags}*\

> Optional. Tags to use when deploying the Stack.
>
> The *Tags* key can be specified in either of 2 forms:
>
> ```
> # Natural (recommended)
> Tags:
>   Foo: Bar
>   Baz: Bat
>
> # CloudFormation / CloudGenesis
> Parameters:
>   - Key: Foo
>     Value: Bar
>   - Key: Baz
>     Value: Bat
> ```

## Example

The following example shares a single Template between two deployments in two
regions of a single account.

```
stacks/
  111111111111.prod/
    us-east-1/
      my-app.yaml
        | Template: web.yaml
        | Parameters:
        |   ...

    us-west-2/
      my-app.yaml
        | Template: web.yaml
        | Parameters:
        |   ...

templates/
  web.yaml
    | Parameters:
    |   ...
    | Resources:
    |   ...
```

## Deployment

Once we have a specification, deployment is *conceptually* simple:

```sh
aws configure # for {account-id}

aws --region {region} cloudformation deploy \
  --stack-name {stack-name} \
  --template-file templates/{.Template} \
  --parameter-overrides {.Parameters} \
  --capabilities {.Capabilities} \
  --tags {.Tags}
```

In reality, we create changesets, optionally present them for review, execute
them, wait, stream events, and finally clean up.

See **stackctl-changes(1)** and **stackctl-deploy(1)**.

# ENVIRONMENT

*STACKCTL_DIRECTORY*\

> Environment-based alternative for *\--directory*.

*STACKCTL_FILTER*\

> Environment-based alternative for *\--filter*.

*LOG_\**\

> Variables such as *LOG_COLOR* or *LOG_LEVEL* will be respected by the
> underlying logging framework (Blammo). Please see its documentation for
> complete details:
>
> https://github.com/freckle/blammo#configuration

*AWS_PROFILE*\

> If set, will be used as account name in commands that create new
> specifications.

# AUTHOR

Freckle Engineering <freckle-engineering@renaissance.com>

# SEE ALSO

**stackctl-cat(1)**, **stackctl-capture(1)**, **stackctl-changes(1)**,
**stackctl-deploy(1)**, **stackctl-version(1)**.

# ACKNOWLEDGEMENTS

The specification format and semantics is a minor extension of that used by the
CloudGenesis project, capturing more of a CloudFormation Stack's deployed state
statically is terraform-inspired, and GitOps as an approach was pioneered for
Kubernetes by Flux CD.
