% PLATFORM(8) User Manual
%
% January 2021

# USAGE

**platform** <subcommand> [option...] [argument...]

# DESCRIPTION

CLI for interacting with the Freckle Platform.

# CONCEPTS

The Platform CLI is manages and deploys **App** **Resource**s in
**Environment**s. These are the three pieces that uniquely define a
CloudFormation Stack to represent a deployment.

**App** and **Resource** are determined by the path to the configuration file
(see *platform(5)*), while **Environment** is set at deploy-time (see the
*SUBCOMMANDS*, *deploy*).

For toy applications you may have a single **Resource** in the **App** that is
deployed only to a *dev* **Environment**. On the other hand, you may have a
number of possibly inter-related **Resource**s within a single **App** and
deployed in distinct *dev*, *test*, or *prod* **Environment**s.

# OPTIONS

**\--color** *\<auto|always|never\>*\

> When to colorize output. **auto** (the default) will colorize output when
> connected to a terminal.

**\-v**, **\--verbose**\

> Log more verbosely

**\-e**, **\--environment** *\<ENVIRONMENT\>*\

> Prefix the deployed Stack name with with *{ENVIRONMENT}-*, and deploy with
> parameter **Environment**=*{ENVIRONMENT}*. Only required if deploying a
> template that expects the Parameter.

**\-r**, **\--resource** *\<NAME\>*\

> Limit deployment to a specific resource. Concretely, this means to only
> process **.platform/{NAME}.yaml**.

# SUBCOMMANDS

## assets:push

Compress and store files or directories to the Platform assets bucket.

**\--zip-quiet**\

> Use *zip \--quiet*.

**\--dryrun**\

> Emulate *aws s3 cp \--dryrun*.

**\-t**, **\--tag** *\<TAG\>*\

> Required. Compress and push assets to *{bucket}/{app}/{TAG}.zip*.

**FILE**...\

> Required. Files, directories, or patterns to include in the archive.
>
> Directories MUST end in a glob, and everything SHOULD be quoted to avoid
> pre-expansion by your shell.
>
> ```
> => Bad, may expand in current directory and not {resource}/
> platform --resource {resource} assets:push *.js
>
> => Bad, will not match the desired directory
> platform --resource {resource} assets:push '*.js' node_modules
>
> => Bad, for some reason includes only *.js files in node_modules
> platform --resource {resource} assets:push '*.js' node_modules/
>
> => Good
> platform --resource {resource} assets:push '*.js' 'node_modules/*'
>
> => Also good
> platform --resource {resource} assets:push \*.js 'node_modules/\*
> ```

## container:login

Log into the Platform registry. This subcommand takes no options.

## container:push

Push a built image to the Platform registry.

**\-t**, **\--tag** *\<TAG\>*\

> Required. Push an image named *{app}:{TAG}*.

**\--create**\

> Create the ECR repository if it doesn't already exist.

## spec:cat

Pretty-print discovered *Stack Specifications*.

**\--filter** *\<PATTERN[,PATTERN]\>*\

> Restrict specifications to those whose paths match any of the given
> **PATTERN**s.
>
> Note that Templates shown are always whatever is used by the Stacks being
> shown. In other words, this option cannot be used to filter Templates
> directly.

**\--no-stacks**\

> Don't print **stacks/**.

**\--no-templates**\

> Don't print **templates/**.

**\-b**, **\--brief**\

> Don't print file contents, only paths.

**DIRECTORY**\

> Where to find specifications. Default is **.platform/specs** (matches
> **spec:generate**'s default **\--output-directory**).

## spec:capture

Generate a _Stack Specification_ based on an already-deployed Stack.

**\-d**, **\--output-directory** *\<PATH\>*\

> Directory to write specs. Default is **.platform/specs/**.

**\-n**, **\--account-name** *\<NAME\>*\

> Write specs paths to **stacks/{account-id}.NAME/...**. If not given, we will
> use **${AWS_PROFILE:-unknown}**.

**\-t**, **\--template-path** *\<PATH\>*\

> Relative path for template. Default is **${STACK}.yaml**.

**\-p**, **\--path** *\<PATH\>*\

> Relative path for specification. Default is **${STACK}.yaml**.

**STACK**\

> Name of Stack to capture.

## spec:generate

Generate *Stack Specifications* for resources defined in any
**.platform/{resource}.yaml** files.

**\-t**, **\--tag** *\<TAG\>*\

> Required. Use **TAG** for deployment artifacts values.

**\-p**, **\--parameter** *\<KEY=VALUE\>*\

> Generate with Parameter **KEY** set to **VALUE**.

**\-d**, **\--output-directory** *\<PATH\>*\

> Directory to write specs. Default is **.platform/specs/**.

**\-n**, **\--account-name** *\<NAME\>*\

> Write specs paths to **stacks/{account-id}.NAME/...**. If not given, we will
> use **${AWS_PROFILE:-unknown}**.

## spec:changes

Create and present Change Sets for any *Stack Specifications*. This subcommand
will locate all specs for the currently active AWS Account and Region and
process them. To restrict this set, use **\--filter**.

**\--filter** *\<PATTERN[,PATTERN]\>*\

> Restrict specifications to those whose paths match any of the given
> **PATTERN**s.

**\-f**, **\--format** *\<FORMAT\>*\

> Output changes in **FORMAT**. **tty** (the default) will produce a simplified
> but colorized (unless redirected) listing. **pr** will produce markdown
> suitable to post as a comment to a GitHub Pull Request.

**DIRECTORY**\

> Where to find specifications. Default is **.platform/specs** (matches
> **spec:generate**'s default **\--output-directory**).

## spec:deploy

Deploy any *Stack Specifications*. This subcommand will locate all specs for the
currently active AWS Account and Region and process them. To restrict this set,
use **\--filter**.

**\--filter** *\<PATTERN[,PATTERN]\>*\

> Restrict specifications to those whose paths match any of the given
> **PATTERN**s.

**\--save-change-sets** *\<PATH\>*\

> Save generated Change Sets to **PATH/STACK.json**

**\--no-confirm**\

> Don't confirm before deployment.

**\--clean**\

> If successful, remove all Change Sets from the deployed Stack.

**DIRECTORY**\

> Where to find specifications. Default is **.platform/specs** (matches
> **spec:generate**'s default **\--output-directory**).

## deploy

Create or update resources defined in any **.platform/{resource}.yaml** files.

**\-t**, **\--tag** *\<TAG\>*\

> Required. Use **TAG** for deployment artifacts values.

**\-p**, **\--parameter** *\<KEY=VALUE\>*\

> Deploy with Parameter **KEY** set to **VALUE**.

**\-i**, **\--inspect**\

> Stop before deploying. The generated *Stack Specifications* can be found in
> **.platform/specs** for inspection (see **spec:cat**), review (see
> **spec:changes**), or deployment (see **spec:deploy**).

**\--no-confirm**\

> Don't confirm Parameters or ChangeSets before deployment.

## query

Query for information about deployed Apps.

Supported queries:

**outputs.{Key}**\

> Query in App Resource Stack's Outputs by Key.

**settings.{Setting}**\

> Query an App Resource's Settings by name.
>
> Available settings are *DockerBuildContext*, *Dockerfile*, *EcrRepository*,
> *S3AssetBucket*, *S3AssetPathPrefix*, *StackName*, and *StackTemplate*.

# AUTHOR

Freckle Engineering <freckle-engineering@renaissance.com>

# SEE ALSO

**platform(5)**, **zip(1)**.

For a description of *Stack Specifications*, see\

> https://github.com/freckle/platform/blob/main/STACK_SPEC.md
