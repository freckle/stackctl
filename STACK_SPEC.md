# Stack Specification

A *Stack Specification* is a file format and file-system structure used
internally by PlatformCLI to fully describe a deployed (or deployable)
CloudFormation Stack.

## Format

Specification files ("specs") have the following path structure:

```
stacks/{account-id}.{account-name}/{region}/{stack-name}.yaml
```

(This is called a `StackSpecPath` in the code.)

And contents:

```yaml
Template: <path>

Depends:
  - <name>

Parameters:
  - ParameterKey: <string>
    ParameterValue: <string>

Capabilities:
  - <capability>

Tags:
  - Key: <string>
    Value: <string>
```

(This is called a `StackSpecYaml` in the code.)

- `{account-id}`

  The AWS Account Id in which to deploy this Stack.

- `{account-name}`

  A friendly name for this Account. This is never used logically and can be
  whatever you find useful for identifying this Account.

- `{region}`

  The AWS Region in which to deploy this Stack

- `{stack-name}`

  The name to use for this Stack.

- `{.Template}` (required)

  The template to use when deploying this Stack. Must be a relative path under
  `templates/`.

- `{.Depends}` (optional)

  Other Stacks (by name) that should be ordered before this one if deployed
  together.

- `{.Parameters}` (optional)

  Parameters to use when deploying the Stack.

- `{.Capabilities}` (optional)

  Capabilities to use when deploying the Stack.

- `{.Tags}` (optional)

  Tags to use when deploying the Stack.

## Example

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

## Generation

For Platform Apps (resources defined in `.platform/*.yaml`), the first phase of
a deployment is to take those platform-yaml details, along with inferred and
passed values (e.g. other Stack Outputs, `--tag`, etc), and produce full
specifications for the required stacks.

The `spec:generate` subcommand handles this.

## Deployment

Once we have a specification, deployment is _conceptually_ simple:

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
them, wait, and finally clean up.

The `spec:deploy` subcommand handles this.

## GitOps

Stack Specifications can be committed into a git repository. This makes every
change to deployed Stacks tracked in version history for audit or revert.

This is [GitOps][].

[gitops]: https://about.gitlab.com/topics/gitops/

This repository can also be stored in GitHub. As changes are made, `platform`
subcommands for review (i.e. `spec:changes`) and execution can be wrapped into a
Pull Request flow.

Platform Apps and non-Apps deployments can both target Stack Specifications, and
are thus both utilizing the same process and tooling.

```
Platform App's GitHub Workflow:
  platform spec:generate --> <tmp>        --> platform spec:deploy
                               stacks/
                               templates/

Platform Team's GitOps Repo:
  {operator hand-edits}  --> <repo>       --> platform spec:changes (PR)
                               stacks/    --> platform spec:deploy (merged)
                               templates/
```

## Prior Art

This format and semantics is a minor extension of that used by the
[CloudGenesis][cg] project ([license][cg-license]).

[cg]: https://github.com/LifeWay/CloudGenesis
[cg-license]: https://github.com/LifeWay/CloudGenesis/blob/8a2fdffa0e83c45bf118b1a6102b5b31a8fdfcd7/LICENSE
