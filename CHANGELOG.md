## [_Unreleased_](https://github.com/freckle/stackctl/compare/v1.7.0.0...main)

## [v1.7.0.0](https://github.com/freckle/stackctl/compare/v1.6.1.2...v1.7.0.0)

- Retain numeric parameter types, and add boolean parameter handling, in our own
  yaml generation

  This required changing `Generate` to use `ParametersYaml` instead of
  `[Parameter]`, hence the major version bump.

## [v1.6.1.2](https://github.com/freckle/stackctl/compare/v1.6.1.1...v1.6.1.2)

- Require Blammo-1.2.2.3

## [v1.6.1.1](https://github.com/freckle/stackctl/compare/v1.6.1.0...v1.6.1.1)

- Fix: finding removed stacks now respects `STACKCTL_DIRECTORY`

## [v1.6.1.0](https://github.com/freckle/stackctl/compare/v1.6.0.0...v1.6.1.0)

- Add `Ord` instance on `RequiredVersion` and `RequiredVersionOp`

## [v1.6.0.0](https://github.com/freckle/stackctl/compare/v1.5.0.1...v1.6.0.0)

- Re-implement `Stackctl.AWS` with `amazonka-mtl`.

_No CLI or behavior changes._

## [v1.5.0.1](https://github.com/freckle/stackctl/compare/v1.5.0.0...v1.5.0.1)

- Handle missing-or-empty specs directory more explicitly
- Add warning for `Depends` pointing to non-existent spec
- Fix formatting of required version in warning message

## [v1.5.0.0](https://github.com/freckle/stackctl/compare/v1.4.4.0...v1.5.0.0)

Breaking changes:

- Don't require a name argument to the `awsSimple` function

New features:

- Add `Exec` and `Shell` features in `actions[].run`
- Support lists in `actions[].run` (single items still work)
- Add more granular status indicators in `stack-ls(1)` output, print a legend of
  these indicators as a footer (disable with `--no-legend`)

Fixes:

- Fix for redundant change-set creation errors in logging output
- Fix globbing bug in auto-expansion of `--filter` arguments

## [v1.4.4.0](https://github.com/freckle/stackctl/compare/v1.4.3.0...v1.4.4.0)

- Add `awsSilently`

## [v1.4.3.0](https://github.com/freckle/stackctl/compare/v1.4.2.2...v1.4.3.0)

- Add `awsWithAuth`
- Add `forEachSpec_`

## [v1.4.2.2](https://github.com/freckle/stackctl/compare/v1.4.2.1...v1.4.2.2)

- Use `amazonka-2.0` :tada:
- Finalize update to `UnliftIO.Exception.Lens`
- Re-export upstreamed `Blammo.Logging.Colors`

## [v1.4.2.1](https://github.com/freckle/stackctl/compare/v1.4.2.0...v1.4.2.1)

No changes. Bumped to trigger release workflow.

## [v1.4.2.0](https://github.com/freckle/stackctl/compare/v1.4.0.1...v1.4.2.0)

- Add `stackctl-ls` for listing stacks and their deployed status
- Add `--auto-sso` option for automating `aws sso login` when required

## [v1.4.0.1](https://github.com/freckle/stackctl/compare/v1.4.0.0...v1.4.0.1)

- Document and read a consistently-named `STACKCTL_FILTER` for `--filter`. For
  now, the old and incorrect `STACKCTL_FILTERS` will still work.

## [v1.4.0.0](https://github.com/freckle/stackctl/compare/v1.3.0.2...v1.4.0.0)

- Add `awsAssumeRole` for running an action as an assumed role
- Refactor `Generate` interface to better support generating stacks with
  pre-existing templates

## [v1.3.0.2](https://github.com/freckle/stackctl/compare/v1.3.0.1...v1.3.0.2)

- Adjust timeout when invoking Lambdas to allow up to Lambda's own execution
  timeout (15 minutes).

## [v1.3.0.1](https://github.com/freckle/stackctl/compare/v1.3.0.0...v1.3.0.1)

- Fix bug where `LOG_COLOR` was never respected
- Also accept `"required_version: == <version>"`
- Add `Eq`, `ToJSON` instance on `RequiredVersion`

## [v1.3.0.0](https://github.com/freckle/stackctl/compare/v1.2.0.1...v1.3.0.0)

- Fix it so commands like `version` don't need a valid AWS environment

  This changes the `Subcommand` interface and so is a major version update for
  the purposes of those using Stackctl as a library.

## [v1.2.0.0](https://github.com/freckle/stackctl/compare/v1.1.3.1...v1.2.0.0)

- Use more specific types in `Has{Directory,Filter,Color}Option`
- Add environment variable configuration for `STACKCTL_{DIRECTORY,FILTERS}`

## [v1.1.4.0](https://github.com/freckle/stackctl/compare/v1.1.3.1...v1.1.4.0)

- Support matching Stacks by glob in `capture`
- Add `--tag` to `changes` and `deploy`

## [v1.1.3.1](https://github.com/freckle/stackctl/compare/v1.1.3.0...v1.1.3.1)

- Fix JSON formatting bugs in generating specification

## [v1.1.3.0](https://github.com/freckle/stackctl/compare/v1.1.2.2...v1.1.3.0)

- Repository-local configuration

  See https://github.com/freckle/stackctl/commit/564678203fe70b5c4c46c655dd3daeaafb6de9e0

- Don't duplicate re-used templates in `stackctl-cat`
- Improve `--filter`

  - Match against stack name and template, in addition to spec path.
  - Automatically prepend `**/` (unless there is already a leading wildcard) and
    append `{/*,.yaml,.json}` (unless there is already a trailing wildcard or
    extension).

  In general, this aims to make `--filter` match more things more intuitively
  for operators, but still match exactly in programmatic use-cases.

- Various documentation improvements
- Support more natural `{Key}: {Value}` syntax in `Parameters` and `Tags`
- Fix bug where we may generate an `{}` element in `Parameters`

## [v1.1.2.2](https://github.com/freckle/stackctl/compare/v1.1.2.1...v1.1.2.2)

- Add support for Stack descriptions

## [v1.1.2.1](https://github.com/freckle/stackctl/compare/v1.1.2.0...v1.1.2.1)

- Build with LTS-20.4 / GHC 9.2

## [v1.1.2.0](https://github.com/freckle/stackctl/compare/v1.1.1.1...v1.1.2.0)

- Fix incorrect ordering of log-messages by setting `LOG_CONCURRENCY=1`
- Fix potential coloring of changes being redirected to a file
- Make `PATH` optional (again) in `stackctl changes`
- Add `--no-flip` to `stackctl capture`

## [v1.1.1.1](https://github.com/freckle/stackctl/compare/v1.1.1.0...v1.1.1.1)

- Trigger release

## [v1.1.1.0](https://github.com/freckle/stackctl/compare/v1.1.0.5...v1.1.1.0)

- Add `--parameter` to `changes` and `deploy`
- Sort changes by causing-before-caused

## [v1.1.0.5](https://github.com/freckle/stackctl/compare/v1.1.0.4...v1.1.0.5)

- Trigger release workflow

## [v1.1.0.4](https://github.com/freckle/stackctl/compare/v1.1.0.3...v1.1.0.4)

- Fix bug where only the last spec in a multi-spec case had its changes present
  in the output file generated by `changes`.

## [v1.1.0.3](https://github.com/freckle/stackctl/compare/v1.1.0.2...v1.1.0.3)

- Require Blammo-1.1.1.0

## [v1.1.0.2](https://github.com/freckle/stackctl/compare/v1.1.0.1...v1.1.0.2)

- Log responses from `awsLambdaInvoke` when running actions
- Clarify discovery logging
- Add install script

## [v1.1.0.1](https://github.com/freckle/stackctl/compare/v1.1.0.0...v1.1.0.1)

- Update to `cfn-flip-0.1.0.3`

## [v1.1.0.0](https://github.com/freckle/stackctl/compare/v1.0.2.0...v1.1.0.0)

- Fix interleaved or out-of-order output bugs by streaming deployment events
  through the Logger instead of directly to `stdout`
- Logging goes to `stdout` by default (`LOG_DESTINATION` can still be used)
- The `changes` subcommand now requires a `PATH` argument

## [v1.0.2.0](https://github.com/freckle/stackctl/compare/v1.0.1.2...v1.0.2.0)

- Add `Stackctl.Action`

  Support for taking actions during Stack management, currently we support
  invoking a lambda post-deployment. In the future, we can add more, such as
  running local pre-deploy validation or preparation scripts.

- Add `awsCloudFormationDescribeStackOutputs`

## [v1.0.1.2](https://github.com/freckle/stackctl/compare/v1.0.1.1...v1.0.1.2)

- Always flush log messages before our own output

## [v1.0.1.1](https://github.com/freckle/stackctl/compare/v1.0.1.0...v1.0.1.1)

- Respect `LOG_DESTINATION` (the default remains `stderr`)

## [v1.0.1.0](https://github.com/freckle/stackctl/compare/v1.0.0.2...v1.0.1.0)

- Support reading CloudGenesis specifications

  - Accept account paths like `id.name` or `name.id`
  - Read `Parameters` as `Parameter{Key,Value}` or `{Name,Value}`

  This allows us to work with specifications directories originally implemented
  for, and potentially still used with, the CloudGenesis tooling.

## [v1.0.0.2](https://github.com/freckle/stackctl/compare/v1.0.0.1...v1.0.0.2)

- Fix tailing all events to read most recent, causing Throttling errors

## [v1.0.0.1](https://github.com/freckle/stackctl/compare/v1.0.0.0...v1.0.0.1)

- Fix non-portable paths issue in OSX executable build

## [v1.0.0.0](https://github.com/freckle/stackctl/tree/v1.0.0.0)

First release
