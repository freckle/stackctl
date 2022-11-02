## [_Unreleased_](https://github.com/freckle/stackctl/compare/v1.1.0.0...main)

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
