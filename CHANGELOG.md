## [_Unreleased_](https://github.com/freckle/stackctl/compare/v1.0.1.0...main)

## [v1.0.0.2](https://github.com/freckle/stackctl/compare/v1.0.0.2...v1.0.1.0)

- Support reading CloudGenesis specifications

  - Accept account paths like `id.name` or `name.id`
  - Read `Parameters` as `Parameter{Key,Value}` or `{Name,Value}`

  This allows us to work with specifications directories originally implement
  for (and potentially still used with), the CloudGenesis tooling.

## [v1.0.0.2](https://github.com/freckle/stackctl/compare/v1.0.0.1...v1.0.0.2)

- Fix tailing all events to read most recent, causing Throttling errors

## [v1.0.0.1](https://github.com/freckle/stackctl/compare/v1.0.0.0...v1.0.0.1)

- Fix non-portable paths issue in OSX executable build

## [v1.0.0.0](https://github.com/freckle/stackctl/tree/v1.0.0.0)

First release
