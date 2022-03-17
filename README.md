# Platform

CLI for deploying App Resources on the Freckle Platform.

## Install

### Binary install

Go to the [latest release][latest] and download the `.tar.gz` asset appropriate
for your OS. Navigate to the directory containing the downloaded file and run:

```console
tar xvf platform-*.tar.gz && cd platform
```

[latest]: https://github.com/freckle/platform/releases/latest

#### Global installation

Install into `/usr/local/`, with appropriate permissions:

```console
sudo make install
```

#### User installation

Set `PREFIX` to base the installation in a directory of your choosing:

```console
make install PREFIX=$HOME/.local
```

## Usage

See `platform --help` and `platform <subcommand> --help`.

See `man 8 platform` for complete CLI documentation and `man 5 platform` for
documentation about `$resource.yaml` templates.

These are also available in-repository:

- [man 8 platform](./doc/platform.8.md)
- [man 5 platform](./doc/platform.5.md)

But will contain documentation as of `main`, and not your installed version.

## Integration Tests

In addition to the standard Haskell unit tests, this project contains a suite of
[integration tests](./integration/tests/) implemented with [cram][].

[cram]: https://bitheap.org/cram/

### Running

These can be run locally via:

```sh
# For example
pip install cram

# The tests expect the platform on $PATH to be the one to test
stack build ... --copy-bins

# Alternatively,
make install.check

# Some tests interact with our AWS Dev Account
AWS_PROFILE=freckle-dev aws sso login
AWS_PROFILE=freckle-dev cram integration/tests
```

There are also some tests in `integration/adhoc`. These may be slow or require
elevated privileges, so they're not normally run.

### Maintaining

These are a form of [_golden tests_][golden], so you may make changes that
require updating the expectations for the new behavior. This can be done easily
by running `cram` with the `-i` option.

[golden]: https://ro-che.info/articles/2017-12-04-golden-tests

### Example Apps

Under [`example-apps/`](./example-apps/) are standalone Apps that make use of
our templates and features. Besides acting as documentation, they are also used
by our Integration Tests.

Any cram test starting with `example-app-` is a test where we generate a
change-set for one of these Apps and assert no changes would occur. If you do
expect changes as a result of your work, you will need to `platform deploy` the
affected examples manually, to get the tests passing again.

## Releases

To create a release:

- Update `CHANGELOG.md` as necessary
- Update `version` in `package.yaml` (and `platform.cabal`)
- Merge

Feel free to do this on `main` if you're confident of the process, include a
version bump in the PR of the feature/fix if you'd like, or open a separate PR
for review of the bump and `CHANGELOG` content.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
