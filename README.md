# Stackctl

Manage CloudFormation Stacks through specifications.

## About

`stackctl` is a command-line tool for working with [Stack Specifications][spec]. A Stack
Specification is a file-system format for describing deployed (or
to-be-deployed) CloudFormation Stacks including the Template, Parameters, and
Tags. `stackctl` can be used to pretty-print, diff, and deploy these
specifications.

[spec]: https://github.com/freckle/stackctl/blob/main/doc/stackctl.1.md#stack-specifications

This project also contains a Haskell library for doing the same.

## Install

### Binary install

Go to the [latest release][latest] and download the `.tar.gz` asset appropriate
for your OS. Navigate to the directory containing the downloaded file and run:

```console
tar xvf stackctl-*.tar.gz && cd stackctl
```

[latest]: https://github.com/freckle/stackctl/releases/latest

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

Once installed, see:

- `stackctl --help`,
- `stackctl <command> --help`,
- `man 1 stackctl`, or
- `man 1 stackctl <command>`

The man pages are also available [in-repository](./doc), but contain
documentation as of `main`, and not your installed version.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
