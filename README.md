# Stackctl

[![Hackage](https://img.shields.io/hackage/v/stackctl.svg?style=flat)](https://hackage.haskell.org/package/stackctl)
[![CI](https://github.com/freckle/stackctl/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/stackctl/actions/workflows/ci.yml)

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

### Pre-requisites

- Have `~/.local/bin` on your `$PATH`
- Have `~/.local/share/man` on your `$MANPATH` (for documentation)
- If on OSX, `brew install coreutils` (i.e. have `ginstall` available)
- If on OSX, `brew install jq`

### Scripted

```console
curl -L https://raw.githubusercontent.com/freckle/stackctl/main/install | bash
```

**NOTE**: some in the community have expressed [concerns][curlsh-bad] about the
security of so-called "curl-sh" installations. We think the argument has been
[pretty well debunked][curlsh-ok], but feel free to use the manual steps
instead.

[curlsh-bad]: https://0x46.net/thoughts/2019/04/27/piping-curl-to-shell/
[curlsh-ok]: https://www.arp242.net/curl-to-sh.html

### Manual

Go to the [latest release][latest] and download the `.tar.gz` asset appropriate
for your OS. Navigate to the directory containing the downloaded file and run:

[latest]: https://github.com/freckle/stackctl/releases/latest

```console
tar xvf stackctl-*.tar.gz
cd stackctl
```

User installation:

```console
make install PREFIX="$HOME/.local"
```

Global installation

```console
sudo make install
```

## Usage

Once installed, see:

- `stackctl --help`,
- `stackctl <command> --help`,
- `man 1 stackctl`, or
- `man 1 stackctl <command>`

The man pages are also available [online](https://freckle.github.io/stackctl/),
but contain documentation as of `main`, and not your installed version.

## Comparison to AWS CloudFormation Git Sync

[AWS CloudFormation Git Sync][aws-git-sync] was recently released by AWS. It
allows you to link a repository on GitHub to a CloudFormation Stack. The
repository contains a "deployment file" that defines a `template-file-path`,
`parameters`, and `tags` -- effectively, a Stack Specification.

When AWS notices updates to the deployment or template file land on a defined
branch, it updates the configured Stack accordingly, emitting events to SNS as
it does.

This is great for simple use-cases, and we fully expect they'll improve and
extend it such that it obviates Stackctl one day. In the meantime, there are
currently the following limitations when compared to Stackctl:

1. A repository can only target a single account and region
1. There is no changeset flow amenable to previewing changes via PRs. You update
   the file(s) on `main` and it syncs, that's it. If you're using a PR, you have
   only linting and human review as possible pre-deployment steps.
1. There is no way to specify description, capabilities, or dependencies
1. As of 12/23, there seemed to be some bugs, and the setup installs a managed
   event bridge that "phones home", sending events about your updates to some
   other AWS account ([source][first-look-blog])

[aws-git-sync]: https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/git-sync.html
[first-look-blog]: https://medium.com/@mattgillard/first-look-git-sync-for-cloudformation-stacks-9e2f39c311ac

## Relationship to CloudGenesis

[CloudGenesis][] is a project that also takes a directory of Stack
Specifications and deploys them when changed. Its on-disk format inspired ours
and, in fact, directories built for CloudGenesis can be managed by `stackctl`
(not necessarily the other way around).

[cloudgenesis]: https://github.com/LifeWay/CloudGenesis

The key differences are:

- CloudGenesis supplies AWS CodeBuild tooling for handling changes to your
  GitOps repository; Stackctl expects you to implement a GitHub Action that
  installs and executes `stackctl` commands as appropriate

  This makes Stackctl better if you need or want to also run the same tooling in
  a local context, but it makes CloudGenesis better if you need or want this
  activity to remain within the boundaries of your AWS VPC.

- CloudGenesis reacts to file-change events in S3, which only happens when you
  synchronize from `main`; Stackctl can run on any branch and easily be scoped
  to files changed in the PR or push.

  This enables Stackctl features like commenting with ChangeSet details on PRs,
  which are not possible in CloudGenesis as it's currently implemented.

- Stackctl adds the `Depends` key, for ordering multi-Stack processing

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
