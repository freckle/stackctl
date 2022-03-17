Discovering Stack name collisions

  $ mkdir -p \
  >   stacks/539282909833.x/us-east-1/dev \
  >   stacks/539282909833.x/us-east-1/foo/bar \
  >   stacks/539282909833.x/us-east-1/foo-bar
  > touch \
  >   stacks/539282909833.x/us-east-1/dev-app.yaml \
  >   stacks/539282909833.x/us-east-1/dev/app.yaml \
  >   stacks/539282909833.x/us-east-1/foo/bar/app.yaml \
  >   stacks/539282909833.x/us-east-1/foo-bar/app.yaml \
  >   stacks/539282909833.x/us-east-1/foo-bar-app.yaml \
  >   stacks/539282909833.x/us-east-1/foo/bar-app.yaml
  > stackctl cat
  Multiple specifications produced Stack name dev-app:
    - stacks/539282909833.x/us-east-1/dev-app.yaml
    - stacks/539282909833.x/us-east-1/dev/app.yaml
  Multiple specifications produced Stack name foo-bar-app:
    - stacks/539282909833.x/us-east-1/foo-bar-app.yaml
    - stacks/539282909833.x/us-east-1/foo/bar-app.yaml
    - stacks/539282909833.x/us-east-1/foo/bar/app.yaml
    - stacks/539282909833.x/us-east-1/foo-bar/app.yaml
  [1]
