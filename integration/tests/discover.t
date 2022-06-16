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
  > LOG_FORMAT=json stackctl cat |& jq --sort-keys '.message'
  {
    "meta": {
      "name": "dev-app",
      "paths": [
        "dev-app.yaml",
        "dev/app.yaml"
      ]
    },
    "text": "Multiple specifications produced the same Stack name"
  }
  {
    "meta": {
      "name": "foo-bar-app",
      "paths": [
        "foo-bar-app.yaml",
        "foo/bar-app.yaml",
        "foo/bar/app.yaml",
        "foo-bar/app.yaml"
      ]
    },
    "text": "Multiple specifications produced the same Stack name"
  }
