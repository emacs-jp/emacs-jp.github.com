#!/bin/bash
# Invoked by Docker Compose's /bin/bash entrypoint; not a host executable.
set -eu

# An empty token is treated as invalid credentials by this image's Octokit.
if [ -z "${JEKYLL_GITHUB_TOKEN:-}" ]; then
  unset JEKYLL_GITHUB_TOKEN
fi

# Use the image's gems and Pages configuration, as its entrypoint does.
cd "$BUNDLE_APP_CONFIG"
case "${1:-build}" in
  build)
    exec "$BUNDLE_APP_CONFIG/bin/github-pages" build --verbose \
      --source /workspace/docs --destination /tmp/site-build
    ;;
  serve)
    exec ruby -rgithub-pages "$BUNDLE_APP_CONFIG/bin/jekyll" serve \
      --source /workspace/docs --destination /tmp/site-preview --host 0.0.0.0
    ;;
  *)
    echo "Usage: pages.sh [build|serve]" >&2
    exit 2
    ;;
esac
