#!/usr/bin/env bash
#
# Clone benchmark repositories at specific commits.
# Repos are cloned into repos/<name> using shallow clones.

set -euo pipefail

# Repository definitions: url commit
repos=(
  "https://github.com/jellyfin/jellyfin a0931baa8eb879898f4bc4049176ed3bdb4d80d1"
  "https://github.com/grafana/grafana afcb55156260fe1887c4731e6cc4c155cc8281a2"
  "https://gitlab.com/gitlab-org/gitlab 915627de697e2dd71fe8205853de51ad3794f3ac"
  "https://github.com/Netflix/lemur 28b9a73a83d350b1c7ab71fdd739d64eec5d06aa"
  "https://github.com/pythongosssss/ComfyUI-Custom-Scripts 943e5cc7526c601600150867a80a02ab008415e7"
  "https://github.com/pmd/pmd 81739da5caff948dbcd2136c17532b65c726c781"
  "https://github.com/square/leakcanary bf5086da26952e3627f18865bb232963e4d019c5"
)

clone_specific_commit() {
  local url="$1"
  local commit="$2"
  local name="$3"
  local target="repos/$name"

  if [[ -d "$target/.git" ]]; then
    local current_sha
    current_sha=$(git -C "$target" rev-parse HEAD 2>/dev/null || echo "")
    if [[ "$current_sha" == "$commit" ]]; then
      echo "$name: already at $commit"
      return
    fi
    echo "$name: at $current_sha, expected $commit"
    echo "Fetching and checking out correct commit..."
    git -C "$target" fetch --depth 1 origin "$commit"
    git -C "$target" checkout "$commit"
    echo "Done: $name at commit $commit"
    echo
    return
  fi

  echo "Cloning $url into $target (shallow)..."
  git clone --no-checkout --depth 1 "$url" "$target"

  echo "Fetching commit $commit..."
  git -C "$target" fetch --depth 1 origin "$commit"

  echo "Checking out commit $commit..."
  git -C "$target" checkout "$commit"

  echo "Done: $name at commit $commit"
  echo
}

mkdir -p repos

for entry in "${repos[@]}"; do
  url="${entry% *}"
  commit="${entry##* }"
  name=$(basename "$url" .git)

  clone_specific_commit "$url" "$commit" "$name"
done

echo "All repositories cloned."
