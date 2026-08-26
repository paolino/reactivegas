# shellcheck shell=bash
#
# Shared semantics for the manifest/Cabal version seam (D001, I001).
#
# `.release-please-manifest.json` is the sole release-version authority. Its
# SemVer `x.y.z` has exactly one legal Cabal PVP representation, `x.y.z.0`.
# Both `check-release-version` and `sync-cabal-version` source this file so the
# two commands cannot drift apart from each other.

# A strict SemVer/PVP numeric identifier: no leading zeros (`01` is not `1`).
SEMVER_NUM='(0|[1-9][0-9]*)'

release_fail() {
    printf '%s: %s\n' "${RELEASE_TOOL:-release}" "$*" >&2
    return 1
}

# Read and validate the authoritative manifest version.
release_manifest_version() {
    local manifest=$1 version
    [ -f "$manifest" ] || release_fail "manifest not found: $manifest" || return 1
    version=$(jq -er '."."' "$manifest" 2>/dev/null) \
        || release_fail "manifest has no string \".\" key: $manifest" || return 1
    [[ $version =~ ^${SEMVER_NUM}\.${SEMVER_NUM}\.${SEMVER_NUM}$ ]] \
        || release_fail "manifest version is not strict SemVer x.y.z: $version" || return 1
    printf '%s\n' "$version"
}

# The one Cabal PVP version a manifest SemVer may map to.
release_expected_cabal_version() {
    printf '%s.0\n' "$1"
}

# Read the Cabal `version:` field without judging its shape.
#
# Fails closed unless the field occurs exactly once: every command here rewrites
# or compares that single field, and a second occurrence would make "the package
# version" ambiguous (INV-49-CABAL-VERSION-FIELD-UNIQUE).
release_cabal_version_raw() {
    local cabal=$1 count version
    [ -f "$cabal" ] || release_fail "cabal file not found: $cabal" || return 1
    count=$(grep -c '^version:' "$cabal" || true)
    [ "$count" = 1 ] \
        || release_fail "expected exactly one '^version:' field in $cabal, found $count" || return 1
    version=$(sed -n -E 's/^version:[[:space:]]*([^[:space:]]+)[[:space:]]*$/\1/p' "$cabal")
    [ -n "$version" ] \
        || release_fail "could not parse the version field of $cabal" || return 1
    printf '%s\n' "$version"
}

# Read the Cabal `version:` field and require a four-component PVP shape.
release_cabal_version() {
    local cabal=$1 version
    version=$(release_cabal_version_raw "$cabal") || return 1
    [[ $version =~ ^${SEMVER_NUM}\.${SEMVER_NUM}\.${SEMVER_NUM}\.${SEMVER_NUM}$ ]] \
        || release_fail "cabal version is not PVP w.x.y.z: $version" || return 1
    printf '%s\n' "$version"
}
