# Releases

Reactivegas publishes **provisional milestone artifacts**. Every GitHub release
this pipeline creates is marked a *prerelease*, and the single asset it carries
is the current Nix `.#default` server build. It exists so the milestone can be
exercised end to end by someone with no source checkout. It is not a general
availability release and carries no stability or support guarantee.

## The version authority

`.release-please-manifest.json` is the **sole** source of the release version.
Everything else is derived from it:

| Artifact | Value | Derived how |
| --- | --- | --- |
| `.release-please-manifest.json` | `2021.11.5` (SemVer) | authoritative |
| `reactivegas.cabal` | `2021.11.5.0` (Cabal PVP) | `scripts/release/sync-cabal-version` |
| Git tag / GitHub release | `v<manifest version>` | Release Please |

A manifest SemVer `x.y.z` has exactly one legal Cabal representation, `x.y.z.0`.
Both are strict: `01.2.3` is not a version. CI runs
`scripts/release/check-release-version` on every push and pull request and
**fails** when the two disagree, printing both observed versions on success so a
green run still shows what it compared:

```console
$ scripts/release/check-release-version .release-please-manifest.json reactivegas.cabal
manifest version:       2021.11.5 (.release-please-manifest.json)
cabal version:          2021.11.5.0 (reactivegas.cabal)
expected cabal version: 2021.11.5.0
release version seam: manifest and Cabal agree
```

Never edit the Cabal `version:` field by hand.

## Cutting a release

Publication takes **two separate merges**, and merging the first publishes
nothing.

1. **Land conventional commits on `master`.** `feat:` and `fix:` commits cause
   Release Please to open or update the release pull request
   `release-please--branches--master`, carrying the manifest bump and changelog.

2. **Recover that pull request's checks — synchronization first.** Release
   Please opens it as the `github-actions` bot using `GITHUB_TOKEN`, and GitHub
   does **not** run workflows on bot-created pull requests. The branch therefore
   arrives with a bumped manifest, a *stale* Cabal version, and zero checks.
   This repository has no organization App and no long-lived personal token, so
   the no-secret recovery is two manual dispatches, **in this order**:

   ```console
   # 1. propagate the bumped manifest version into reactivegas.cabal
   gh workflow run "Sync Cabal version" --ref release-please--branches--master
   # 2. only then run the checks, which include the drift guard
   gh workflow run CI --ref release-please--branches--master
   gh run list --branch release-please--branches--master
   ```

   The order is not cosmetic. Dispatching CI first produces a red
   `Check the manifest/Cabal version seam` step that nothing on the branch can
   clear, because the synchronization workflow never fires on a bot-created
   pull request by itself. Diagnose a stuck pull request with
   `gh pr view <number> --json mergeable,mergeStateStatus,statusCheckRollup`.

3. **Merge the release pull request.** This is the authorization point for
   publication and needs milestone-owner approval. Merging it makes the next
   `Release` run on `master` create the `v<version>` tag and GitHub release.

4. **Wait for the publish job.** `publish-linux-server` runs only when Release
   Please reports `release_created == 'true'`. It checks out the emitted
   `tag_name` — not `master` — marks the release a prerelease, packages the
   bundle, and uploads it.

## The published asset

Exactly one asset per release, `reactivegas-server-<tag>-linux-x86_64.tar.gz`,
containing `bin/server` (the entrypoint) and `PROVISIONAL.md`, which declares
the tag the archive was built for. `scripts/release/package-release-artifact`
builds it, and the same command runs locally, on pull requests, and in the
release workflow:

```console
$ scripts/release/package-release-artifact v2021.11.5 /tmp/release
```

It smoke-tests `bin/server --help` on the staged tree, then extracts the archive
it just wrote, checks the archived entrypoint is **byte-identical** to the
binary it smoked, confirms `PROVISIONAL.md` declares the requested tag, and
smokes the extracted entrypoint again. The bytes that reach GitHub are the bytes
that were tested, and they say which release they belong to.

## Fetching the release as a stranger

The acceptance boundary is a download from GitHub into a clean directory outside
any checkout. A workflow artifact or a local Nix store path does not count.

```console
$ TAG=v2021.11.5
$ ASSET="reactivegas-server-$TAG-linux-x86_64.tar.gz"
$ mkdir /tmp/reactivegas-stranger && cd /tmp/reactivegas-stranger
$ gh release download "$TAG" --repo paolino/reactivegas --pattern "$ASSET"
$ test -f "$ASSET"
$ sha256sum "$ASSET"
$ mkdir extracted && tar -xzf "$ASSET" -C extracted
$ grep -F "release tag: $TAG" extracted/PROVISIONAL.md
$ ./extracted/bin/server --help
```

**Never select the asset with a wildcard.** A pattern like
`reactivegas-server-*-linux-x86_64.tar.gz` matches another release's asset just
as well, and a genuine archive for the wrong tag downloads, extracts and smokes
perfectly — so the check passes while proving nothing about the tag you asked
for. Bind the requested tag to the exact filename with `--pattern`, and
reconcile it against the tag the archive itself declares in `PROVISIONAL.md`.

The ticket gate automates exactly that sequence, including the identity
reconciliation: `./gate.sh stranger-fetch v2021.11.5`, or
`./gate.sh validate-stranger-dir v2021.11.5 <directory>` against an
already-downloaded directory.

## Safety properties

The `Release` workflow has no `pull_request` trigger, so a pull request can
never publish; the publish job is gated on `release_created`, so a `master` push
that creates no release uploads nothing; and the asset is built from the emitted
tag, so its provenance is immutable.
`scripts/release/check-release-wiring` asserts each of these structurally on
every CI run, and `--self-test` seeds a deliberate defect against each assertion
to prove it can still fail.
