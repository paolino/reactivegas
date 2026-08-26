# frozen_string_literal: true
#
# Structural assertions over the release line's declarative surface.
#
# The frozen ticket gate proves the release tooling's *behaviour*. These
# assertions cover the seams that have no runtime: that the release workflow
# builds from the emitted tag rather than a mutable branch (I003), that it
# cannot be reached from a pull request at all (I007), that the GitHub release
# is marked provisional (I004), and that CI still enforces the version seam and
# the dev-shell build (R003, R010). They parse YAML and JSON rather than
# grepping, so a reshuffled but correct workflow keeps passing and a
# plausible-looking but wrong one does not.
#
# usage: ruby check-wiring.rb [REPO_ROOT]

require 'yaml'
require 'json'

ROOT = ARGV[0] || Dir.pwd
FAILURES = []

def failed(message)
  FAILURES << message
end

def want(condition, message)
  failed(message) unless condition
end

def read_json(path)
  JSON.parse(File.read(path))
rescue StandardError => e
  failed("#{path}: unreadable JSON (#{e.class}: #{e.message})")
  nil
end

def read_yaml(path)
  YAML.load_file(path)
rescue StandardError => e
  failed("#{path}: unreadable YAML (#{e.class}: #{e.message})")
  nil
end

# Psych resolves a bare `on:` mapping key to the boolean true (YAML 1.1), so a
# workflow's trigger block is not reachable under the string "on".
def triggers(doc)
  return {} unless doc.is_a?(Hash)

  block = doc.key?(true) ? doc[true] : doc['on']
  block.is_a?(Hash) ? block : {}
end

def jobs_of(doc)
  doc.is_a?(Hash) && doc['jobs'].is_a?(Hash) ? doc['jobs'] : {}
end

def steps_of(job)
  job.is_a?(Hash) && job['steps'].is_a?(Array) ? job['steps'] : []
end

def all_runs(doc)
  jobs_of(doc).values.flat_map { |job| steps_of(job).map { |step| step['run'].to_s } }
end

config_path   = File.join(ROOT, 'release-please-config.json')
manifest_path = File.join(ROOT, '.release-please-manifest.json')
release_path  = File.join(ROOT, '.github/workflows/release.yml')
ci_path       = File.join(ROOT, '.github/workflows/ci.yaml')
sync_path     = File.join(ROOT, '.github/workflows/sync-cabal-version.yml')
docs_path     = File.join(ROOT, 'docs/en/releases.md')

# --- M001: the version authority and its release policy

config = read_json(config_path)
package = config.is_a?(Hash) ? config.dig('packages', '.') : nil
want(package.is_a?(Hash), 'release-please-config.json: no packages["."] entry (manifest mode)')
if package.is_a?(Hash)
  want(package['release-type'] == 'simple',
       'release-please-config.json: packages["."].release-type must be "simple"')
  want(package['prerelease'] == true,
       'release-please-config.json: packages["."].prerelease must be true so every  GitHub release is visibly marked a prerelease (R002, I004)')
  want(!package.key?('versioning') || package['versioning'] == 'default',
       'release-please-config.json: the versioning strategy must stay default — a  prerelease version x.y.z-label.n has no legal Cabal PVP form and would make  the version seam unsatisfiable (D001, I001)')
end

manifest = read_json(manifest_path)
if manifest.is_a?(Hash)
  want(manifest.keys == ['.'],
       ".release-please-manifest.json: expected exactly the \".\" key, got #{manifest.keys.inspect}")
  want(manifest['.'].to_s.match?(/\A(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)\z/),
       ".release-please-manifest.json: version #{manifest['.'].inspect} is not strict SemVer x.y.z (D001)")
end

# --- M004: the release workflow

release = read_yaml(release_path)
if release
  on = triggers(release)
  want(on.key?('push'), 'release.yml: must run on push to master')
  push_branches = on['push'].is_a?(Hash) ? Array(on['push']['branches']) : []
  want(push_branches.include?('master'), 'release.yml: push trigger must include the master branch')
  want(on.key?('workflow_dispatch'), 'release.yml: must keep workflow_dispatch for manual re-runs')
  want(!on.key?('pull_request') && !on.key?('pull_request_target'),
       'release.yml: must not be triggered by pull requests — the pipeline cannot be  allowed to publish from a pull-request run (I007)')

  jobs = jobs_of(release)
  release_job_name, release_job = jobs.find do |_, job|
    steps_of(job).any? { |step| step['uses'].to_s.include?('release-please-action') }
  end
  want(release_job, 'release.yml: no job uses googleapis/release-please-action (R002)')

  if release_job
    outputs = release_job['outputs'].is_a?(Hash) ? release_job['outputs'] : {}
    want(outputs.key?('release_created'),
         "release.yml: job #{release_job_name} must export a release_created output (D003)")
    want(outputs['tag_name'].to_s.include?('tag_name'),
         "release.yml: job #{release_job_name} must export the action's tag_name output (D003, I003)")

    publish_name, publish_job = jobs.find do |name, job|
      name != release_job_name &&
        steps_of(job).any? { |step| step['run'].to_s.include?('scripts/release/package-release-artifact') }
    end
    want(publish_job,
         'release.yml: no job runs scripts/release/package-release-artifact (R006)')

    if publish_job
      want(Array(publish_job['needs']).include?(release_job_name),
           "release.yml: job #{publish_name} must depend on #{release_job_name}")
      want(publish_job['if'].to_s.include?("#{release_job_name}.outputs.release_created"),
           "release.yml: job #{publish_name} must be gated on release_created, so merging " \
           'the automation branch alone publishes nothing (R009, D003)')

      checkout = steps_of(publish_job).find { |step| step['uses'].to_s.start_with?('actions/checkout') }
      want(checkout, "release.yml: job #{publish_name} must check out the repository")
      checked_out_ref = checkout.is_a?(Hash) && checkout['with'].is_a?(Hash) ? checkout['with']['ref'].to_s : ''
      want(checked_out_ref.include?("#{release_job_name}.outputs.tag_name"),
           "release.yml: job #{publish_name} must check out the emitted tag_name, not a " \
           'mutable branch — the published asset\'s provenance is the tag (I003)')

      publish_runs = steps_of(publish_job).map { |step| step['run'].to_s }
      want(publish_runs.any? { |run| run.include?('gh release upload') },
           "release.yml: job #{publish_name} must upload the asset to the GitHub release (R006)")
      want(publish_runs.any? { |run| run.include?('--prerelease') },
           "release.yml: job #{publish_name} must assert the release is a prerelease, so " \
           'provisional status does not depend on configuration alone (I004)')
    end
  end
end

# --- M003: continuous integration

ci = read_yaml(ci_path)
if ci
  want(triggers(ci).key?('workflow_dispatch'),
       'ci.yaml: workflow_dispatch must stay — it is the only no-secret way to give a  bot-created release pull request its required checks (R005, I002)')

  ci_runs = all_runs(ci)
  want(ci_runs.any? { |run| run.match?(/nix\s.*develop/) && run.include?('cabal build') },
       'ci.yaml: the job that enters nix develop and runs a Cabal build must stay; a  packaged Nix build does not exercise the dev shell (R010)')
  want(ci_runs.any? { |run| run.include?('scripts/release/check-release-version') },
       'ci.yaml: manifest/Cabal drift must be rejected on every run (R003, I001)')
  want(ci_runs.any? { |run| run.include?('scripts/release/check-release-wiring') },
       'ci.yaml: these wiring assertions must themselves run in CI, or they cannot fail')
end

# --- M002: release-pull-request synchronization

sync = read_yaml(sync_path)
if sync
  on = triggers(sync)
  pull_request = on['pull_request'].is_a?(Hash) ? on['pull_request'] : {}
  want(on.key?('pull_request'), 'sync-cabal-version.yml: must run on pull_request (R003)')
  missing_types = %w[opened synchronize] - Array(pull_request['types'])
  want(missing_types.empty?,
       "sync-cabal-version.yml: pull_request types must include #{missing_types.join(', ')}")

  want(on.key?('workflow_dispatch'),
       'sync-cabal-version.yml: needs workflow_dispatch — a bot-created release pull  request fires no pull_request event, so without it the bumped manifest can  never be propagated into the Cabal file and CI stays red (R003, R005, I002)')

  sync_job = jobs_of(sync).values.first
  guard = sync_job.is_a?(Hash) ? sync_job['if'].to_s : ''
  want(guard.include?('release-please--'),
       'sync-cabal-version.yml: the job must be guarded to release-please branches only')
  want(guard.include?('workflow_dispatch') && guard.include?('ref_name'),
       'sync-cabal-version.yml: the guard must admit workflow_dispatch through  github.ref_name — github.head_ref is empty on a dispatch, so a head_ref-only  guard makes the recovery surface silently skip instead of running')
  sync_checkout = steps_of(sync_job).find { |step| step['uses'].to_s.start_with?('actions/checkout') }
  sync_ref = sync_checkout.is_a?(Hash) && sync_checkout['with'].is_a?(Hash) ? sync_checkout['with']['ref'].to_s : ''
  want(sync_ref.include?('ref_name'),
       'sync-cabal-version.yml: checkout must resolve the branch on dispatch runs too')
  want(all_runs(sync).any? { |run| run.include?('scripts/release/sync-cabal-version') },
       'sync-cabal-version.yml: must run scripts/release/sync-cabal-version, not an  inline copy of the version semantics (F002)')
end


# --- M006: operator documentation

if File.file?(docs_path)
  docs = File.read(docs_path)
  sync_dispatch = 'gh workflow run "Sync Cabal version" --ref release-please--branches--master'
  ci_dispatch = 'gh workflow run CI --ref release-please--branches--master'

  want(docs.include?(ci_dispatch),
       "#{docs_path}: must name the exact bot-PR CI recovery command (R005, I002)")
  want(docs.include?(sync_dispatch),
       "#{docs_path}: must name the synchronization dispatch — it is the first step " \
       'of the no-secret recovery, and CI alone cannot clear the drift it guards')
  if docs.include?(sync_dispatch) && docs.include?(ci_dispatch)
    want(docs.index(sync_dispatch) < docs.index(ci_dispatch),
         "#{docs_path}: synchronization must be dispatched BEFORE CI; the reverse " \
         'order leaves a red drift check that nothing on the branch can clear')
  end

  want(docs.include?('--pattern "$ASSET"'),
       "#{docs_path}: the stranger fetch must bind the requested tag to the exact " \
       'asset name with --pattern; a wildcard happily accepts another release\'s  archive, which downloads, extracts and smokes perfectly (D004, F004)')
  want(docs.include?('grep -F "release tag: $TAG"'),
       "#{docs_path}: the stranger fetch must reconcile the requested tag against " \
       'the tag the archive itself declares in PROVISIONAL.md (D004, I006)')
  want(docs.include?('reactivegas-server-'),
       "#{docs_path}: must document the release asset naming (I004)")
  want(docs.include?('gh release download'),
       "#{docs_path}: must document the stranger fetch of the published asset (R008, I006)")
else
  failed("#{docs_path}: missing release operator documentation (M006)")
end

if FAILURES.empty?
  puts "release wiring: #{ROOT} satisfies every structural release assertion"
  exit 0
end

warn "check-release-wiring: #{FAILURES.length} assertion(s) failed against #{ROOT}"
FAILURES.each { |message| warn "  - #{message}" }
exit 1
