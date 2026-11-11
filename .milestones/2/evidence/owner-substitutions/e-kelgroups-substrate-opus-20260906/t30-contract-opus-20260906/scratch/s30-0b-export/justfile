# Build all packages
build:
    cabal build all -O0

# Run tests
test:
    cabal test all -O0 --test-show-details=direct

# Format Haskell sources
format:
    fourmolu -i lib/**/*.hs test/*.hs app/*.hs

# Lint Haskell sources
lint:
    hlint lib/

# Format cabal file
cabal-fmt:
    cabal-fmt -i kelgroups.cabal

# Build Lean proofs
lean:
    cd lean && lake build

# Build PureScript client
build-client:
    cd client && npm install && spago build

# Bundle PureScript client
bundle-client:
    cd client && spago bundle -p kelgroups-trivial

# Format PureScript sources
format-client:
    cd client && purs-tidy format-in-place "kelgroups-client/src/**/*.purs" "kelgroups-trivial/src/**/*.purs"

# Lint PureScript sources
lint-client:
    cd client && spago build

# Test PureScript client
test-client:
    cd client && spago test -p kelgroups-client

# Full CI check
ci: format cabal-fmt lint build test lean build-client test-client

# Build documentation
docs:
    mkdocs build --config-file docs/mkdocs.yml

# Run the server (with static file serving)
serve port="8080" db="kelgroups.db" pass="bootstrap": bundle-client
    cabal run kelgroups-server -O0 -- {{port}} {{db}} {{pass}}

# Restart the server (rebundle + relaunch)
restart port="8080" db="kelgroups.db" pass="bootstrap": bundle-client
    -pkill -f "kelgroups-server"
    cabal run kelgroups-server -O0 -- {{port}} {{db}} {{pass}}

# Clean build artifacts
clean:
    cabal clean
    cd lean && lake clean
    cd client && rm -rf .spago output node_modules
