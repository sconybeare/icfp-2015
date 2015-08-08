# ICFP 2015

UIUC SIGPLAN's entry to the ICFP 2015 contest

# Update to latest version of Cabal.
```console
cabal update
cabal install cabal-install
```

# Initialize a sandbox and install the package's dependencies.
```console
make install
```

# For Nix users:
```console
make nix-shell
```

# Configure & build the package.
```console
make configure
make build
```

# Run executable.
```console
make run
```
# Start REPL.
```console
make repl
```

# Generate documentation.
```console
make haddock
```
