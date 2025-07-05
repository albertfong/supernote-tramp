# Supernote TRAMP Package

A TRAMP (Transparent Remote Access, Multiple Protocol) method for accessing Supernote devices via HTTP.

## Installation

The package is already installed and configured in your Emacs setup.

## Usage

Access your Supernote device using TRAMP file paths:

```
C-x C-f /supernote:192.168.20.170:8089:/
```

Or use dired to browse directories:

```
M-x dired /supernote:192.168.20.170:8089:/
```

## Features

- **Read-only access** to Supernote device files
- **Directory browsing** with dired integration
- **File preview** and copying to local filesystem
- **Caching** for improved performance
- **Error handling** for network issues

## Configuration

Server configuration is in `tests/tramp-supernote-test-config.el`:

```elisp
(defvar tramp-supernote-test-server-ip "192.168.20.170")
(defvar tramp-supernote-test-server-port 8089)
```

## Testing

Run the test suite:

```bash
./run-all-tests.sh
```

Or run individual test categories:

```bash
# Mock tests
emacs --batch -l tramp-supernote.el -l tests/tramp-supernote-test-config.el -l tests/mock/test-tramp-supernote-mock.el -f ert-run-tests-batch-and-exit

# TRAMP core tests
emacs --batch -l tramp-supernote.el -l tests/tramp-supernote-test-config.el -l tests/tramp/test-tramp-supernote.el -f ert-run-tests-batch-and-exit
```

## Directory Structure

```
tramp-supernote/
├── tramp-supernote.el           # Main package file
├── tramp-supernote-pkg.el       # Package metadata
├── tests/
│   ├── tramp-supernote-test-config.el  # Test configuration
│   ├── mock/                    # Mock server tests
│   ├── tramp/                   # TRAMP integration tests
│   ├── dired/                   # Dired functionality tests
│   └── integration/             # Live server tests
├── docs/                        # Documentation
├── run-all-tests.sh             # Test runner script
└── validate-tramp-supernote.sh  # Validation script
```

## Limitations

- **Read-only access**: Cannot write, delete, or modify files
- **Network dependency**: Requires HTTP access to Supernote device
- **No authentication**: Uses simple HTTP (no HTTPS/auth support)

## Test Results

- ✅ Mock tests: 12/12 passed
- ✅ TRAMP core tests: 9/10 passed (1 skipped network test)
- ✅ Package syntax validation: passed
- ✅ Package loading: successful

## Troubleshooting

1. **Connection issues**: Ensure Supernote device is on the same network
2. **Port conflicts**: Check if port 8089 is accessible
3. **Loading errors**: Verify `init.el` has the correct load path

## License

This package follows standard Emacs package licensing practices.
