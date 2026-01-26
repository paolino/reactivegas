# Installation

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- A web server capable of SCGI (nginx, lighttpd, etc.) for production deployment

## Building from Source

Clone the repository and enter the development shell:

```bash
git clone https://github.com/paolino/reactivegas.git
cd reactivegas
nix develop
```

Build the server:

```bash
cabal build server
```

## Running the Server

### Basic Usage

Start the server with a data directory and admin password:

```bash
cabal run server -- /path/to/data mypassword
```

### With Custom Options

```bash
cabal run server -- \
    --port 8080 \
    --sessions 100 \
    --movements 20 \
    --memories 30 \
    /path/to/data mypassword
```

### Command Line Reference

| Option | Short | Default | Description |
|--------|-------|---------|-------------|
| `--port` | `-p` | 5000 | SCGI server port |
| `--movements` | `-m` | 15 | Max group movements in memory |
| `--sessions` | `-s` | 200 | Max sessions in memory |
| `--memories` | `-r` | 20 | Max memories per session |

**Positional arguments:**

1. `DIRECTORY` - Working directory containing group data
2. `PASSWORD` - Administration password for token access

## Directory Structure

The working directory should be organized as follows:

```
data/
├── static/           # Static web assets
│   └── report.html   # Generated reports
├── *.db              # SQLite databases for historical data
└── state/            # Current state files
```

## Web Server Configuration

The server uses SCGI protocol. Configure your web server to proxy requests:

### Nginx Example

```nginx
location /reactivegas {
    include scgi_params;
    scgi_pass 127.0.0.1:5000;
}
```

### Lighttpd Example

```lighttpd
server.modules += ("mod_scgi")
scgi.server = (
    "/reactivegas" => ((
        "host" => "127.0.0.1",
        "port" => 5000
    ))
)
```
