#!/usr/bin/env sh
set -eu

INSTALL_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/knorgpub"
BIN_DIR="${HOME}/.local/bin"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

log() { printf '[knorgpub] %s\n' "$*"; }
die() { printf '[knorgpub] error: %s\n' "$*" >&2; exit 1; }

command -v rsync > /dev/null 2>&1 || die "'rsync' is required but not found in PATH"

log "installing knorgpub..."

if [ "$SCRIPT_DIR" != "$INSTALL_DIR" ]; then
    mkdir -p "$INSTALL_DIR"
    rsync -a --exclude='.git' "$SCRIPT_DIR/" "$INSTALL_DIR/"
    log "copied to $INSTALL_DIR"
fi

chmod +x "$INSTALL_DIR/bin/knorgpub"

mkdir -p "$BIN_DIR"
rm -f "$BIN_DIR/knorgpub"
cat > "$BIN_DIR/knorgpub" << EOF
#!/usr/bin/env sh
KNORGPUB_FRAMEWORK_DIR="$INSTALL_DIR" exec "$INSTALL_DIR/bin/knorgpub" "\$@"
EOF
chmod +x "$BIN_DIR/knorgpub"

log "installed: $BIN_DIR/knorgpub"
log ""
log "make sure $BIN_DIR is on your PATH"
log "then run 'knorgpub init' from any new project directory"
