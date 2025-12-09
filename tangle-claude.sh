#!/bin/bash
# Tangle Claude Code configurations from org files

set -e

echo "Tangling Claude Code configurations..."

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIGS_DIR="$SCRIPT_DIR/content/agents/claude-code"

if [ ! -d "$CONFIGS_DIR" ]; then
    echo "Error: Directory $CONFIGS_DIR not found"
    exit 1
fi

# Counter for processed files
count=0

# Find all org files (excluding backups) and tangle them
find "$CONFIGS_DIR" -name "*.org" ! -name "*.org.backup" ! -name "index.org" ! -name "README.org" -print0 | while IFS= read -r -d '' file; do
    echo "Tangling: $(basename "$file")"
    emacs -Q --batch \
        --eval "(require 'org)" \
        --eval "(setq org-confirm-babel-evaluate nil)" \
        --eval "(find-file \"$file\")" \
        --eval "(org-babel-tangle)" \
        --kill
    ((count++))
done

echo ""
echo "Done! Tangled configuration files from content/agents/claude-code/"
echo "Generated configs:"
echo "  - .claude/settings.json (project settings)"
echo "  - .claude/settings.local.json (personal overrides)"
echo "  - .claude/commands/*.md (slash commands)"
echo "  - .claude/skills/*/SKILL.md (agent skills)"
echo "  - .mcp.json (MCP server configuration)"
echo ""
echo "IMPORTANT: Before using the MCP server configs:"
echo "  1. Edit content/agents/claude-code/04-mcp-servers.org"
echo "  2. Update the placeholder paths to your actual MCP server locations"
echo "  3. Re-run this script to regenerate .mcp.json"
echo ""
echo "To use the configurations:"
echo "  1. Review generated files in .claude/ and .mcp.json"
echo "  2. Restart or reload Claude Code to pick up changes"
echo "  3. Test with /command-name for slash commands"
echo "  4. Skills activate automatically based on context"
