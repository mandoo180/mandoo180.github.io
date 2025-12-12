#!/bin/bash
# retarget-skills.sh
# Retarget all Claude Code skill files to a new base directory
#
# Usage: ./retarget-skills.sh /path/to/new/base/dir
# Example: ./retarget-skills.sh /path/to/project/.claude/skills

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

CONFIGS_DIR="content/agents/claude-code"

if [ -z "$1" ]; then
    echo -e "${RED}Error: No target directory specified${NC}"
    echo ""
    echo "Usage: $0 <new-base-directory>"
    echo ""
    echo "Examples:"
    echo "  $0 ~/.claude/skills                     # Global skills"
    echo "  $0 /path/to/project/.claude/skills       # Project-specific"
    echo "  $0 \$(pwd)/.claude/skills                 # Current project"
    echo ""
    exit 1
fi

NEW_BASE_DIR="$1"
OLD_BASE_DIR="~/.claude/skills"

echo -e "${GREEN}Retargeting Claude Code skills...${NC}"
echo ""
echo "  From: ${OLD_BASE_DIR}"
echo "  To:   ${NEW_BASE_DIR}"
echo ""

# Confirm with user
read -p "Continue? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 1
fi

# Find all org files in the configs directory (excluding backups)
skill_files=$(find "$CONFIGS_DIR" -name "*.org" ! -name "*.org.backup" ! -name "index.org" ! -name "README.org")

files_updated=0

for file in $skill_files; do
    # Check if file contains :tangle directives
    if grep -q ":tangle $OLD_BASE_DIR" "$file"; then
        echo -e "${YELLOW}Updating:${NC} $(basename "$file")"

        # Use sed to replace the old base directory with the new one
        # macOS sed needs -i '' while GNU sed needs -i
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            sed -i '' "s|:tangle $OLD_BASE_DIR|:tangle $NEW_BASE_DIR|g" "$file"
        else
            # Linux
            sed -i "s|:tangle $OLD_BASE_DIR|:tangle $NEW_BASE_DIR|g" "$file"
        fi

        ((files_updated++))
    fi
done

echo ""
if [ $files_updated -eq 0 ]; then
    echo -e "${YELLOW}No files needed updating${NC}"
else
    echo -e "${GREEN}Updated $files_updated file(s)${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Review changes: git diff $CONFIGS_DIR"
    echo "  2. Tangle skills: ./tangle-claude.sh"
    echo "  3. Restart Claude Code to discover new skill locations"
fi

echo ""
echo -e "${GREEN}Done!${NC}"
