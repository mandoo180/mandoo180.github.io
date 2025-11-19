#!/usr/bin/env python3
"""
Split the large emacs-configs.org file into modular pieces.
"""

def extract_section(lines, start_line, end_line):
    """Extract lines from start to end (1-indexed)."""
    return lines[start_line-1:end_line]

def main():
    # Read the original file
    with open('content/emacs/emacs-configs.org', 'r') as f:
        lines = f.readlines()

    # Section boundaries (line numbers from our analysis)
    sections = [
        # Already done: Early Init (6-81)
        {
            'name': '02-init',
            'start': 82,
            'end': 356,
            'title': 'Core Initialization',
            'intro': 'Core Emacs settings using use-package. This configures built-in Emacs behavior without external packages.'
        },
        {
            'name': '03-theme',
            'start': 357,
            'end': 1543,
            'title': 'Theme and Appearance',
            'intro': 'Visual customization including color theme, mode line, and UI elements.'
        },
        {
            'name': '04-window',
            'start': 1544,
            'end': 1589,
            'title': 'Window Management',
            'intro': 'Configuration for window splitting, sizing, and navigation.'
        },
        {
            'name': '05-completion',
            'start': 1590,
            'end': 1676,
            'title': 'Completion System',
            'intro': 'Completion frameworks and interfaces for enhanced productivity.'
        },
        {
            'name': '06-dired',
            'start': 1677,
            'end': 1695,
            'title': 'Directory Editor (Dired)',
            'intro': 'Enhancements for the built-in directory editor.'
        },
        {
            'name': '07-note',
            'start': 1696,
            'end': 2137,
            'title': 'Note Taking',
            'intro': 'Org-mode and note-taking configuration for knowledge management.'
        },
        {
            'name': '08-shell',
            'start': 2138,
            'end': 2250,
            'title': 'Shell Integration',
            'intro': 'Terminal emulators and shell integration within Emacs.'
        },
        {
            'name': '09-programming',
            'start': 2251,
            'end': 3506,
            'title': 'Programming Languages',
            'intro': 'Language-specific configurations and development tools.'
        },
        {
            'name': '10-miscellaneous',
            'start': 3507,
            'end': 3659,
            'title': 'Miscellaneous',
            'intro': 'Utility functions and helper configurations.'
        },
        {
            'name': '11-platform-linux',
            'start': 3660,
            'end': 3678,
            'title': 'Linux-Specific Configuration',
            'intro': 'Settings that only apply on Linux systems.'
        },
        {
            'name': '12-platform-macos',
            'start': 3679,
            'end': 3703,
            'title': 'macOS-Specific Configuration',
            'intro': 'Settings that only apply on macOS systems.'
        },
        {
            'name': '13-platform-windows',
            'start': 3704,
            'end': 3739,
            'title': 'Windows-Specific Configuration',
            'intro': 'Settings that only apply on Windows systems.'
        }
    ]

    # Create each section file
    for section in sections:
        content = extract_section(lines, section['start'], section['end'])

        # Build the new file with header
        output = []
        output.append(f"* {section['title']}\n\n")
        output.append(f"{section['intro']}\n\n")

        # Add the section content (skip the original heading line)
        if content and content[0].startswith('*'):
            content = content[1:]  # Skip the heading

        output.extend(content)

        # Write to file
        filename = f"content/emacs/configs/{section['name']}.org"
        with open(filename, 'w') as f:
            f.writelines(output)

        print(f"Created {filename}")

if __name__ == '__main__':
    main()
