#!/usr/bin/env bash
set -eu

JSON_FILE="${1}"
dir=$(mktemp -d)
trap "rm -r $dir" EXIT

# Extract and unpack each message
i=0
for ID in $(jq -r '.[].id' "${JSON_FILE}"); do
    msgdir="${dir}/$(printf '%03d' $i)"
    mkdir -p "${msgdir}"
    file="${msgdir}/msg"
    notmuch show --format=raw "${ID}" > "${file}"
    clk mail unpack-rfc822 "${file}" --output "${msgdir}"
    rm -f "${file}"
    i=$((i + 1))
done

# Generate index.html with thread tree
python3 - "${JSON_FILE}" "${dir}" <<'PYEOF'
import json
import sys
import os
import html

json_file = sys.argv[1]
output_dir = sys.argv[2]

with open(json_file) as f:
    messages = json.load(f)

def find_nav_page(msgdir):
    """Return the path to the index.html nav page in a message directory."""
    return os.path.join(msgdir, "index.html")

def is_last_sibling(messages, idx):
    """Check if message at idx is the last child of its parent depth."""
    depth = messages[idx]["depth"]
    for j in range(idx + 1, len(messages)):
        if messages[j]["depth"] <= depth:
            if messages[j]["depth"] == depth:
                return False
            return True
    return True

def has_children(messages, idx):
    """Check if message at idx has children (next message is deeper)."""
    if idx + 1 < len(messages):
        return messages[idx + 1]["depth"] > messages[idx]["depth"]
    return False

def build_prefix(messages, idx):
    """Build the tree prefix with box-drawing characters."""
    depth = messages[idx]["depth"]
    if depth == 0 and len(messages) == 1:
        return "─►"
    prefix_parts = []
    # Build continuation lines for ancestors
    for d in range(depth):
        # Find if there's a still-open ancestor at this depth
        ancestor_open = False
        for j in range(idx + 1, len(messages)):
            if messages[j]["depth"] <= d:
                break
            if messages[j]["depth"] == d:
                # There IS a later sibling at this depth => draw │
                # but actually we need to check if there's a sibling
                pass
        # Check if there's a later message at depth d
        has_later = False
        for j in range(idx + 1, len(messages)):
            if messages[j]["depth"] < d:
                break
            if messages[j]["depth"] == d:
                has_later = True
                break
        if has_later:
            prefix_parts.append("│")
        else:
            prefix_parts.append(" ")
    # Now the connector for this node
    last = is_last_sibling(messages, idx)
    children = has_children(messages, idx)
    if depth == 0:
        if children:
            connector = "┬►"
        else:
            connector = "─►"
    else:
        if last and children:
            connector = "╰┬►"
        elif last and not children:
            connector = "╰─►"
        elif not last and children:
            connector = "├┬►"
        else:
            connector = "├─►"
    prefix_parts.append(connector)
    return "".join(prefix_parts)

NAV_STYLE = """<style>
.thread-nav {
  background: #2B2B2B;
  padding: 8px 12px;
  font-family: monospace;
  font-size: 13px;
  border-bottom: 1px solid #4F4F4F;
  margin-bottom: 12px;
}
.thread-nav a {
  color: #8CD0D3;
  text-decoration: none;
  margin-right: 16px;
}
.thread-nav a:hover {
  text-decoration: underline;
  color: #93E0E3;
}
.thread-nav .disabled {
  color: #656555;
  margin-right: 16px;
}
</style>"""

def find_parent_idx(messages, idx):
    """Find the index of the parent message (closest preceding message with depth-1)."""
    depth = messages[idx]["depth"]
    if depth == 0:
        return None
    for j in range(idx - 1, -1, -1):
        if messages[j]["depth"] == depth - 1:
            return j
    return None

def msg_link_from(from_dir, to_idx):
    """Build relative link from a message dir to another message's nav page."""
    to_dir = os.path.join(output_dir, f"{to_idx:03d}")
    target = find_nav_page(to_dir)
    return os.path.relpath(target, from_dir)

def build_nav_bar(messages, idx):
    """Build an HTML navigation bar for message at idx."""
    msgdir = os.path.join(output_dir, f"{idx:03d}")
    parts = []
    parts.append('<div class="thread-nav">')
    # Index link
    index_rel = os.path.relpath(os.path.join(output_dir, "index.html"), msgdir)
    parts.append(f'<a href="{html.escape(index_rel)}">[index]</a>')
    # Parent (reply-to)
    parent_idx = find_parent_idx(messages, idx)
    if parent_idx is not None:
        parts.append(f'<a href="{html.escape(msg_link_from(msgdir, parent_idx))}">[parent]</a>')
    else:
        parts.append('<span class="disabled">[parent]</span>')
    # Prev
    if idx > 0:
        parts.append(f'<a href="{html.escape(msg_link_from(msgdir, idx - 1))}">[prev]</a>')
    else:
        parts.append('<span class="disabled">[prev]</span>')
    # Next
    if idx < len(messages) - 1:
        parts.append(f'<a href="{html.escape(msg_link_from(msgdir, idx + 1))}">[next]</a>')
    else:
        parts.append('<span class="disabled">[next]</span>')
    parts.append('</div>')
    return "\n".join(parts)

def build_msg_index(messages, idx):
    """Create index.html for a message dir with nav bar, content, and file listing."""
    msgdir = os.path.join(output_dir, f"{idx:03d}")
    if not os.path.isdir(msgdir):
        return
    files = [f for f in sorted(os.listdir(msgdir)) if f != "index.html"]
    html_files = [f for f in files if f.endswith('.html')]
    page = f"""<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>{html.escape(messages[idx]["subject"])}</title>
{NAV_STYLE}
<style>
body {{ background: #3F3F3F; color: #DCDCCC; font-family: monospace; font-size: 14px; margin: 0; padding: 0; }}
.msg-frame {{ width: 100%; border: none; min-height: 60vh; background: #DCDCCC; }}
.file-list {{ padding: 12px 20px; border-top: 1px solid #4F4F4F; }}
.file-list a {{ color: #8CD0D3; text-decoration: none; }}
.file-list a:hover {{ text-decoration: underline; color: #93E0E3; }}
</style>
</head><body>
{build_nav_bar(messages, idx)}"""
    if len(html_files) == 1:
        page += f'\n<iframe class="msg-frame" src="{html.escape(html_files[0])}"></iframe>'
    page += '\n<div class="file-list"><p>Files:</p><ul>'
    for fname in files:
        page += f'\n<li><a href="{html.escape(fname)}">{html.escape(fname)}</a></li>'
    page += "\n</ul></div>\n</body></html>"
    with open(os.path.join(msgdir, "index.html"), "w") as f:
        f.write(page)

# Create index.html with nav + file listing in every message dir
for idx in range(len(messages)):
    build_msg_index(messages, idx)

# Build index.html
lines = []
subject = html.escape(messages[0]["subject"]) if messages else "Thread"
lines.append(f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>{subject}</title>
<style>
body {{
  background: #3F3F3F;
  color: #DCDCCC;
  font-family: monospace;
  font-size: 14px;
  padding: 20px;
  line-height: 1.6;
}}
h1 {{
  color: #8CD0D3;
  font-size: 16px;
  border-bottom: 1px solid #4F4F4F;
  padding-bottom: 8px;
  margin-bottom: 16px;
}}
a {{
  color: #8CD0D3;
  text-decoration: none;
}}
a:hover {{
  text-decoration: underline;
  color: #93E0E3;
}}
.tree-line {{
  white-space: pre;
}}
.date {{
  color: #7F9F7F;
}}
.from {{
  color: #F0DFAF;
}}
.connector {{
  color: #656555;
}}
</style>
</head>
<body>
<h1>{subject}</h1>
""")

for idx, msg in enumerate(messages):
    prefix = build_prefix(messages, idx)
    msgdir_name = f"{idx:03d}"
    link = find_nav_page(os.path.join(output_dir, msgdir_name))
    # Make link relative
    link = os.path.relpath(link, output_dir)
    date = html.escape(msg["date"])
    sender = html.escape(msg["from"])
    subj = html.escape(msg["subject"])
    # Truncate long subjects
    max_subj = 80
    if len(msg["subject"]) > max_subj:
        subj = html.escape(msg["subject"][:max_subj]) + "…"
    lines.append(
        f'<div class="tree-line">'
        f'<span class="date">{date:>16}</span>  '
        f'<span class="from">{sender:>20.20}</span>  '
        f'<span class="connector">{prefix}</span>'
        f'<a href="{html.escape(link)}">{subj}</a>'
        f'</div>'
    )

lines.append("""</body>
</html>""")

index_path = os.path.join(output_dir, "index.html")
with open(index_path, "w") as f:
    f.write("\n".join(lines))
PYEOF

url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}" | sed -r 's/^(.+)\?.+/\1/')/"
if [ -n "$DISPLAY" ]; then
    echo -n "${url}" | konix_xclip_in_all.sh
fi
echo "${url}"
