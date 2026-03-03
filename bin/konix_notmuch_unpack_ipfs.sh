#!/usr/bin/env bash

ID="${1}"
dir=`mktemp -d`
file="${dir}/msg"

notmuch show --format=raw "${ID}" > "${file}"
trap "rm -r $dir" EXIT
clk mail unpack-rfc822 "${file}" --output "${dir}"

# Generate index.html with file listing (and iframe for HTML content)
python3 - "${dir}" <<'PYEOF'
import os, sys, re, html as h

output_dir = sys.argv[1]
files = [f for f in sorted(os.listdir(output_dir)) if f not in ("index.html", "msg")]
html_files = [f for f in files if f.endswith('.html')]

page = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Message</title>
<base target="_blank">
<style>
body { background: #3F3F3F; color: #DCDCCC; font-family: monospace; font-size: 14px; margin: 0; padding: 0; }
.msg-content { background: #DCDCCC; color: #000; padding: 8px; min-height: 100vh; }
.file-list { padding: 12px 20px; border-top: 1px solid #4F4F4F; }
.file-list a { color: #8CD0D3; text-decoration: none; }
.file-list a:hover { text-decoration: underline; color: #93E0E3; }
</style>
</head><body>"""

if len(html_files) == 1:
    html_path = os.path.join(output_dir, html_files[0])
    with open(html_path, 'r', errors='replace') as hf:
        html_content = hf.read()
    # Extract body content (or use whole content if no body tag)
    body_match = re.search(r'<body[^>]*>(.*)</body>', html_content, re.DOTALL | re.IGNORECASE)
    body_content = body_match.group(1) if body_match else html_content
    # Extract style tags to preserve them
    styles = re.findall(r'<style[^>]*>.*?</style>', html_content, re.DOTALL | re.IGNORECASE)
    if styles:
        page += '\n' + '\n'.join(styles)
    page += f'\n<div class="msg-content">{body_content}</div>'

page += '\n<div class="file-list"><p>Files:</p><ul>'
for fname in files:
    page += f'\n<li><a href="{h.escape(fname)}">{h.escape(fname)}</a></li>'
page += "\n</ul></div>\n</body></html>"

with open(os.path.join(output_dir, "index.html"), "w") as f:
    f.write(page)
PYEOF

url="${KONIX_IPFS_GATEWAY}$(ipfa "${dir}"|sed -r 's/^(.+)\?.+/\1/')/"
if [ -n "$DISPLAY" ]
then
    echo -n "${url}" | konix_xclip_in_all.sh
fi
echo "${url}"
