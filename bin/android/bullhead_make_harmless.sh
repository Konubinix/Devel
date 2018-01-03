#!/bin/bash

cat <<EOF > "/bin/kbd_mode"
#!/bin/bash
exit 1
EOF
cat <<EOF > "/bin/setupcon"
#!/bin/bash
exit 1
EOF
