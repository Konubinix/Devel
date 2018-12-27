#!/bin/bash

set -eux
#sudo aptitude install debootstrap
arch=${1:-armhf}

# testing won't work because of resolv.conf problems
sudo debootstrap --arch=${arch} --variant=minbase --foreign stable debian-${arch} http://httpredir.debian.org/debian
sudo cp $(which android_setup_perso.sh) "debian-${arch}/sbin/"
sudo cp $(which bullhead_make_harmless.sh) "debian-${arch}/sbin/"
sudo cp $(which setup_debian_second_stage.sh) "debian-${arch}/sbin/"
# for some obscure reason, kbd_mode make android reboot
cat <<EOF > "kbd_mode"
#!/bin/bash
exit 1
EOF
sudo mv kbd_mode "debian-${arch}/bin/"

cat <<EOF > "setupcon"
#!/bin/bash
exit 1
EOF
sudo mv setupcon "debian-${arch}/bin"

sudo tar cf debian-${arch}.tar debian-${arch}
adb push debian-${arch}.tar /sdcard/

debinstall.sh bullhead
cat <<EOF > bullheadsetup_phase_one.sh
#!/bin/bash

set -eux
rm -rf /data/debian*
cp /sdcard/debian-${arch}.tar /data/debian-${arch}.tar
cd /data
tar xf debian-${arch}.tar
rm debian-${arch}.tar
mv debian-${arch} debian
EOF
adb push bullheadsetup_phase_one.sh /sdcard/

adb shell su -c "bash /sdcard/bullheadsetup_phase_one.sh"
echo "run su -c deb.sh, then setup_debian_second_stage.sh, then reboot the phone"
adb shell
