#!/bin/bash -eux

ext="${1:-jpg}"
fps="${2:-1}"
TMPTL="$(mktemp -d tl.XXX)"
clean () {
    rm -rf "${TMPTL}"
}
trap clean EXIT

number=0
copy_to_tl () {
    local file="${1}"
    local dest="${TMPTL}/$(printf '%04d' "${number}").${ext}"
    cp "${file}" "${dest}"
    number=$((number + 1))
}

# repeat a few time the first one, to emphasize the starting effect
first_one="$(ls *${ext}|head -1)"
for i in {1..4}
do
    copy_to_tl "${first_one}"
done

for i in *${ext}
do
    copy_to_tl "${i}"
done

# repeat a few time the last one, to emphasize the finish effect
last_one="$(ls *${ext}|tail -1)"
for i in {1..4}
do
    copy_to_tl "${last_one}"
done

# http://techedemic.com/2014/09/18/creating-a-timelapse-clip-with-avconv/
#       -vf "tblend=average,framestep=2,setpts=0.50*PTS" \

ffmpeg -y \
       -r "${fps}" \
       -i "${TMPTL}/%4d.${ext}" \
       -r "${fps}" \
       -vcodec mpeg4 -q:v 3 \
       output.mkv
