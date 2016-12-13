#!/bin/bash
set -eux

front="${1:-front.pdf}"
verso="${2:-verso.pdf}"
front_name="$(basename "${front}")"
verso_name="$(basename "${verso}")"
output="${3:-${front_name}_${verso_name}.pdf}"
output_name="$(basename "${output}")"
output_dir="$(dirname "${output}")"

[ -e "${output}" ] && { echo "${output} already exists" ; exit 1 ; }

# Let n be the even number of pages (it is even since twice the number of
# scanned papers). I assume that front.pdf contains the pages 1, 3, 5 ... n-1 and
# verso contains n, n-2, n-4 ... 2

front_tmp=`mktemp -d`
verso_tmp=`mktemp -d`
clean ( ) {
    rm -rf "${front_tmp}" "${verso_tmp}"
}
trap clean 0

echo "Bursting the front"
cp "${front}" "${front_tmp}"
pushd "${front_tmp}"
{
    pdftk "${front_name}" burst
    rm "${front_name}"

    # list all pages and rename them
    page_number=1
    ls -1 *.pdf > pages
    while read page
    do
        file_name="`printf "%05d" "${page_number}"`.pdf"
        mv "${page}" "${file_name}"
        page_number="$((page_number + 2))"
    done < pages
}
popd

echo "Bursting the verso"
cp "${verso}" "${verso_tmp}"
pushd "${verso_tmp}"
{
    pdftk "${verso_name}" burst
    rm "${verso_name}"

    # list all pages and rename them
    page_number=2
    ls -r -1 *.pdf > pages
    while read page
    do
        file_name="`printf "%05d" "${page_number}"`.pdf"
        mv "${page}" "${file_name}"
        page_number="$((page_number + 2))"
    done < pages
}
popd

# bring them together
mv "${verso_tmp}"/*pdf "${front_tmp}"
pushd "${front_tmp}"
{
    pdftk *.pdf cat output "${output_name}"
}
popd

# get the output here
mv "${front_tmp}/${output_name}" "${output}"
