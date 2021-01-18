#!/bin/bash -eu

nomad_list_eligible () {
    nomad node status -json|jq -r '.[] | select(.SchedulingEligibility == "eligible").Name'
}

nomad_nodes () {
    nomad node status -json|jq -r '.[].Name'
}

nomad_list_ineligible () {
    nomad node status -json|jq -r '.[] | select(.SchedulingEligibility == "ineligible").Name'
}

nomad_name_to_id () {
    local name="$1"
    nomad node status -json|jq -r ".[] | select(.Name == \"${name}\").ID"
}
