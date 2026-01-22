#!/bin/bash

# Script to run hospital transmission calibration in Docker

set -e

# Function to show usage
usage() {
    echo "Usage: $0 <script.R> [-- <args...>]"
    echo "Examples:"
    echo "  $0 scripts/calibrate_quick.R"
    echo "  $0 scripts/calibrate.R"
    echo "  $0 scripts/calibrate.R -- --vanilla"
    exit 1
}

if [ $# -eq 0 ]; then
    echo "Missing script argument."
    usage
fi

SCRIPT_PATH="$1"
shift

EXTRA_ARGS=()
if [ "${1:-}" = "--" ]; then
    shift
    EXTRA_ARGS=("$@")
fi

# Build the Docker image
echo "Building Docker image..."
docker-compose build

echo "Running: ${SCRIPT_PATH}"
docker-compose run --rm calibration "${SCRIPT_PATH}" "${EXTRA_ARGS[@]}"

echo "Calibration completed. Results are in the out*/ directories."