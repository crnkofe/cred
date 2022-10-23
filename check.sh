#!/bin/bash

# fail fast and fix things
cargo fmt -- --check
if [ $? -eq 0 ]
then
	echo "Formatter check succeeded."
else
	echo "Formatting check failed"
	echo "To fix formatting run #cargo fmt --all"
fi

cargo clippy -- -Dwarnings
if [ $? -eq 0 ]
then
	echo "Clippy check succeeded."
else
	echo "Clippy check failed"
fi
