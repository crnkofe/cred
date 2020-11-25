#!/bin/bash

# fail fast and fix things
cargo fmt -- --check
if [ $? -eq 0 ]
then
	echo "Formatter check succeeded."
else
	echo "Formatting check failed"
	sys.exit(1)
fi

cargo clippy -- -Dwarnings
if [ $? -eq 0 ]
then
	echo "Clippy check succeeded."
else
	echo "Clippy check failed"
	sys.exit(1)
fi
