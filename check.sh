#!/bin/bash
cargo fmt -- --check

cargo clippy -- -Dwarnings
