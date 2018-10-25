#!/bin/sh
set -ex

lalrpop src/grammar.lalrpop
cargo build --release --target wasm32-unknown-unknown
wasm-bindgen target/wasm32-unknown-unknown/release/lambdacube.wasm --out-dir www

cd www && npm run start
