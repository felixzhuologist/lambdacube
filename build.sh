#!/bin/sh
set -ex

lalrpop src/grammar.lalrpop
cargo build --target wasm32-unknown-unknown
wasm-bindgen target/wasm32-unknown-unknown/debug/lambdacube.wasm --out-dir www

cd www && npm run start
