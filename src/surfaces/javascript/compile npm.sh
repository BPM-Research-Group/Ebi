wasm-pack build -- --features javascript
sed -i 's/"name": "ebi"/"name": "ebi_pm"/' pkg/package.json
wasm-pack pack
npm login
wasm-pack publish
