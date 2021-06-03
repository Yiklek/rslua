# rslua

rslua is a lua interpreter implemented in Rust. This implementation is compatible with Lua 5.3, and mainly references to *WRITE YOUR OWN LUA VM,COMPILER AND STANDARD LIBRARY*.
Please feel free to give me PR.

## milestone

- [x] parse binary chunk
- [x] lua stack and lua state
- [ ] vm instruction(41/47)
- [x] invoke rust function
- [ ] upvalue support
- [ ] meta programming
- [ ] compiler
- [ ] stdlib
- [ ] rust dynamic ABI
- [ ] async concurrent
- [ ] just in time(JIT)

## run

You can run all test cases by using the following command.

```bash
cargo test
```

## LICENSE

This project is licensed under the [MIT license](LICENSE)
