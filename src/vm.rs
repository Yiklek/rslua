use crate::api::LuaApi;
use crate::chunk::read_file;
use crate::state::LuaState;

pub type PCAddr = isize;


pub fn lua_main(file: &dyn ToString) {
    let mut state = LuaState::new();
    state.load(read_file(file), file.to_string().as_str(), "b");
    state.call(0, 0);
}

#[cfg(test)]
mod tests {
    use crate::api::{LuaApi, LuaVM};
    use crate::chunk::{print_chunk, read_file};
    use crate::state::LuaState;
    use crate::vm::lua_main;

    #[test]
    fn simple_vm() {
        lua_main(&"tests/vm.out")
    }
    #[test]
    fn function() {
        let path = "tests/function.out";
        print_chunk(&path);
        lua_main(&path)
    }
    #[test]
    fn function_return_fix() {
        let path = "tests/function_return_fix.out";
        print_chunk(&path);
        lua_main(&path);
    }
    #[test]
    fn set_list_tail_func() {
        let path = "tests/set_list_tail_func.out";
        print_chunk(&path);
        lua_main(&path);
    }
    fn print(ls: &mut dyn LuaVM) -> usize {
        let nargs = ls.get_top();
        for i in 1..(nargs + 1) {
            if ls.is_boolean(i) {
                print!("{}", ls.to_boolean(i));
            } else if ls.is_string(i) {
                print!("{}", ls.to_string(i));
            } else {
                print!("{}", ls.get(i).borrow());
            }
            if i < nargs {
                print!("\t")
            }
        }
        println!();
        return 0;
    }

    #[test]
    fn rust_fn() {
        let file = &"tests/rust_fn.out";
        let mut state = LuaState::new();
        state.register("print".to_string(), print);
        state.load(read_file(file), file.to_string().as_str(), "b");
        state.call(0, 0);
    }

    #[test]
    fn upv() {
        let file = &"tests/upvalue.out";
        print_chunk(file);
        let mut state = LuaState::new();
        state.register("print".to_string(), print);
        state.load(read_file(file), file.to_string().as_str(), "b");
        state.call(0, 0);
    }
}