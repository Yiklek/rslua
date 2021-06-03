use crate::chunk::{read_file};
use crate::state::{LuaState};
use crate::api::{LuaApi};

pub type PCAddr = isize;


pub fn lua_main(file: &dyn ToString){
    let mut state = LuaState::new();
    state.load(read_file(file), file.to_string().as_str(), "b");
    state.call(0,0);
}
#[cfg(test)]
mod tests {
    use crate::vm::{lua_main};
    use crate::chunk::print_chunk;

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
}