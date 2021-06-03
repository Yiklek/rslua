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

    #[test]
    fn simple_vm() {
        lua_main(&"tests/vm.out")
    }
    #[test]
    fn function() {
        lua_main(&"tests/function.out")
    }
}