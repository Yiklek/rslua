---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by Yiklek.
--- DateTime: 2021/6/3 下午9:44
---
local function a()
    local b = function()
        return 2, 3
    end
    return 1, b()
end
local function assert(v)
    if not v then
        fail()
    end
end

local c1,c2,c3 = a()
assert(c1 == 1)
assert(c2 == 2)
assert(c3 == 3)