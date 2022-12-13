const std = @import("std");

const Item = u32;

const Operation = struct {
    op_fun: *const fn(old: Item, constant: Item) Item,
    constant: Item,
};

fn sqFun(old: Item, constant: Item) Item {
    _ = constant;
    return old * old;
}

fn addFun(old: Item, constant: Item) Item {
    return old + constant;
}

fn mulFun(old: Item, constant: Item) Item {
    return old * constant;
}

const square = Operation {
    .op_fun = sqFun,
    .constant = 0,
};

pub fn add(comptime constant: Item) Operation {
    return Operation {
        .op_fun = addFun,
        .constant = constant,
    };
}

pub fn mul(comptime constant: Item) Operation {
    return Operation {
        .op_fun = mulFun,
        .constant = constant,
    };
}

const Monkey = struct {
    items: std.ArrayList(Item),
    operation: Operation,
    test_den: Item,
    true_monkey: usize,
    false_monkey: usize,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var example_monkeys = [_]Monkey{
        .{
            .items = try std.ArrayList(Item).init(allocator).appendSlice([_]Item{79, 98}),
        },
    };
    _ = example_monkeys;
}
