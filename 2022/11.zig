const std = @import("std");

const Item = u64;

const Operation = struct {
    op_fun: *const fn(old: Item, constant: Item) Item,
    constant: Item,

    fn apply(self: *Operation, it: Item) Item {
        return self.op_fun(it, self.constant);
    }
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
    items: Queue(Item),
    operation: Operation,
    test_den: Item,
    true_monkey: usize,
    false_monkey: usize,
    inspections: usize,

    fn init(item_slice: []const Item, op: Operation, den: Item, true_monkey: usize, false_monkey: usize, allocator: std.mem.Allocator) !Monkey {
        var items = Queue(Item).init(allocator);
        for (item_slice) |it| {
            try items.enqueue(it);
        }
        return .{
            .items = items,
            .operation = op,
            .test_den = den,
            .true_monkey = true_monkey,
            .false_monkey = false_monkey,
            .inspections = 0,
        };
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // var monkeys1 = [_]Monkey{
    //     try Monkey.init(&[_]Item{79, 98}, mul(19), 23, 2, 3, allocator),
    //     try Monkey.init(&[_]Item{54, 65, 75, 74}, add(6), 19, 2, 0, allocator),
    //     try Monkey.init(&[_]Item{79, 60, 97}, square, 13, 1, 3, allocator),
    //     try Monkey.init(&[_]Item{74}, add(3), 17, 0, 1, allocator),
    // };
    var monkeys1 = [_]Monkey{
        try Monkey.init(&[_]Item{54, 82, 90, 88, 86, 54}, mul(7), 11, 2, 6, allocator),
        try Monkey.init(&[_]Item{91, 65}, mul(13), 5, 7, 4, allocator),
        try Monkey.init(&[_]Item{62, 54, 57, 92, 83, 63, 63}, add(1), 7, 1, 7, allocator),
        try Monkey.init(&[_]Item{67, 72, 68}, square, 2, 0, 6, allocator),
        try Monkey.init(&[_]Item{68, 89, 90, 86, 84, 57, 72, 84}, add(7), 17, 3, 5, allocator),
        try Monkey.init(&[_]Item{79, 83, 64, 58}, add(6), 13, 3, 0, allocator),
        try Monkey.init(&[_]Item{96, 72, 89, 70, 88}, add(4), 3, 1, 2, allocator),
        try Monkey.init(&[_]Item{79}, add(8), 19, 4, 5, allocator),
    };
    var monkeys2: [monkeys1.len]Monkey = undefined;
    std.mem.copy(Monkey, &monkeys2, &monkeys1);

    var lcd: Item = 1;
    for (monkeys1) |m| {
        lcd *= m.test_den;
    }

    std.debug.print("lcd = {}\n", .{lcd});

    var round: usize = 0;
    while (round < 20) : (round += 1) {
        for (monkeys1) |*m| {
            while (m.items.dequeue()) |it| {
                m.inspections += 1;
                const worry = (m.operation.apply(it) / 3) % lcd;
                const receiver = if (worry % m.test_den == 0) m.true_monkey else m.false_monkey;
                try monkeys1[receiver].items.enqueue(worry);
                //std.debug.print("{} threw {} to {}\n", .{i, worry, receiver});
            }
        }

        // std.debug.print("Round {}\n", .{round+1});
        // for (monkeys1) |*m, i| {
        //     std.debug.print("Monkey {}: ", .{i});
        //     m.items.print();
        // }
        // std.debug.print("\n", .{});
    }
    std.debug.print("Part 1: {}\n", .{monkey_business(&monkeys1)});

    round = 0;
    while (round < 10000) : (round += 1) {
        for (monkeys2) |*m| {
            while (m.items.dequeue()) |it| {
                m.inspections += 1;
                const worry = (m.operation.apply(it)) % lcd;
                const receiver = if (worry % m.test_den == 0) m.true_monkey else m.false_monkey;
                try monkeys2[receiver].items.enqueue(worry);
                //std.debug.print("{} threw {} to {}\n", .{i, worry, receiver});
            }
        }
    }
    std.debug.print("Part 2: {}\n", .{monkey_business(&monkeys2)});
}

fn monkey_business(monkeys: []const Monkey) usize {
    var max = [_]usize{0, 0};
    for (monkeys) |m| {
        //std.debug.print("{}\n", .{m.inspections});
        if (m.inspections > max[1]) {
            max[0] = max[1];
            max[1] = m.inspections;
        } else if (m.inspections > max[0]) {
            max[0] = m.inspections;
        }
    }
    return max[0] * max[1];
}

pub fn Queue(comptime Child: type) type {
    return struct {
        const This = @This();
        const Node = struct {
            data: Child,
            next: ?*Node,
        };
        gpa: std.mem.Allocator,
        start: ?*Node,
        end: ?*Node,

        pub fn init(gpa: std.mem.Allocator) This {
            return This{
                .gpa = gpa,
                .start = null,
                .end = null,
            };
        }
        pub fn enqueue(this: *This, value: Child) !void {
            const node = try this.gpa.create(Node);
            node.* = .{ .data = value, .next = null };
            if (this.end) |end| end.next = node //
            else this.start = node;
            this.end = node;
        }
        pub fn dequeue(this: *This) ?Child {
            const start = this.start orelse return null;
            defer this.gpa.destroy(start);
            if (start.next) |next|
                this.start = next
            else {
                this.start = null;
                this.end = null;
            }
            return start.data;
        }
        pub fn print(this: *This) void {
            var node = this.start;
            while (node) |n| : (node = n.next) {
                std.debug.print("{}, ", .{n.data});
            }
            std.debug.print("\n", .{});
        }
    };
}
