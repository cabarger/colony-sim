//!
//! sp_sim.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/25/24
//! barg8397@vandals.uidaho.edu
//!
//! Simulation code
//!

const ecs = @import("ecs.zig");

pub const tick_rate_sec = 0.25;
pub const worker_carry_cap = 3;

pub const ResourceKind = enum(u8) {
    rock,
    stick,
    tree,
    berry,
};

pub const Inventory = packed struct {
    rock_count: u16 = 0,
    stick_count: u16 = 0,
    berry_count: u16 = 0,
};

pub inline fn sumOfInventory(ica: *ecs.ComponentArray(Inventory), ica_index: usize) usize {
    var result: usize = 0;
    result += ica.data[ica_index].rock_count;
    result += ica.data[ica_index].stick_count;
    result += ica.data[ica_index].berry_count;
    return result;
}

pub const TickGranularity = enum(u8) {
    minute = 0,
    hour,
    day,
    year,

    count,
};

pub const WorkerState = enum(u8) {
    pathing_to_target,
    choose_new_target,
    idle,
};
