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

pub inline fn sumOfInventory(ica: *ecs.ComponentArray(Inventory), ica_index: usize) usize {
    var result: usize = 0;
    result += ica.data[ica_index].rock_count;
    result += ica.data[ica_index].stick_count;
    result += ica.data[ica_index].berry_count;
    return result;
}

pub inline fn updateGameTime(
    tick_granularity: TickGranularity,
    game_minutes: *u8,
    game_hours: *u8,
    game_days: *u16,
    game_years: *usize,
) void {
    switch (tick_granularity) {
        .minute => game_minutes.* += 1,
        .hour => game_hours.* += 1,
        .day => game_days.* += 1,
        .year => game_years.* += 1,
        else => unreachable,
    }
    if (game_minutes.* >= 60) {
        game_minutes.* = 0;
        game_hours.* += 1;
    }
    if (game_hours.* >= 24) {
        game_hours.* = 0;
        game_days.* += 1;
    }
    if (game_days.* >= 365) {
        game_days.* = 0;
        game_years.* += 1;
    }
}

pub fn updateWorkers() void {
    // for (0..worker_state_components.data_count) |wsc_index| {
    //     const worker_state = worker_state_components.data[wsc_index];
    //     const worker_entity_id = worker_state_components.data_to_entity
    //         .get(wsc_index) orelse unreachable;
    //     const worker_tile_p_index = tile_p_components.entity_to_data
    //         .get(worker_entity_id) orelse unreachable;
    //     const worker_target_tile_p_index = target_tile_p_components.entity_to_data
    //         .get(worker_entity_id) orelse unreachable;
    //     const worker_inventory_index = inventory_components.entity_to_data
    //         .get(worker_entity_id) orelse unreachable;
    //     switch (worker_state) {
    //         .choose_new_target => {
    //             if (sp_sim.sumOfInventory(inventory_components, worker_inventory_index) > 0) {
    //                 for (0..inventory_components.data_count) |ic_index| {
    //                     const entity_id = inventory_components.data_to_entity
    //                         .get(ic_index) orelse unreachable;
    //                     if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) { // Filter for piles
    //                         const storage_tile_p_index = tile_p_components.entity_to_data
    //                             .get(entity_id) orelse unreachable;
    //                         if (@reduce(.And, tile_p_components.data[worker_tile_p_index] ==
    //                             tile_p_components.data[storage_tile_p_index]))
    //                         {
    //                             inventory_components.data[ic_index].rock_count +=
    //                                 inventory_components.data[worker_inventory_index].rock_count;
    //                             inventory_components.data[ic_index].stick_count +=
    //                                 inventory_components.data[worker_inventory_index].stick_count;
    //                             inventory_components.data[ic_index].berry_count +=
    //                                 inventory_components.data[worker_inventory_index].berry_count;
    //                             inventory_components.data[worker_inventory_index] = .{};
    //                         }
    //                     }
    //                 }
    //             }

    //             var resources_on_board = if (resource_kind_components.data_count > 0) true else false;
    //             var piles_on_board = false;
    //             for (0..inventory_components.data_count) |ic_index| {
    //                 const entity_id = inventory_components.data_to_entity
    //                     .get(ic_index) orelse unreachable;
    //                 if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) {
    //                     piles_on_board = true;
    //                     break;
    //                 }
    //             }

    //             if ((piles_on_board and sp_sim.sumOfInventory(inventory_components, worker_inventory_index) >= sp_sim.worker_carry_cap) or
    //                 (piles_on_board and sp_sim.sumOfInventory(inventory_components, worker_inventory_index) > 0 and !resources_on_board))
    //             {
    //                 var closest_pile_d: f32 = math.floatMax(f32);
    //                 for (0..inventory_components.data_count) |ic_index| {
    //                     const entity_id = inventory_components.data_to_entity
    //                         .get(ic_index) orelse unreachable;
    //                     if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) { // Filter for piles
    //                         const storage_tile_p_index =
    //                             tile_p_components.entity_to_data.get(entity_id) orelse unreachable;
    //                         const storage_tile_p = tile_p_components.data[storage_tile_p_index];
    //                         if (base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p) < closest_pile_d) {
    //                             closest_pile_d = base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p);
    //                             target_tile_p_components.data[worker_target_tile_p_index] = storage_tile_p;
    //                         }
    //                     }
    //                 }
    //                 worker_state_components.data[wsc_index] = .pathing_to_target;
    //             } else if (resources_on_board and
    //                 (sp_sim.sumOfInventory(inventory_components, worker_inventory_index) < sp_sim.worker_carry_cap))
    //             {
    //                 var closest_resource_d: f32 = math.floatMax(f32);
    //                 for (0..resource_kind_components.data_count) |rk_component_index| {
    //                     const entity_id = resource_kind_components.data_to_entity
    //                         .get(rk_component_index) orelse unreachable;
    //                     const tile_p_index = tile_p_components.entity_to_data
    //                         .get(entity_id) orelse unreachable;
    //                     const tile_p = tile_p_components.data[tile_p_index];

    //                     if (base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], tile_p) < closest_resource_d) {
    //                         closest_resource_d = base_math.vector2DistanceU16(
    //                             tile_p_components.data[worker_tile_p_index],
    //                             tile_p,
    //                         );
    //                         target_tile_p_components.data[worker_target_tile_p_index] = tile_p;
    //                     }
    //                 }
    //                 worker_state_components.data[wsc_index] = .pathing_to_target;
    //             } else worker_state_components.data[wsc_index] = .idle;
    //         },
    //         .pathing_to_target => {
    //             sp_map.swpUpdateMap(
    //                 &game_state.scratch_fba,
    //                 game_state.sample_walk_map,
    //                 target_tile_p_components.data[worker_target_tile_p_index][1],
    //             ) catch unreachable;

    //             // Have worker navigate to rock
    //             var next_worker_tile_p: @Vector(2, u16) = undefined;
    //             var closest_tile_distance: usize = math.maxInt(usize);

    //             const region_tile_index = tile_p_components.data[worker_tile_p_index][1];
    //             const region_tile_row = @divTrunc(region_tile_index, board_dim);
    //             const region_tile_col = region_tile_index % board_dim;
    //             for (&[_]@Vector(2, i8){
    //                 @Vector(2, i8){ 0, -1 },
    //                 @Vector(2, i8){ 0, 1 },
    //                 @Vector(2, i8){ 1, 0 },
    //                 @Vector(2, i8){ -1, 0 },
    //             }) |d_tile_coords| {
    //                 const neighbor_tile_p = @Vector(2, i8){
    //                     @intCast(region_tile_col),
    //                     @intCast(region_tile_row),
    //                 } + d_tile_coords;
    //                 if (!(neighbor_tile_p[1] >= board_dim or
    //                     neighbor_tile_p[1] < 0 or
    //                     neighbor_tile_p[0] >= board_dim or
    //                     neighbor_tile_p[0] < 0))
    //                 {
    //                     if (game_state.sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])] < closest_tile_distance) {
    //                         closest_tile_distance = game_state.sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])];
    //                         next_worker_tile_p = @Vector(2, u16){ 0, @intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0]) }; // FIXME(caleb): WORLD INDEX
    //                     }
    //                 }
    //             }
    //             tile_p_components.data[worker_tile_p_index] = next_worker_tile_p;
    //             if (@reduce(.And, tile_p_components.data[worker_tile_p_index] == target_tile_p_components.data[worker_target_tile_p_index])) {
    //                 for (0..tile_p_components.data_count) |tp_component_index| {
    //                     if (@reduce(.And, tile_p_components.data[worker_tile_p_index] == tile_p_components.data[tp_component_index])) {
    //                         const entity_id = tile_p_components.data_to_entity
    //                             .get(tp_component_index) orelse unreachable;
    //                         if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.resource))) {
    //                             const rkc_index = resource_kind_components.entity_to_data
    //                                 .get(entity_id) orelse unreachable;
    //                             switch (resource_kind_components.data[rkc_index]) {
    //                                 .rock => inventory_components.data[worker_inventory_index].rock_count += 1,
    //                                 .stick => inventory_components.data[worker_inventory_index].stick_count += 1,
    //                                 .berry => inventory_components.data[worker_inventory_index].berry_count += 1,
    //                                 .tree => unreachable, // TODO(caleb): Handle me
    //                             }
    //                             resource_kind_components.remove(entity_id);
    //                             tile_p_components.remove(entity_id);
    //                             entity_man.release(entity_id);
    //                             break;
    //                         }
    //                     }
    //                 }
    //                 worker_state_components.data[wsc_index] = .choose_new_target;
    //             }
    //         },
    //         .idle => {},
    //     }
    // }
}
