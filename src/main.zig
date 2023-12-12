//!
//! main.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 12/06/23
//! barg8397@vandals.uidaho.edu
//!
//! Terry sim - colony sim toy thingy
//!

// TODO(caleb):
// - (other colonies)

const std = @import("std");
const rl = @import("rl.zig");

const heap = std.heap;
const math = std.math;
const fmt = std.fmt;
const mem = std.mem;

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StaticBitSet = std.StaticBitSet;

const assert = std.debug.assert;

const board_rows = 20;
const board_cols = 20;
const tile_width_px = 160;
const tile_height_px = 160;
const glyph_size = 30;
const tick_rate_sec = 0.10;

const Worker = struct {
    const State = enum {
        pathing_to_target,
        choose_new_target,
        idle,
    };

    rock_count: usize,
    target_tile_p: rl.Vector2,
    tile_p: rl.Vector2,
    state: State,
};

const Storage = struct {
    rock_count: usize,
    tile_p: rl.Vector2,
};

const ResourceKind = enum(usize) {
    rock,
};

const EntityManager = struct {
    free_entities: ArrayList(usize),
    component_masks: []StaticBitSet(@intFromEnum(ComponentKind.count)),
    // living_entity_count: usize,

    pub fn init(
        ally: mem.Allocator,
        entity_count: usize,
    ) !EntityManager {
        var result = EntityManager{
            .free_entities = try ArrayList(usize).initCapacity(ally, entity_count), // TODO(caleb): SinglyLinkedList
            .component_masks = try ally.alloc(StaticBitSet(@intFromEnum(ComponentKind.count)), entity_count),
            // .living_entity_count = 0,
        };
        for (0..entity_count) |free_entity_index|
            result.free_entities.insertAssumeCapacity(free_entity_index, free_entity_index);
        for (result.component_masks) |*mask|
            mask.setRangeValue(
                .{
                    .start = 0,
                    .end = @intFromEnum(ComponentKind.count),
                },
                false,
            );
        return result;
    }

    pub fn newEntity(entity_man: *EntityManager) usize {
        assert(entity_man.free_entities.items.len > 0);
        const entity_id = entity_man.free_entities.pop();
        entity_man.component_masks[entity_id].setRangeValue(
            .{ .start = 0, .end = @intFromEnum(ComponentKind.count) },
            false,
        );
        return entity_id;
    }

    pub inline fn remove(entity_man: *EntityManager, entity_id: usize) void {
        entity_man.free_entities.appendAssumeCapacity(entity_id);
    }

    pub inline fn hasComponent(
        entity_man: *EntityManager,
        entity_id: usize,
        component_kind: ComponentKind,
    ) bool {
        return entity_man.component_masks[entity_id].isSet(@intFromEnum(component_kind));
    }
};

fn ComponentArray(comptime T: type) type {
    return struct {
        /// Component data
        data: []T,
        /// Component data count
        data_count: usize,

        /// Entity id => data index
        entity_to_data: AutoHashMap(usize, usize),

        /// Data index => entity id
        data_to_entity: AutoHashMap(usize, usize),

        pub fn init(
            ally: mem.Allocator,
            entity_count: usize,
        ) !@This() {
            return @This(){
                .data = try ally.alloc(T, entity_count),
                .data_count = 0,
                .entity_to_data = AutoHashMap(usize, usize).init(ally),
                .data_to_entity = AutoHashMap(usize, usize).init(ally),
            };
        }

        pub fn removeComponent(
            component_array: *@This(),
            entity_id: usize,
        ) void {
            const component_index = component_array.entity_to_data
                .get(entity_id) orelse unreachable;
            const last_slot_entity_id = component_array.data_to_entity
                .get(component_array.data_count - 1) orelse unreachable;

            // Overwrite the data
            component_array.data[component_index] =
                component_array.data[component_array.data_count - 1];
            component_array.entity_to_data.putAssumeCapacity(last_slot_entity_id, component_index);
            component_array.data_to_entity.putAssumeCapacity(component_index, last_slot_entity_id);
            component_array.data_count -= 1;
        }
    };
}

const ComponentKind = enum(u8) {
    tile_p = 0,
    resource_kind,

    count, // NOTHING BELOW THIS LINE
};

// const Tile = struct {
//     const Kind = enum {
//         grass,
//     };

//     entities: [10]usize = undefined,
//     entity_count: u8,
//     kind: Kind,
// };

const SWPEntry = struct {
    distance: usize,
    tile_p: rl.Vector2,
};

fn swpUpdateMap(
    scratch_arena: *heap.ArenaAllocator,
    sample_walk_map: []usize,
    target_p: rl.Vector2,
) !void {
    defer _ = scratch_arena.reset(.retain_capacity);
    const scratch_ally = scratch_arena.allocator();

    var swp_entry_list = try ArrayList(SWPEntry).initCapacity(scratch_ally, board_rows * board_cols);
    swp_entry_list.appendAssumeCapacity(.{ .distance = 0, .tile_p = target_p });

    for (sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

    while (swp_entry_list.items.len > 0) {
        const swp_entry = swp_entry_list.orderedRemove(0);
        sample_walk_map[@intFromFloat(swp_entry.tile_p.y * board_cols + swp_entry.tile_p.x)] = swp_entry.distance;

        // Add neighbors that haven't already been queued. i.e distance isn't max int...
        for (&[_]rl.Vector2{
            rl.Vector2{ .x = 0.0, .y = -1.0 },
            rl.Vector2{ .x = 0.0, .y = 1.0 },
            rl.Vector2{ .x = 1.0, .y = 0.0 },
            rl.Vector2{ .x = -1.0, .y = 0.0 },
        }) |d_tile_coords| {
            const neighbor_tile_p = rl.Vector2Add(swp_entry.tile_p, d_tile_coords);
            if (!(neighbor_tile_p.y >= board_rows or
                neighbor_tile_p.y < 0 or
                neighbor_tile_p.x >= board_cols or
                neighbor_tile_p.x < 0))
            {
                if (sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_cols + neighbor_tile_p.x)] == math.maxInt(usize)) {
                    swp_entry_list.appendAssumeCapacity(.{ .distance = swp_entry.distance + 1, .tile_p = neighbor_tile_p });
                    sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_cols + neighbor_tile_p.x)] = swp_entry.distance + 1;
                }
            }
        }
    }
}

inline fn isoProjMatrix() rl.Matrix {
    var result = mem.zeroes(rl.Matrix);
    result.m0 = 0.5 * tile_width_px / 2;
    result.m1 = 0.25 * tile_height_px / 2;
    result.m4 = -0.5 * tile_width_px / 2;
    result.m5 = 0.25 * tile_height_px / 2;
    result.m10 = 1;
    result.m15 = 1;
    return result;
}

const iso_proj_mat = isoProjMatrix();

/// [m0 m4] [x] m0*x + m4*y
/// [m1 m5] [y] m1*x + m5*y
inline fn matrixVector2Multiply(m: rl.Matrix, p: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = m.m0 * p.x + m.m4 * p.y,
        .y = m.m1 * p.x + m.m5 * p.y,
    };
}

fn isoProj(p: rl.Vector2) rl.Vector2 {
    return rl.Vector2Add(matrixVector2Multiply(iso_proj_mat, p), .{
        .x = @as(f32, @floatFromInt(rl.GetScreenWidth())) / 2.0,
        .y = (@as(f32, @floatFromInt(rl.GetScreenHeight())) - screenSpaceBoardHeight()) / 2.0,
    });
}

fn isoProjGlyph(p: rl.Vector2) rl.Vector2 {
    return rl.Vector2Add(isoProj(p), .{
        .x = -glyph_size / 2,
        .y = glyph_size / 4,
    });
}

fn screenSpaceBoardHeight() f32 {
    return matrixVector2Multiply(iso_proj_mat, .{ .x = board_cols, .y = board_rows }).y + tile_height_px / 2.0;
}

pub fn main() !void {
    rl.InitWindow(1600, 1200, "terry-cool");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();
    const rl_font = rl.GetFontDefault();

    ////////////////////////////////////
    var scratch_mem = heap.page_allocator.alloc(u8, 1024 * 10) catch unreachable;
    var scratch_fba = heap.FixedBufferAllocator.init(scratch_mem);
    var scratch_arena = heap.ArenaAllocator.init(scratch_fba.allocator());

    var entity_mem = heap.page_allocator.alloc(u8, 1024 * 20) catch unreachable;
    var entity_fba = heap.FixedBufferAllocator.init(entity_mem);
    var entity_arena = heap.ArenaAllocator.init(entity_fba.allocator());

    ////////////////////////////////////
    // ECS stuff
    const max_entity_count = 100;
    var entity_man = try EntityManager.init(
        entity_arena.allocator(),
        max_entity_count,
    );

    var tile_p_components =
        try ComponentArray(rl.Vector2).init(
        entity_arena.allocator(),
        max_entity_count,
    );

    var resource_kind_components =
        try ComponentArray(ResourceKind).init(
        entity_arena.allocator(),
        max_entity_count,
    );

    ////////////////////////////////////
    // NOTE(caleb): Dirty board *gen* code

    // NOTE(caleb): I want to have "LARGE" worlds
    // TODO(caleb): World chunks. Sim regions...
    // var board: [board_rows * board_cols]Tile = undefined;
    for (0..board_rows) |board_row_index| {
        for (0..board_cols) |board_col_index| {
            // Initialize tile
            // var tile = &board[board_row_index * board_cols + board_col_index];
            // tile.kind = .grass;
            // tile.entity_count = 0;

            // 1 in 10 to gen a rock
            if (rl.GetRandomValue(0, 10) == 0) {
                const rock_entity_index = entity_man.newEntity();

                // Component masks position and resource
                entity_man.component_masks[rock_entity_index]
                    .set(@intFromEnum(ComponentKind.tile_p));
                entity_man.component_masks[rock_entity_index]
                    .set(@intFromEnum(ComponentKind.resource_kind));

                // Tile p component
                tile_p_components.data[tile_p_components.data_count] =
                    rl.Vector2{
                    .x = @floatFromInt(board_col_index),
                    .y = @floatFromInt(board_row_index),
                };
                try tile_p_components.data_to_entity.put(
                    tile_p_components.data_count,
                    rock_entity_index,
                );
                try tile_p_components.entity_to_data.put(
                    rock_entity_index,
                    tile_p_components.data_count,
                );
                tile_p_components.data_count += 1;

                // Resource component
                resource_kind_components.data[resource_kind_components.data_count] = .rock;
                try resource_kind_components.data_to_entity.put(
                    resource_kind_components.data_count,
                    rock_entity_index,
                );
                try resource_kind_components.entity_to_data.put(
                    rock_entity_index,
                    resource_kind_components.data_count,
                );
                resource_kind_components.data_count += 1;

                // assert(tile.entity_count + 1 < tile.entities.len);
                // tile.entities[tile.entity_count] = rock_entity_index;
                // tile.entity_count += 1;
            }
        }
    }

    var sample_walk_map: [board_rows * board_cols]usize = undefined;
    for (&sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

    // NOTE(caleb): Obvious approach TODO(caleb): Replace with ecs

    var pile_list = try ArrayList(Storage).initCapacity(
        entity_arena.allocator(),
        100,
    );
    pile_list.appendAssumeCapacity(.{
        .rock_count = 0,
        .tile_p = .{
            .x = 11.0,
            .y = 10.0,
        },
    });

    pile_list.appendAssumeCapacity(.{
        .rock_count = 0,
        .tile_p = .{
            .x = 11.0,
            .y = 0.0,
        },
    });

    var worker = Worker{
        .target_tile_p = undefined,
        .state = .choose_new_target,
        .tile_p = rl.Vector2{
            .x = @floatFromInt(board_cols / 2),
            .y = @floatFromInt(board_rows / 2),
        },
        .rock_count = 0,
    };
    const worker_rock_carry_cap = 3;

    var selected_tile_tile_p = rl.Vector2{
        .x = 0.0,
        .y = 0.0,
    };

    const track1 = rl.LoadMusicStream("assets/music/track_1.wav");
    rl.PlayMusicStream(track1);

    var game_time_minute: usize = 0;
    var game_time_hour: usize = 7;
    var game_time_day: usize = 0;

    var debug_draw_distance_map = true;
    var debug_draw_grid_lines = false;

    var is_paused = false;
    var pause_start_time: f64 = 0.0;
    var last_tick_time: f64 = 0;

    while (!rl.WindowShouldClose()) {
        rl.UpdateMusicStream(track1);
        var key_pressed = rl.GetKeyPressed();
        while (key_pressed != 0) {
            if (key_pressed == rl.KEY_UP) {
                selected_tile_tile_p.y -= 1;
            } else if (key_pressed == rl.KEY_DOWN) {
                selected_tile_tile_p.y += 1;
            } else if (key_pressed == rl.KEY_LEFT) {
                selected_tile_tile_p.x -= 1;
            } else if (key_pressed == rl.KEY_RIGHT) {
                selected_tile_tile_p.x += 1;
            } else if (key_pressed == rl.KEY_F1) {
                debug_draw_distance_map = !debug_draw_distance_map;
            } else if (key_pressed == rl.KEY_F2) {
                debug_draw_grid_lines = !debug_draw_grid_lines;
            } else if (key_pressed == rl.KEY_SPACE) {
                is_paused = !is_paused;
                if (is_paused) { // NOTE(caleb): Record amount of time spent paused.
                    pause_start_time = rl.GetTime();
                } else last_tick_time += rl.GetTime() - pause_start_time;
            }
            key_pressed = rl.GetKeyPressed();
        }

        if (!is_paused and rl.GetTime() - last_tick_time >= tick_rate_sec) {
            switch (worker.state) {
                .choose_new_target => {
                    // NOTE(caleb): I think the worker should know what it is doing...
                    // For now just have specific behaviors for the current tile.
                    if (worker.rock_count > 0) {
                        for (pile_list.items) |*pile| {
                            if (rl.Vector2Equals(worker.tile_p, pile.tile_p) != 0) {
                                pile.rock_count += worker.rock_count;
                                worker.rock_count = 0;
                            }
                        }
                    }

                    var rocks_on_board = false;
                    for (0..resource_kind_components.data_count) |resource_kind_component_index| {
                        if (resource_kind_components.data[resource_kind_component_index] == .rock) {
                            rocks_on_board = true;
                            break;
                        }
                    }

                    if ((pile_list.items.len > 0 and worker.rock_count >= worker_rock_carry_cap) or
                        (pile_list.items.len > 0 and worker.rock_count > 0 and !rocks_on_board))
                    {
                        var closest_pile_d: f32 = math.floatMax(f32);
                        for (pile_list.items) |pile| {
                            if (rl.Vector2Distance(worker.tile_p, pile.tile_p) < closest_pile_d) {
                                closest_pile_d = rl.Vector2Distance(worker.tile_p, pile.tile_p);
                                worker.target_tile_p = pile.tile_p;
                            }
                        }
                        worker.state = .pathing_to_target;
                    } else if (rocks_on_board and
                        (worker.rock_count < worker_rock_carry_cap))
                    {
                        var closest_rock_d: f32 = math.floatMax(f32);
                        for (0..resource_kind_components.data_count) |rk_component_index| {
                            if (resource_kind_components.data[rk_component_index] == .rock) {
                                const entity_id = resource_kind_components.data_to_entity
                                    .get(rk_component_index) orelse unreachable;
                                const tile_p_index = tile_p_components.entity_to_data
                                    .get(entity_id) orelse unreachable;
                                const tile_p = tile_p_components.data[tile_p_index];

                                if (rl.Vector2Distance(worker.tile_p, tile_p) < closest_rock_d) {
                                    closest_rock_d = rl.Vector2Distance(worker.tile_p, tile_p);
                                    worker.target_tile_p = tile_p;
                                }
                            }
                        }
                        worker.state = .pathing_to_target;
                    } else worker.state = .idle;
                },
                .pathing_to_target => {
                    try swpUpdateMap(
                        &scratch_arena,
                        &sample_walk_map,
                        worker.target_tile_p,
                    );

                    // Have worker navigate to rock
                    var next_worker_tile_p: rl.Vector2 = undefined;
                    var closest_tile_distance: usize = math.maxInt(usize);
                    for (&[_]rl.Vector2{
                        rl.Vector2{ .x = 0.0, .y = -1.0 },
                        rl.Vector2{ .x = 0.0, .y = 1.0 },
                        rl.Vector2{ .x = 1.0, .y = 0.0 },
                        rl.Vector2{ .x = -1.0, .y = 0.0 },
                    }) |d_tile_coords| {
                        const neighbor_tile_p = rl.Vector2Add(worker.tile_p, d_tile_coords);
                        if (!(neighbor_tile_p.y >= board_rows or
                            neighbor_tile_p.y < 0 or
                            neighbor_tile_p.x >= board_cols or
                            neighbor_tile_p.x < 0))
                        {
                            if (sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_cols + neighbor_tile_p.x)] < closest_tile_distance) {
                                closest_tile_distance = sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_cols + neighbor_tile_p.x)];
                                next_worker_tile_p = neighbor_tile_p;
                            }
                        }
                    }
                    worker.tile_p = next_worker_tile_p;
                    if (rl.Vector2Equals(worker.tile_p, worker.target_tile_p) != 0) {
                        for (0..tile_p_components.data_count) |tp_component_index| {
                            if ((rl.Vector2Equals(
                                worker.tile_p,
                                tile_p_components.data[tp_component_index],
                            ) != 0)) {
                                // Remove rock entity
                                const entity_id = tile_p_components.data_to_entity
                                    .get(tp_component_index) orelse unreachable;

                                assert(entity_man.hasComponent(entity_id, .resource_kind));
                                resource_kind_components.removeComponent(entity_id);

                                assert(entity_man.hasComponent(entity_id, .tile_p));
                                tile_p_components.removeComponent(entity_id);

                                entity_man.remove(entity_id);

                                worker.rock_count += 1;

                                break;
                            }
                        }
                        worker.state = .choose_new_target;
                    }
                },
                .idle => {},
            }

            game_time_minute += 1;
            if (game_time_minute >= 60) {
                game_time_minute = 0;
                game_time_hour += 1;
                if (game_time_hour >= 24) {
                    game_time_hour = 0;
                    game_time_day += 1;
                }
            }

            // How should sleep be handled??

            if (game_time_hour < 7 or game_time_hour >= 23) {
                worker.state = .idle;
            } else if (worker.state == .idle) { // NOTE(caleb): worker should go back to what it was doing.
                worker.state = .choose_new_target;
            }

            last_tick_time = rl.GetTime();
        }

        rl.BeginDrawing();
        rl.ClearBackground(.{ .r = 0, .g = 0, .b = 0, .a = 255 });

        if (debug_draw_grid_lines) {
            // NOTE(caleb): + 1 for outer grid lines
            for (0..board_rows + 1) |grid_row| {
                const start_p = isoProj(.{
                    .x = 0,
                    .y = @as(f32, @floatFromInt(grid_row)),
                });
                const end_p = isoProj(.{
                    .x = board_cols,
                    .y = @as(f32, @floatFromInt(grid_row)),
                });
                rl.DrawLineEx(start_p, end_p, 1, rl.RED);
            }
            for (0..board_cols + 1) |grid_col| {
                const start_p = isoProj(.{
                    .x = @floatFromInt(grid_col),
                    .y = 0.0,
                });
                const end_p = isoProj(.{
                    .x = @as(f32, @floatFromInt(grid_col)),
                    .y = @as(f32, @floatFromInt(board_rows)),
                });
                rl.DrawLineEx(start_p, end_p, 1, rl.RED);
            }
        }

        // Draw distance map
        if (debug_draw_distance_map) {
            for (0..board_rows) |grid_row_index| {
                for (0..board_cols) |grid_col_index| {
                    const tile_distance = if (sample_walk_map[grid_row_index * board_cols + grid_col_index] > 9)
                        '.'
                    else
                        sample_walk_map[grid_row_index * board_cols + grid_col_index];

                    const is_selected_tile = rl.Vector2Equals(.{
                        .x = @floatFromInt(grid_col_index),
                        .y = @floatFromInt(grid_row_index),
                    }, selected_tile_tile_p) != 0;

                    const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                    var projected_p = isoProjGlyph(.{
                        .x = @as(f32, @floatFromInt(grid_col_index)),
                        .y = @as(f32, @floatFromInt(grid_row_index)),
                    });
                    projected_p.y += y_offset;
                    rl.DrawTextCodepoint(
                        rl_font,
                        @intCast(tile_distance + '0'),
                        projected_p,
                        glyph_size,
                        if (is_selected_tile) .{ .r = 255, .g = 255, .b = 255, .a = 200 } else .{ .r = 200, .g = 200, .b = 200, .a = 100 },
                    );
                }
            }
        }

        // Draw rocks
        for (0..resource_kind_components.data_count) |rk_component_index| {
            if (resource_kind_components.data[rk_component_index] == .rock) {
                const entity_id = resource_kind_components.data_to_entity
                    .get(rk_component_index) orelse unreachable;
                const tile_p_index = tile_p_components.entity_to_data
                    .get(entity_id) orelse unreachable;

                const rock_tile_p = tile_p_components.data[tile_p_index];
                const is_selected_tile = rl.Vector2Equals(rock_tile_p, selected_tile_tile_p) != 0;
                const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                var projected_p = isoProjGlyph(.{
                    .x = rock_tile_p.x,
                    .y = rock_tile_p.y,
                });
                projected_p.y += y_offset;
                rl.DrawTextCodepoint(
                    rl_font,
                    @intCast('R'),
                    projected_p,
                    glyph_size,
                    if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                );
            }
        }
        // for (rock_tile_p_list.items) |rock_tile_p| {
        // }

        // for (0..board_rows) |board_row_index| {
        //     for (0..board_cols) |board_col_index| {
        //         // const tile = &board[board_row_index * board_cols + board_col_index];
        //         if (tile.entity_count > 0) {
        //             // TODO(caleb): Which entites have higher draw presidence.
        //             switch (tile.entities[0].kind) {
        //                 .resource => {
        //                     const is_selected_tile = rl.Vector2Equals(rock_tile_p, selected_tile_tile_p) != 0;
        //                     const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
        //                     var projected_p = isoProjGlyph(.{
        //                         .x = rock_tile_p.x,
        //                         .y = rock_tile_p.y,
        //                     });
        //                     projected_p.y += y_offset;
        //                     rl.DrawTextCodepoint(
        //                         rl_font,
        //                         @intCast('R'),
        //                         projected_p,
        //                         glyph_size,
        //                         if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
        //                     );
        //                 },
        //             }
        //         }
        //     }
        // }

        // Draw piles
        for (pile_list.items) |pile| {
            const is_selected_tile = rl.Vector2Equals(pile.tile_p, selected_tile_tile_p) != 0;
            const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
            var projected_p = isoProjGlyph(.{
                .x = pile.tile_p.x,
                .y = pile.tile_p.y,
            });
            projected_p.y += y_offset;
            rl.DrawTextCodepoint(
                rl_font,
                @intCast('P'),
                projected_p,
                glyph_size,
                if (is_selected_tile) rl.YELLOW else .{ .r = 200, .g = 200, .b = 0, .a = 255 },
            );
        }

        // Draw worker
        {
            const y_offset: f32 = if (rl.Vector2Equals(worker.tile_p, selected_tile_tile_p) != 0) -10.0 else 0.0;
            var projected_p = isoProjGlyph(.{
                .x = worker.tile_p.x,
                .y = worker.tile_p.y,
            });
            projected_p.y += y_offset;
            rl.DrawTextCodepoint(rl_font, @intCast('W'), projected_p, glyph_size, rl.ORANGE);
        }

        // Pause text
        if (is_paused) {
            const paused_width = rl.MeasureText("PAUSED", glyph_size * 2);
            rl.DrawTextEx(rl_font, "PAUSED", .{
                .x = @floatFromInt(@divFloor(rl.GetScreenWidth(), 2) - @divFloor(paused_width, 2)),
                .y = @floatFromInt(rl.GetScreenHeight() - @divFloor(rl.GetScreenHeight(), 5)),
            }, glyph_size * 2, 1.0, rl.WHITE);
        }

        // Debug info
        {
            defer _ = scratch_arena.reset(.retain_capacity);
            const scratch_ally = scratch_arena.allocator();

            const game_timez = try fmt.allocPrintZ(
                scratch_ally,
                "Time: {d}::{d}::{d}",
                .{ game_time_day, game_time_hour, game_time_minute },
            );
            const worker_namez = try fmt.allocPrintZ(
                scratch_ally,
                "Worker: Leroy",
                .{},
            );
            const worker_statez = try fmt.allocPrintZ(
                scratch_ally,
                "State: {s}",
                .{@tagName(worker.state)},
            );
            const worker_rock_countz = try fmt.allocPrintZ(
                scratch_ally,
                "Rock count: {d}",
                .{worker.rock_count},
            );
            const entity_countz = try fmt.allocPrintZ(
                scratch_ally,
                "Entity count: {d}",
                .{entity_man.free_entities.capacity - entity_man.free_entities.items.len},
            );
            for (&[_][:0]const u8{
                game_timez,
                worker_namez,
                worker_statez,
                worker_rock_countz,
                entity_countz,
            }, 0..) |strz, strz_index| {
                rl.DrawTextEx(rl_font, strz, .{
                    .x = 0, //@as(f32, @floatFromInt(board_cols)) * tile_width_px,
                    .y = @as(f32, @floatFromInt(strz_index)) * 40,
                }, glyph_size, 1.0, rl.WHITE);
            }
        }

        rl.EndDrawing();
    }

    rl.CloseWindow();
}
