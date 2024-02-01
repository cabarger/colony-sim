//!
//! sp_game_code.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/05/24
//! barg8397@vandals.uidaho.edu
//!

//- cabarger: TODO
// Tree growth [ ]
// Replace raylib Vector2 with builtin @Vector [ ]
// Pass Inputs/Time as another field to updateAndRender function [ ]
// "Domeular" map gen [ ]
// Dynamic textures at world view [ ]

const std = @import("std");
const base = @import("base");
const third_party = @import("third_party");

const sp_platform = @import("sp_platform.zig");
const sp_map = @import("sp_map.zig");
const sp_sim = @import("sp_sim.zig");
const sp_render = @import("sp_render.zig");
const ecs = @import("ecs.zig");

const rl = third_party.rl;
const math = std.math;
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const rand = std.rand;
const base_math = base.base_math;
const base_thread_context = base.base_thread_context;

const Tileset = @import("Tileset.zig");
const Matrix2x2 = base_math.Matrix2x2;
const World = sp_map.World;
const RegionData = sp_map.RegionData;

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StaticBitSet = std.StaticBitSet;
const Random = rand.Random;

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const assert = std.debug.assert;
const board_dim = sp_map.board_dim;

////////////////////////////////
//- cabarger: Game code globals

var entity_man: *ecs.EntityManager = undefined;
var tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
var target_tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
var resource_kind_components: *ecs.ComponentArray(sp_sim.ResourceKind) = undefined;
var inventory_components: *ecs.ComponentArray(sp_sim.Inventory) = undefined;
var worker_state_components: *ecs.ComponentArray(sp_sim.WorkerState) = undefined;
var tileset: *Tileset = undefined;
var world: *World = undefined;

export fn spUpdateAndRender(
    platform_api: *const sp_platform.PlatformAPI,
    game_state: *sp_platform.GameState,
    game_input: *sp_platform.GameInput,
    tctx: *base_thread_context.TCTX,
) void {
    _ = tctx;

    //- cabarger: This is done exactly ONCE on initial game code load.
    if (!game_state.did_init)
        gameStateInit(game_state, platform_api);

    if (game_state.did_reload) {
        fixStuffThatBrokeBecauseOfReload(game_state);
        //- cabarger: Condition for taking this code path. We've handled
        // the reload.
        game_state.did_reload = false;
    }

    var tick_granularity: sp_sim.TickGranularity = @enumFromInt(game_state.tick_granularity);
    var view_mode: sp_map.ViewMode = @enumFromInt(game_state.view_mode);
    var draw_rot_state: sp_render.DrawRotState = @enumFromInt(game_state.draw_rot_state);

    ////////////////////////////////
    //- cabarger: Update

    const mouse_wheel_move = game_input.mouse_input.wheel_move;
    if (mouse_wheel_move != 0) {
        game_state.scale_factor += if (mouse_wheel_move == -1) -sp_render.scale_inc else sp_render.scale_inc;
        game_state.scale_factor = base_math.clampF32(game_state.scale_factor, 0.25, 10.0);
    }

    const scaled_tile_dim: rl.Vector2 = .{
        .x = @as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor,
        .y = @as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor,
    };

    const mouse_moved = @reduce(.And, game_input.last_mouse_input.p != game_input.mouse_input.p);

    if (mouse_moved) {
        //- cabarger: FIXME Iterate over tiles from the bottom right to top left, this way
        // if two tiles overlap the selected tile is allways the one in front.
        if (game_state.draw_3d) {
            game_state.selected_tile_p = .{ -1, -1 };
            outer: for (0..board_dim) |row_index| {
                for (0..board_dim) |col_index| {
                    const canonical_board_p: @Vector(2, usize) = @intCast(
                        sp_map.canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), @enumFromInt(game_state.draw_rot_state)),
                    );
                    const height = switch (@as(sp_map.ViewMode, @enumFromInt(game_state.view_mode))) {
                        .region => world.region_data[game_state.selected_region_p[1] * board_dim + game_state.selected_region_p[0]]
                            .height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                        .world => world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                    };
                    var projected_p = sp_render.isoProj(
                        platform_api,
                        .{
                            .x = @as(f32, @floatFromInt(col_index)) * scaled_tile_dim.x,
                            .y = @as(f32, @floatFromInt(row_index)) * scaled_tile_dim.y,
                        },
                        @intFromFloat(scaled_tile_dim.x),
                        @intFromFloat(scaled_tile_dim.y),
                        game_state.board_translation,
                    );
                    projected_p.y -= @as(f32, @floatFromInt(height)) * (scaled_tile_dim.y / 2.0);
                    if (platform_api.checkCollisionPointRec(@bitCast(game_input.mouse_input.p), .{
                        .x = projected_p.x,
                        .y = projected_p.y,
                        .width = scaled_tile_dim.x,
                        .height = scaled_tile_dim.y / 2.0,
                    })) {
                        game_state.selected_tile_p = @Vector(2, i8){ @intCast(col_index), @intCast(row_index) };
                        break :outer;
                    }
                }
            }
        } else {
            const deprojected_mouse_p = sp_render.isoInvert(
                platform_api,
                game_input.mouse_input.p - @Vector(2, f32){ scaled_tile_dim.x / 2.0, 0.0 },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            game_state.selected_tile_p[0] = @intFromFloat(deprojected_mouse_p.x / scaled_tile_dim.x);
            game_state.selected_tile_p[1] = @intFromFloat(deprojected_mouse_p.y / scaled_tile_dim.y);
        }
    }

    if (game_input.mouse_input.right_click)
        game_state.board_translation += game_input.mouse_input.p - game_input.last_mouse_input.p;

    if (game_input.key_input.isKeyPressed(.r) and game_input.key_input.isKeyPressed(.left_shift)) {
        draw_rot_state =
            @enumFromInt(@mod(@as(i8, @intCast(game_state.draw_rot_state)) - 1, @intFromEnum(sp_render.DrawRotState.count)));
        game_state.draw_rot_state = @intFromEnum(draw_rot_state);
    } else if (game_input.key_input.isKeyPressed(.r)) {
        draw_rot_state =
            @enumFromInt((game_state.draw_rot_state + 1) % @intFromEnum(sp_render.DrawRotState.count));
        game_state.draw_rot_state = @intFromEnum(draw_rot_state);
    } else if (game_input.key_input.isKeyPressed(.h)) {
        game_state.draw_3d = !game_state.draw_3d;
    } else if (game_input.key_input.isKeyPressed(.kp_4) or game_input.key_input.isKeyPressed(.kp_6)) {
        if (game_input.key_input.isKeyPressed(.kp_6)) {
            game_state.seed += 1;
        } else if (game_input.key_input.isKeyPressed(.kp_4) and game_state.seed > 0)
            game_state.seed -= 1;
        std.debug.print("{d}\n", .{game_state.seed});
        game_state.xoshiro_256 = rand.DefaultPrng.init(game_state.seed);

        while (entity_man.free_entities.capacity != entity_man.free_entities.items.len)
            entity_man.free_entities.insertAssumeCapacity(entity_man.free_entities.items.len, entity_man.free_entities.items.len);

        tile_p_components.reset();
        resource_kind_components.reset();

        sp_map.genWorld(
            game_state.xoshiro_256.random(),
            tileset,
            world,
            entity_man,
            tile_p_components,
            resource_kind_components,
        ) catch unreachable;
    } else if (game_input.key_input.isKeyPressed(.up)) {
        tick_granularity =
            @enumFromInt((game_state.tick_granularity + 1) % @intFromEnum(sp_sim.TickGranularity.count));
        game_state.tick_granularity = @intFromEnum(tick_granularity);
        game_state.selected_tile_p[1] -= 1;
    } else if (game_input.key_input.isKeyPressed(.down)) {
        game_state.selected_tile_p[1] += 1;
    } else if (game_input.key_input.isKeyPressed(.left)) {
        game_state.selected_tile_p[0] -= 1;
    } else if (game_input.key_input.isKeyPressed(.right)) {
        game_state.selected_tile_p[0] += 1;
    } else if (game_input.key_input.isKeyPressed(.f1)) {
        game_state.debug_draw_distance_map = !game_state.debug_draw_distance_map;
    } else if (game_input.key_input.isKeyPressed(.f2)) {
        game_state.debug_draw_grid_lines = !game_state.debug_draw_grid_lines;
    } else if (game_input.key_input.isKeyPressed(.f3)) {
        game_state.debug_draw_tile_height = !game_state.debug_draw_tile_height;
    } else if (game_input.key_input.isKeyPressed(.f4)) {
        game_state.debug_draw_tile_hitboxes = !game_state.debug_draw_tile_hitboxes;
    } else if (game_input.key_input.isKeyPressed(.e)) {
        view_mode = .world;
        game_state.view_mode = @intFromEnum(view_mode);
    } else if (game_input.key_input.isKeyPressed(.enter) and view_mode == .world) {
        const zero_vec: @Vector(2, i8) = @splat(0);
        const dim_vec: @Vector(2, i8) = @splat(board_dim);
        if (@reduce(.And, game_state.selected_tile_p >= zero_vec) and @reduce(.And, game_state.selected_tile_p < dim_vec)) {
            view_mode = .region;
            game_state.view_mode = @intFromEnum(view_mode);
            game_state.selected_region_p = @intCast(
                sp_map.canonicalTileP(game_state.selected_tile_p, @enumFromInt(game_state.draw_rot_state)),
            );
        }
    } else if (game_input.key_input.isKeyPressed(.space)) {
        game_state.is_paused = !game_state.is_paused;
        if (game_state.is_paused) { //- cabarger: Record amount of time spent paused.
            game_state.pause_start_time = platform_api.getTime();
        } else game_state.last_tick_time += platform_api.getTime() - game_state.pause_start_time;
    }

    const time_now = platform_api.getTime();
    if (!game_state.is_paused and time_now - game_state.last_tick_time >= sp_sim.tick_rate_sec) {
        game_state.last_tick_time = time_now;

        //- cabarger: Game tick updates updates happen here. The initial thinking was something like
        //            do this tick n times depending on time granularity i.e once for 1 minutes 60 times
        //            for hours etc.
        for (0..worker_state_components.data_count) |wsc_index| {
            const worker_state = worker_state_components.data[wsc_index];
            const worker_entity_id = worker_state_components.data_to_entity
                .get(wsc_index) orelse unreachable;
            const worker_tile_p_index = tile_p_components.entity_to_data
                .get(worker_entity_id) orelse unreachable;
            const worker_target_tile_p_index = target_tile_p_components.entity_to_data
                .get(worker_entity_id) orelse unreachable;
            const worker_inventory_index = inventory_components.entity_to_data
                .get(worker_entity_id) orelse unreachable;
            switch (worker_state) {
                .choose_new_target => {
                    if (sp_sim.sumOfInventory(inventory_components, worker_inventory_index) > 0) {
                        for (0..inventory_components.data_count) |ic_index| {
                            const entity_id = inventory_components.data_to_entity
                                .get(ic_index) orelse unreachable;
                            if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) { // Filter for piles
                                const storage_tile_p_index = tile_p_components.entity_to_data
                                    .get(entity_id) orelse unreachable;
                                if (@reduce(.And, tile_p_components.data[worker_tile_p_index] ==
                                    tile_p_components.data[storage_tile_p_index]))
                                {
                                    inventory_components.data[ic_index].rock_count +=
                                        inventory_components.data[worker_inventory_index].rock_count;
                                    inventory_components.data[ic_index].stick_count +=
                                        inventory_components.data[worker_inventory_index].stick_count;
                                    inventory_components.data[ic_index].berry_count +=
                                        inventory_components.data[worker_inventory_index].berry_count;
                                    inventory_components.data[worker_inventory_index] = .{};
                                }
                            }
                        }
                    }

                    var resources_on_board = if (resource_kind_components.data_count > 0) true else false;
                    var piles_on_board = false;
                    for (0..inventory_components.data_count) |ic_index| {
                        const entity_id = inventory_components.data_to_entity
                            .get(ic_index) orelse unreachable;
                        if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) {
                            piles_on_board = true;
                            break;
                        }
                    }

                    if ((piles_on_board and sp_sim.sumOfInventory(inventory_components, worker_inventory_index) >= sp_sim.worker_carry_cap) or
                        (piles_on_board and sp_sim.sumOfInventory(inventory_components, worker_inventory_index) > 0 and !resources_on_board))
                    {
                        var closest_pile_d: f32 = math.floatMax(f32);
                        for (0..inventory_components.data_count) |ic_index| {
                            const entity_id = inventory_components.data_to_entity
                                .get(ic_index) orelse unreachable;
                            if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) { // Filter for piles
                                const storage_tile_p_index =
                                    tile_p_components.entity_to_data.get(entity_id) orelse unreachable;
                                const storage_tile_p = tile_p_components.data[storage_tile_p_index];
                                if (base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p) < closest_pile_d) {
                                    closest_pile_d = base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p);
                                    target_tile_p_components.data[worker_target_tile_p_index] = storage_tile_p;
                                }
                            }
                        }
                        worker_state_components.data[wsc_index] = .pathing_to_target;
                    } else if (resources_on_board and
                        (sp_sim.sumOfInventory(inventory_components, worker_inventory_index) < sp_sim.worker_carry_cap))
                    {
                        var closest_resource_d: f32 = math.floatMax(f32);
                        for (0..resource_kind_components.data_count) |rk_component_index| {
                            const entity_id = resource_kind_components.data_to_entity
                                .get(rk_component_index) orelse unreachable;
                            const tile_p_index = tile_p_components.entity_to_data
                                .get(entity_id) orelse unreachable;
                            const tile_p = tile_p_components.data[tile_p_index];

                            if (base_math.vector2DistanceU16(tile_p_components.data[worker_tile_p_index], tile_p) < closest_resource_d) {
                                closest_resource_d = base_math.vector2DistanceU16(
                                    tile_p_components.data[worker_tile_p_index],
                                    tile_p,
                                );
                                target_tile_p_components.data[worker_target_tile_p_index] = tile_p;
                            }
                        }
                        worker_state_components.data[wsc_index] = .pathing_to_target;
                    } else worker_state_components.data[wsc_index] = .idle;
                },
                .pathing_to_target => {
                    sp_map.swpUpdateMap(
                        &game_state.scratch_fba,
                        game_state.sample_walk_map,
                        target_tile_p_components.data[worker_target_tile_p_index][1],
                    ) catch unreachable;

                    // Have worker navigate to rock
                    var next_worker_tile_p: @Vector(2, u16) = undefined;
                    var closest_tile_distance: usize = math.maxInt(usize);

                    const region_tile_index = tile_p_components.data[worker_tile_p_index][1];
                    const region_tile_row = @divTrunc(region_tile_index, board_dim);
                    const region_tile_col = region_tile_index % board_dim;
                    for (&[_]@Vector(2, i8){
                        @Vector(2, i8){ 0, -1 },
                        @Vector(2, i8){ 0, 1 },
                        @Vector(2, i8){ 1, 0 },
                        @Vector(2, i8){ -1, 0 },
                    }) |d_tile_coords| {
                        const neighbor_tile_p = @Vector(2, i8){
                            @intCast(region_tile_col),
                            @intCast(region_tile_row),
                        } + d_tile_coords;
                        if (!(neighbor_tile_p[1] >= board_dim or
                            neighbor_tile_p[1] < 0 or
                            neighbor_tile_p[0] >= board_dim or
                            neighbor_tile_p[0] < 0))
                        {
                            if (game_state.sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])] < closest_tile_distance) {
                                closest_tile_distance = game_state.sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])];
                                next_worker_tile_p = @Vector(2, u16){ 0, @intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0]) }; // FIXME(caleb): WORLD INDEX
                            }
                        }
                    }
                    tile_p_components.data[worker_tile_p_index] = next_worker_tile_p;
                    if (@reduce(.And, tile_p_components.data[worker_tile_p_index] == target_tile_p_components.data[worker_target_tile_p_index])) {
                        for (0..tile_p_components.data_count) |tp_component_index| {
                            if (@reduce(.And, tile_p_components.data[worker_tile_p_index] == tile_p_components.data[tp_component_index])) {
                                const entity_id = tile_p_components.data_to_entity
                                    .get(tp_component_index) orelse unreachable;
                                if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.resource))) {
                                    const rkc_index = resource_kind_components.entity_to_data
                                        .get(entity_id) orelse unreachable;
                                    switch (resource_kind_components.data[rkc_index]) {
                                        .rock => inventory_components.data[worker_inventory_index].rock_count += 1,
                                        .stick => inventory_components.data[worker_inventory_index].stick_count += 1,
                                        .berry => inventory_components.data[worker_inventory_index].berry_count += 1,
                                        .tree => unreachable, // TODO(caleb): Handle me
                                    }
                                    resource_kind_components.remove(entity_id);
                                    tile_p_components.remove(entity_id);
                                    entity_man.release(entity_id);
                                    break;
                                }
                            }
                        }
                        worker_state_components.data[wsc_index] = .choose_new_target;
                    }
                },
                .idle => {},
            }
        }

        // Update in game time
        switch (tick_granularity) {
            .minute => {
                game_state.game_time_minute += 1;
            },
            .hour => {
                game_state.game_time_hour += 1;
            },
            .day => {
                game_state.game_time_day += 1;
            },
            .year => {
                game_state.game_time_year += 1;
            },
            else => unreachable,
        }
        if (game_state.game_time_minute >= 60) {
            game_state.game_time_minute = 0;
            game_state.game_time_hour += 1;
        }
        if (game_state.game_time_hour >= 24) {
            game_state.game_time_hour = 0;
            game_state.game_time_day += 1;
        }
        if (game_state.game_time_day >= 365) {
            game_state.game_time_day = 0;
            game_state.game_time_year += 1;
        }
    }

    ////////////////////////////////
    //- cabarger: Render

    platform_api.beginDrawing();
    platform_api.clearBackground(.{ .r = 10, .g = 10, .b = 10, .a = 255 });

    sp_render.drawBoard(
        platform_api,
        game_state,
        @bitCast(scaled_tile_dim),

        view_mode, //- TODO(cabarger): use the int representations of these within game_state.
        draw_rot_state,
        world,
        tileset,
    );

    if (game_state.debug_draw_grid_lines)
        sp_render.debugDrawGridLines(
            platform_api,
            game_state,
            @bitCast(scaled_tile_dim),
        );

    if (game_state.debug_draw_tile_height)
        sp_render.debugDrawTileHeights(
            platform_api,
            game_state,
            world,
            @bitCast(scaled_tile_dim),
        );

    if (game_state.debug_draw_tile_hitboxes)
        sp_render.debugDrawTileHitboxes(
            platform_api,
            game_state,
            world,
            @bitCast(scaled_tile_dim),
        );

    if (game_state.is_paused)
        sp_render.drawPauseText(platform_api, game_state);

    if (true) //- NOTE(cabarger): Allways drawing debug info right now.
        sp_render.drawDebugInfo(
            platform_api,
            game_state,
            entity_man,
            resource_kind_components,
        );

    platform_api.endDrawing();
}

fn gameStateInit(game_state: *sp_platform.GameState, platform_api: *const sp_platform.PlatformAPI) void {
    const perm_ally = game_state.perm_fba.allocator();

    const max_entity_count = 2000;
    game_state.entity_man_offset = game_state.perm_fba.end_index;
    entity_man = perm_ally.create(ecs.EntityManager) catch unreachable;
    entity_man.* = ecs.EntityManager.init(perm_ally, max_entity_count) catch unreachable;

    game_state.tile_p_components_offset = game_state.perm_fba.end_index;
    tile_p_components = perm_ally.create(ecs.ComponentArray(@Vector(2, u16))) catch unreachable;
    tile_p_components.* =
        ecs.ComponentArray(@Vector(2, u16)).init(perm_ally, max_entity_count) catch unreachable;

    game_state.target_tile_p_components_offset = game_state.perm_fba.end_index;
    target_tile_p_components = perm_ally.create(ecs.ComponentArray(@Vector(2, u16))) catch unreachable;
    target_tile_p_components.* =
        ecs.ComponentArray(@Vector(2, u16)).init(perm_ally, max_entity_count) catch unreachable;

    game_state.resource_kind_components_offset = game_state.perm_fba.end_index;
    resource_kind_components = perm_ally.create(ecs.ComponentArray(sp_sim.ResourceKind)) catch unreachable;
    resource_kind_components.* =
        ecs.ComponentArray(sp_sim.ResourceKind).init(perm_ally, max_entity_count) catch unreachable;

    game_state.inventory_components_offset = game_state.perm_fba.end_index;
    inventory_components = perm_ally.create(ecs.ComponentArray(sp_sim.Inventory)) catch unreachable;
    inventory_components.* =
        ecs.ComponentArray(sp_sim.Inventory).init(perm_ally, max_entity_count) catch unreachable;

    game_state.worker_state_components_offset = game_state.perm_fba.end_index;
    worker_state_components = perm_ally.create(ecs.ComponentArray(sp_sim.WorkerState)) catch unreachable;
    worker_state_components.* =
        ecs.ComponentArray(sp_sim.WorkerState).init(perm_ally, max_entity_count) catch unreachable;

    game_state.tileset_offset = game_state.perm_fba.end_index;
    tileset = perm_ally.create(Tileset) catch unreachable;
    tileset.* = Tileset.init(&game_state.perm_fba, &game_state.scratch_fba, "./assets/art/small-planet.tsj", platform_api) catch unreachable;

    game_state.world_offset = game_state.perm_fba.end_index;
    world = perm_ally.create(World) catch unreachable;
    world.* = World{
        .region_data = perm_ally.alloc(
            RegionData,
            board_dim * board_dim,
        ) catch unreachable,
        .height_map = undefined,
    };

    game_state.seed = 116; // NOTE(caleb): Happens to be a good seed. Nothing special otherwise.
    game_state.xoshiro_256 = rand.DefaultPrng.init(game_state.seed);

    sp_map.genWorld(
        game_state.xoshiro_256.random(),
        tileset,
        world,
        entity_man,
        tile_p_components,
        resource_kind_components,
    ) catch unreachable;

    game_state.sample_walk_map = perm_ally.alloc(
        usize,
        board_dim * board_dim,
    ) catch unreachable;
    for (game_state.sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

    game_state.game_time_minute = 0;
    game_state.game_time_hour = 0;
    game_state.game_time_day = 0;
    game_state.game_time_year = 0;

    game_state.tick_granularity = @intFromEnum(sp_sim.TickGranularity.minute);

    game_state.debug_draw_distance_map = false;
    game_state.debug_draw_grid_lines = false;
    game_state.debug_draw_tile_height = false;
    game_state.debug_draw_tile_hitboxes = false;

    game_state.is_paused = false;
    game_state.pause_start_time = 0.0;
    game_state.last_tick_time = 0;

    game_state.view_mode = @intFromEnum(sp_map.ViewMode.world);
    game_state.selected_tile_p = @Vector(2, i8){ -1, -1 };
    game_state.selected_region_p = @Vector(2, u8){ 0, 0 };

    game_state.draw_3d = true;
    game_state.scale_factor = 2.0;

    game_state.board_translation = @Vector(2, f32){ 0.0, 0.0 };

    game_state.draw_rot_state = @intFromEnum(sp_render.DrawRotState.rotate_nonce);

    game_state.rl_font = platform_api.loadFont("assets/fonts/ComicMono.ttf");

    game_state.did_init = true;
}

/// Fight me.
fn fixStuffThatBrokeBecauseOfReload(game_state: *const sp_platform.GameState) void {
    entity_man =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.entity_man_offset));
    tile_p_components =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.tile_p_components_offset));
    target_tile_p_components =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.target_tile_p_components_offset));
    resource_kind_components =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.resource_kind_components_offset));
    inventory_components =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.inventory_components_offset));
    worker_state_components =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.worker_state_components_offset));
    tileset =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.tileset_offset));
    world =
        @ptrCast(@alignCast(game_state.perm_fba.buffer.ptr + game_state.world_offset));
}
