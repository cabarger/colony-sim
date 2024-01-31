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

export fn spUpdateAndRender(
    platform_api: *const sp_platform.PlatformAPI,
    game_state: *sp_platform.GameState,
    tctx: *base_thread_context.TCTX,
) void {
    _ = tctx;
    const perm_ally = game_state.perm_fba.allocator();
    const scratch_ally = game_state.scratch_fba.allocator();

    var entity_man: *ecs.EntityManager = undefined;
    var tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var target_tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var resource_kind_components: *ecs.ComponentArray(sp_sim.ResourceKind) = undefined;
    var inventory_components: *ecs.ComponentArray(sp_sim.Inventory) = undefined;
    var worker_state_components: *ecs.ComponentArray(sp_sim.WorkerState) = undefined;
    var tileset: *Tileset = undefined;
    var world: *World = undefined;

    //- cabarger: This is done once on FIRST game code load
    if (!game_state.did_init) {
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

    var tick_granularity: sp_sim.TickGranularity = @enumFromInt(game_state.tick_granularity);
    var view_mode: sp_map.ViewMode = @enumFromInt(game_state.view_mode);
    var draw_rot_state: sp_render.DrawRotState = @enumFromInt(game_state.draw_rot_state);

    const mouse_wheel_move = platform_api.getMouseWheelMove();
    if (mouse_wheel_move != 0) {
        game_state.scale_factor += if (mouse_wheel_move == -1) -sp_render.scale_inc else sp_render.scale_inc;
        game_state.scale_factor = base_math.clampF32(game_state.scale_factor, 0.25, 10.0);
    }

    const scaled_tile_dim: rl.Vector2 = .{
        .x = @as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor,
        .y = @as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor,
    };

    const new_mouse_p = platform_api.getMousePosition();
    const mouse_moved = @reduce(.And, @as(@Vector(2, f32), @bitCast(game_state.mouse_p)) != @as(@Vector(2, f32), @bitCast(new_mouse_p)));
    game_state.mouse_p = new_mouse_p;

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
                    if (platform_api.checkCollisionPointRec(game_state.mouse_p, .{
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
                @as(@Vector(2, f32), @bitCast(game_state.mouse_p)) - @Vector(2, f32){ scaled_tile_dim.x / 2.0, 0.0 },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            game_state.selected_tile_p[0] = @intFromFloat(deprojected_mouse_p.x / scaled_tile_dim.x);
            game_state.selected_tile_p[1] = @intFromFloat(deprojected_mouse_p.y / scaled_tile_dim.y);
        }
    }

    if (platform_api.isMouseButtonDown(rl.MOUSE_BUTTON_RIGHT))
        game_state.board_translation += @bitCast(platform_api.getMouseDelta());

    var key_pressed = platform_api.getKeyPressed();
    while (key_pressed != 0) { //- cabarger: Handle input
        if (key_pressed == rl.KEY_R and platform_api.isKeyDown(rl.KEY_LEFT_SHIFT)) {
            draw_rot_state =
                @enumFromInt(@mod(@as(i8, @intCast(game_state.draw_rot_state)) - 1, @intFromEnum(sp_render.DrawRotState.count)));
            game_state.draw_rot_state = @intFromEnum(draw_rot_state);
        } else if (key_pressed == rl.KEY_R) {
            draw_rot_state =
                @enumFromInt((game_state.draw_rot_state + 1) % @intFromEnum(sp_render.DrawRotState.count));
            game_state.draw_rot_state = @intFromEnum(draw_rot_state);
        } else if (key_pressed == rl.KEY_H) {
            game_state.draw_3d = !game_state.draw_3d;
        } else if (key_pressed == rl.KEY_KP_6 or key_pressed == rl.KEY_KP_4) {
            if (key_pressed == rl.KEY_KP_6) {
                game_state.seed += 1;
            } else if (key_pressed == rl.KEY_KP_4 and game_state.seed > 0)
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
        } else if (key_pressed == rl.KEY_UP) {
            tick_granularity =
                @enumFromInt((game_state.tick_granularity + 1) % @intFromEnum(sp_sim.TickGranularity.count));
            game_state.tick_granularity = @intFromEnum(tick_granularity);
            game_state.selected_tile_p[1] -= 1;
        } else if (key_pressed == rl.KEY_DOWN) {
            game_state.selected_tile_p[1] += 1;
        } else if (key_pressed == rl.KEY_LEFT) {
            game_state.selected_tile_p[0] -= 1;
        } else if (key_pressed == rl.KEY_RIGHT) {
            game_state.selected_tile_p[0] += 1;
        } else if (key_pressed == rl.KEY_F1) {
            game_state.debug_draw_distance_map = !game_state.debug_draw_distance_map;
        } else if (key_pressed == rl.KEY_F2) {
            game_state.debug_draw_grid_lines = !game_state.debug_draw_grid_lines;
        } else if (key_pressed == rl.KEY_F3) {
            game_state.debug_draw_tile_height = !game_state.debug_draw_tile_height;
        } else if (key_pressed == rl.KEY_F4) {
            game_state.debug_draw_tile_hitboxes = !game_state.debug_draw_tile_hitboxes;
        } else if (key_pressed == rl.KEY_E) {
            view_mode = .world;
            game_state.view_mode = @intFromEnum(view_mode);
        } else if (key_pressed == rl.KEY_ENTER and view_mode == .world) {
            const zero_vec: @Vector(2, i8) = @splat(0);
            const dim_vec: @Vector(2, i8) = @splat(board_dim);
            if (@reduce(.And, game_state.selected_tile_p >= zero_vec) and @reduce(.And, game_state.selected_tile_p < dim_vec)) {
                view_mode = .region;
                game_state.view_mode = @intFromEnum(view_mode);
                game_state.selected_region_p = @intCast(
                    sp_map.canonicalTileP(game_state.selected_tile_p, @enumFromInt(game_state.draw_rot_state)),
                );
            }
        } else if (key_pressed == rl.KEY_SPACE) {
            game_state.is_paused = !game_state.is_paused;
            if (game_state.is_paused) { //- cabarger: Record amount of time spent paused.
                game_state.pause_start_time = platform_api.getTime();
            } else game_state.last_tick_time += platform_api.getTime() - game_state.pause_start_time;
        }
        key_pressed = platform_api.getKeyPressed();
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

    //- cabarger: Draw

    platform_api.beginDrawing();
    platform_api.clearBackground(.{ .r = 0, .g = 0, .b = 0, .a = 255 });

    // Draw board
    switch (view_mode) {
        .world => {
            for (0..board_dim) |dest_row_index| {
                for (0..board_dim) |dest_col_index| {
                    var source_tile_coords: @Vector(2, usize) = @intCast(
                        sp_map.canonicalTileP(@intCast(@Vector(2, usize){ dest_col_index, dest_row_index }), draw_rot_state),
                    );
                    sp_render.drawWorldTileFromCoords(
                        platform_api,
                        world,
                        tileset,
                        source_tile_coords[1],
                        source_tile_coords[0],
                        dest_row_index,
                        dest_col_index,
                        scaled_tile_dim,
                        @bitCast(game_state.board_translation),
                        game_state.selected_tile_p,
                        game_state.draw_3d,
                        game_state.scale_factor,
                    );
                }
            }
        },
        .region => {
            const region_data_index: usize = @intCast(
                game_state.selected_region_p[1] * @as(usize, @intCast(board_dim)) + game_state.selected_region_p[0],
            );
            for (0..board_dim) |dest_row_index| {
                for (0..board_dim) |dest_col_index| {
                    var source_tile_coords: @Vector(2, usize) = @intCast(
                        sp_map.canonicalTileP(@intCast(@Vector(2, usize){ dest_col_index, dest_row_index }), draw_rot_state),
                    );
                    sp_render.drawRegionTileFromCoords(
                        platform_api,
                        &world.region_data[region_data_index],
                        tileset,
                        source_tile_coords[1],
                        source_tile_coords[0],
                        dest_row_index,
                        dest_col_index,
                        scaled_tile_dim,
                        @bitCast(game_state.board_translation),
                        game_state.selected_tile_p,
                        game_state.draw_3d,
                        game_state.scale_factor,
                    );
                }
            }
        },
    }

    if (game_state.debug_draw_grid_lines) {
        // NOTE(caleb): + 1 for outer grid lines
        for (0..board_dim + 1) |grid_row| {
            const start_p = sp_render.isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x / 2.0,
                    .y = scaled_tile_dim.y * @as(f32, @floatFromInt(grid_row)) - scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            const end_p = sp_render.isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x * @as(f32, @floatFromInt(board_dim)) + scaled_tile_dim.x / 2.0,
                    .y = scaled_tile_dim.y * @as(f32, @floatFromInt(grid_row)) - scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
        }
        for (0..board_dim + 1) |grid_col| {
            const start_p = sp_render.isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x * @as(f32, @floatFromInt(grid_col)) + scaled_tile_dim.x / 2.0,
                    .y = -scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            const end_p = sp_render.isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x * @as(f32, @floatFromInt(grid_col)) + scaled_tile_dim.x / 2.0,
                    .y = scaled_tile_dim.y * @as(f32, @floatFromInt(board_dim)) - scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
        }
    }

    if (game_state.debug_draw_tile_height) {
        for (0..board_dim) |row_index| {
            for (0..board_dim) |col_index| {
                const canonical_board_p: @Vector(2, usize) = @intCast(
                    sp_map.canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), @enumFromInt(game_state.draw_rot_state)),
                );
                const height = switch (@as(sp_map.ViewMode, @enumFromInt(game_state.view_mode))) {
                    .region => world.region_data[game_state.selected_region_p[1] * board_dim + game_state.selected_region_p[0]]
                        .height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                    .world => world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                };
                var projected_p = sp_render.isoProjGlyph(
                    platform_api,
                    .{
                        .x = @as(f32, @floatFromInt(col_index)) * scaled_tile_dim.x + scaled_tile_dim.x / 2.0 + scaled_tile_dim.x / 3.0,
                        .y = @as(f32, @floatFromInt(row_index)) * scaled_tile_dim.y,
                    },
                    @intFromFloat(scaled_tile_dim.x),
                    @intFromFloat(scaled_tile_dim.y),
                    @bitCast(game_state.board_translation),
                );
                if (game_state.draw_3d)
                    projected_p.y -= @as(f32, @floatFromInt(height)) * (scaled_tile_dim.y / 2.0);
                platform_api.drawTextCodepoint(
                    game_state.rl_font,
                    @intCast(height + '0'),
                    projected_p,
                    sp_render.glyph_size,
                    .{ .r = 0, .g = 0, .b = 255, .a = 255 },
                );
            }
        }
    }

    if (game_state.debug_draw_tile_hitboxes) {
        for (0..board_dim) |row_index| {
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
                    @bitCast(game_state.board_translation),
                );
                projected_p.y -= @as(f32, @floatFromInt(height)) * (scaled_tile_dim.y / 2.0);
                platform_api.drawRectangleLinesEx(.{
                    .x = projected_p.x,
                    .y = projected_p.y,
                    .width = scaled_tile_dim.x,
                    .height = scaled_tile_dim.y / 2.0,
                }, 1, rl.GREEN);
            }
        }
    }

    // Pause text
    if (game_state.is_paused) {
        const paused_width = platform_api.measureText("PAUSED", sp_render.glyph_size * 2);
        platform_api.drawTextEx(game_state.rl_font, "PAUSED", .{
            .x = @floatFromInt(@divFloor(platform_api.getScreenWidth(), 2) - @divFloor(paused_width, 2)),
            .y = @floatFromInt(platform_api.getScreenHeight() - @divFloor(platform_api.getScreenHeight(), 5)),
        }, sp_render.glyph_size * 2, 1.0, rl.WHITE);
    }

    // Debug info
    {
        const restore_end_index = game_state.scratch_fba.end_index;
        defer game_state.scratch_fba.end_index = restore_end_index;

        const game_timez = fmt.allocPrintZ(
            scratch_ally,
            "Time: {d}:{d}:{d}:{d}",
            .{ game_state.game_time_year, game_state.game_time_day, game_state.game_time_hour, game_state.game_time_minute },
        ) catch unreachable;
        const entity_countz = fmt.allocPrintZ(
            scratch_ally,
            "Entity count: {d}",
            .{entity_man.free_entities.capacity - entity_man.free_entities.items.len},
        ) catch unreachable;
        const resource_countz = fmt.allocPrintZ(
            scratch_ally,
            "Resource count: {d}",
            .{resource_kind_components.data_count},
        ) catch unreachable;

        const fpsz = fmt.allocPrintZ(
            scratch_ally,
            "FPS: {d}",
            .{platform_api.getFPS()},
        ) catch unreachable;

        const selected_region_p = fmt.allocPrintZ(
            scratch_ally,
            "Selected region p: ({d}, {d})",
            .{ game_state.selected_region_p[0], game_state.selected_region_p[1] },
        ) catch unreachable;

        const selected_tile_p = fmt.allocPrintZ(
            scratch_ally,
            "Selected tile p: ({d}, {d})",
            .{ game_state.selected_tile_p[0], game_state.selected_tile_p[1] },
        ) catch unreachable;

        for (&[_][:0]const u8{
            game_timez,
            resource_countz,
            entity_countz,
            fpsz,
            selected_region_p,
            selected_tile_p,
        }, 0..) |strz, strz_index| {
            platform_api.drawTextEx(game_state.rl_font, strz, .{
                .x = 0, //@as(f32, @floatFromInt(board_dim)) * tile_width_px,
                .y = @as(f32, @floatFromInt(strz_index)) * sp_render.glyph_size,
            }, sp_render.glyph_size, 1.0, rl.WHITE);
        }
    }
    platform_api.endDrawing();
}
