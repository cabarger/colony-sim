//!
//! sp_game_code.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/05/24
//! barg8397@vandals.uidaho.edu
//!

//- TODO(cabarger):
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

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StaticBitSet = std.StaticBitSet;
const Random = rand.Random;

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const assert = std.debug.assert;
const board_dim = sp_map.board_dim;

inline fn scaledTileDim(
    tile_width: u16,
    tile_height: u16,
    scale_factor: f32,
) @Vector(2, f32) {
    return .{
        @as(f32, @floatFromInt(tile_width)) * scale_factor,
        @as(f32, @floatFromInt(tile_height)) * scale_factor,
    };
}

const GameMode = enum(u8) {
    title_screen,
    play,
    count,
};

//- NOTE(cabarger): Possible sub update/draw code paths depending on game mode:
// title_screen:
//    credits
//    title screen itself
//
// play:
//    board
//    help
//    resource management
//    past/future resource use
//

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

    switch (@as(GameMode, @enumFromInt(game_state.game_mode))) {
        .play => {
            ////////////////////////////////
            //- cabarger: Handle game inputs

            //- cabarger: Update game scale factor on mouse scroll
            const mouse_wheel_move = game_input.mouse_input.wheel_move;
            if (mouse_wheel_move != 0) {
                game_state.scale_factor += if (mouse_wheel_move == -1) -sp_render.scale_inc else sp_render.scale_inc;
                game_state.scale_factor = base_math.clampF32(game_state.scale_factor, 0.25, 10.0);
            }

            //- cabarger: Get selected tile from mouse on mouse move
            if (@reduce(.And, game_input.last_mouse_input.p != game_input.mouse_input.p))
                game_state.selected_tile_p = selectedTilePFromMouseP(platform_api, game_state, game_input);

            //- cabarger: Translate the board (panning) akin to the move tool in a paint program.
            if (game_input.mouse_input.right_click)
                game_state.board_translation += game_input.mouse_input.p - game_input.last_mouse_input.p;

            //- cabarger: Rotate the board counter clockwise
            if (game_input.keyPressed(.r) and game_input.keyHeld(.left_shift)) {
                game_state.draw_rot_state = @intCast(@mod(
                    @as(i8, @intCast(game_state.draw_rot_state)) - 1,
                    @intFromEnum(sp_render.DrawRotState.count),
                ));
            }

            //- cabarger: Rotate the board clockwise
            else if (game_input.keyPressed(.r)) {
                game_state.draw_rot_state = (game_state.draw_rot_state + 1) % @intFromEnum(sp_render.DrawRotState.count);
            }

            //- cabarger: Toggle Z drawing of tiles
            if (game_input.keyPressed(.h)) {
                game_state.draw_3d = !game_state.draw_3d;
            }

            //- NOTE(cabarger): Advance the seed and generate a new world... This is for debugging!
            if (game_input.keyPressed(.kp_4) or game_input.keyPressed(.kp_6)) {
                //- cabarger: RNG from new seed
                if (game_input.keyPressed(.kp_6)) {
                    game_state.seed += 1;
                } else if (game_input.keyPressed(.kp_4) and game_state.seed > 0)
                    game_state.seed -= 1;
                game_state.xoshiro_256 = rand.DefaultPrng.init(game_state.seed);
                std.debug.print("{d}\n", .{game_state.seed}); //- NOTE(cabarger): It's nice to see what seed we are working with.

                entity_man.reset();
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
            }

            //- cabarger: Toggle game mode
            if (game_input.keyPressed(.m)) {
                game_state.game_mode = (game_state.game_mode + 1) % @intFromEnum(GameMode.count);
            }

            //- cabarger: Cycle tick granularity
            if (game_input.keyPressed(.t)) {
                game_state.tick_granularity = (game_state.tick_granularity + 1) % @intFromEnum(sp_sim.TickGranularity.count);
            }

            //- cabarger: Change selected tile via arrow key
            if (game_input.keyPressed(.up)) {
                game_state.selected_tile_p[1] -= 1;
            } else if (game_input.keyPressed(.down)) {
                game_state.selected_tile_p[1] += 1;
            } else if (game_input.keyPressed(.left)) {
                game_state.selected_tile_p[0] -= 1;
            } else if (game_input.keyPressed(.right)) {
                game_state.selected_tile_p[0] += 1;
            }

            //- cabarger: Debug draw distnace map
            if (game_input.keyPressed(.f1)) {
                game_state.debug_draw_distance_map = !game_state.debug_draw_distance_map;
            }

            //- cabarger: Debug draw tile grid
            if (game_input.keyPressed(.f2)) {
                game_state.debug_draw_grid_lines = !game_state.debug_draw_grid_lines;
            }

            //- cabarger: Debug draw tile height value overlay
            if (game_input.keyPressed(.f3)) {
                game_state.debug_draw_tile_height = !game_state.debug_draw_tile_height;
            }

            //- cabarger: Debug draw tile hitboxes
            if (game_input.keyPressed(.f4)) {
                game_state.debug_draw_tile_hitboxes = !game_state.debug_draw_tile_hitboxes;
            }

            //- cabarger: Pause the game
            if (game_input.keyPressed(.space)) {
                game_state.is_paused = !game_state.is_paused;
                if (game_state.is_paused) { //- cabarger: Record amount of time spent paused.
                    game_state.pause_start_time = platform_api.getTime();
                } else game_state.last_tick_time += platform_api.getTime() - game_state.pause_start_time;
            }

            ////////////////////////////////
            //- cabarger: Simulation updates

            const time_now = platform_api.getTime();
            if (!game_state.is_paused and time_now - game_state.last_tick_time >= sp_sim.tick_rate_sec) {
                game_state.last_tick_time = time_now;
                //- cabarger: Game tick updates updates happen here. The initial thinking was something like
                // do stuff in the block n times depending on tick granularity.
                sp_sim.updateWorkers();
                sp_sim.updateGameTime(
                    @enumFromInt(game_state.tick_granularity),
                    &game_state.game_time_minute,
                    &game_state.game_time_hour,
                    &game_state.game_time_day,
                    &game_state.game_time_year,
                );
            }

            ////////////////////////////////
            //- cabarger: Draw

            platform_api.beginDrawing();
            platform_api.clearBackground(.{ .r = 10, .g = 10, .b = 10, .a = 255 });

            const draw_info = sp_render.DrawInfo{
                .scaled_tile_dim = scaledTileDim(
                    tileset.tile_width,
                    tileset.tile_height,
                    game_state.scale_factor,
                ),
                .draw_rot_state = @enumFromInt(game_state.draw_rot_state),
                .board_translation = game_state.board_translation,
                // .selected_tile_p = game_state.selected_tile_p,
                .draw_3d = game_state.draw_3d,
                .scale_factor = game_state.scale_factor,
            };

            sp_render.drawBoard(
                &draw_info,
                platform_api,
                world,
                tileset,
                game_state.selected_tile_p,
            );

            if (game_state.debug_draw_grid_lines)
                sp_render.debugDrawGridLines(&draw_info, platform_api);

            if (game_state.debug_draw_tile_height)
                sp_render.debugDrawTileHeights(
                    &draw_info,
                    platform_api,
                    world,
                    game_state.rl_font,
                );

            if (game_state.debug_draw_tile_hitboxes)
                sp_render.debugDrawTileHitboxes(&draw_info, platform_api, world);

            if (game_state.is_paused)
                sp_render.drawPauseText(platform_api, game_state.rl_font);

            if (true) //- NOTE(cabarger): Allways drawing debug info right now.
                sp_render.drawDebugInfo(
                    platform_api,
                    game_state,
                    entity_man,
                    resource_kind_components,
                );
            platform_api.endDrawing();
        },
        .title_screen => {
            //- cabarger: Title screen update
            if (game_input.keyPressed(.m)) {
                game_state.game_mode = (game_state.game_mode + 1) % @intFromEnum(GameMode.count);
            }

            //- cabarger: Title screen draw
            platform_api.beginDrawing();
            platform_api.clearBackground(.{ .r = 10, .g = 10, .b = 10, .a = 255 });
            sp_render.drawTitleScreenText(platform_api, game_state.rl_font);
            platform_api.endDrawing();
        },
        .count => unreachable,
    }
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

    game_state.game_mode = @intFromEnum(GameMode.play);

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

    game_state.selected_tile_p = @Vector(2, i8){ -1, -1 };

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

fn selectedTilePFromMouseP(
    platform_api: *const sp_platform.PlatformAPI,
    game_state: *sp_platform.GameState,
    game_input: *const sp_platform.GameInput,
) @Vector(2, i8) {
    var result: @Vector(2, i8) = .{ -1, -1 };
    const scaled_tile_dim = scaledTileDim(tileset.tile_width, tileset.tile_height, game_state.scale_factor);
    if (game_state.draw_3d) {
        var row_index: isize = board_dim - 1;
        outer: while (row_index >= 0) : (row_index -= 1) {
            var col_index: isize = board_dim - 1;
            while (col_index >= 0) : (col_index -= 1) {
                const canonical_board_p: @Vector(2, usize) = @intCast(
                    sp_map.canonicalTileP(@intCast(@Vector(2, usize){ @intCast(col_index), @intCast(row_index) }), @enumFromInt(game_state.draw_rot_state)),
                );
                const height = world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]];
                var projected_p = sp_render.isoProj(
                    platform_api,
                    .{
                        .x = @as(f32, @floatFromInt(col_index)) * scaled_tile_dim[0],
                        .y = @as(f32, @floatFromInt(row_index)) * scaled_tile_dim[1],
                    },
                    @intFromFloat(scaled_tile_dim[0]),
                    @intFromFloat(scaled_tile_dim[1]),
                    game_state.board_translation,
                );
                projected_p.y -= @as(f32, @floatFromInt(height)) * (scaled_tile_dim[1] / 2.0);
                if (platform_api.checkCollisionPointRec(@bitCast(game_input.mouse_input.p), .{
                    .x = projected_p.x,
                    .y = projected_p.y,
                    .width = scaled_tile_dim[0],
                    .height = scaled_tile_dim[1] / 2.0,
                })) {
                    result = .{ @intCast(col_index), @intCast(row_index) };
                    break :outer;
                }
            }
        }
    } else {
        const deprojected_mouse_p: @Vector(2, f32) = @bitCast(sp_render.isoInvert(
            platform_api,
            game_input.mouse_input.p - @Vector(2, f32){ scaled_tile_dim[0] / 2.0, 0.0 },
            @intFromFloat(scaled_tile_dim[0]),
            @intFromFloat(scaled_tile_dim[1]),
            game_state.board_translation,
        ));
        const resultF32 = deprojected_mouse_p / scaled_tile_dim;
        result = .{ @intFromFloat(resultF32[0]), @intFromFloat(resultF32[1]) };
    }
    return result;
}
