//!
//! small_planet_game_code.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/05/24
//! barg8397@vandals.uidaho.edu
//!
//! Small planet game code
//!

const std = @import("std");
const rl = @import("rl.zig");
const platform = @import("small_planet_platform.zig");
const ecs = @import("ecs.zig");

const math = std.math;
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const rand = std.rand;

const Tileset = @import("Tileset.zig");

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StaticBitSet = std.StaticBitSet;
const Random = rand.Random;

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const assert = std.debug.assert;

const board_dim = 33; // NOTE(caleb): Must = (power of 2) + 1
const max_height = 10;
const scale_inc: f32 = 0.25;
const glyph_size = 30;
const tick_rate_sec = 0.25;
const worker_carry_cap = 3;

const RegionData = struct {
    tiles: [board_dim * board_dim]u8,
};

const World = struct {
    region_data: []RegionData,
    height_map: []i16,
};

const TickGranularity = enum(u8) {
    minute = 0,
    hour,
    day,
    year,

    count,
};

const ViewMode = enum {
    region,
    world,
};

fn vector2Subtract(lhs: rl.Vector2, rhs: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = lhs.x - rhs.x,
        .y = lhs.y - rhs.y,
    };
}

fn vector2Add(lhs: rl.Vector2, rhs: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = lhs.x + rhs.x,
        .y = lhs.y + rhs.y,
    };
}

fn vector2Equals(lhs: rl.Vector2, rhs: rl.Vector2) c_int {
    if (lhs.x == rhs.x and lhs.y == rhs.y)
        return 0;
    return 1;
}

inline fn vector2DistanceU16(v1: @Vector(2, u16), v2: @Vector(2, u16)) f32 {
    const result = @sqrt(@as(f32, @floatFromInt((v1[0] - v2[0]) * (v1[0] - v2[0]) + (v1[1] - v2[1]) * (v1[1] - v2[1]))));
    return result;
}

inline fn clampf32(value: f32, min: f32, max: f32) f32 {
    return @max(min, @min(max, value));
}

const SWPEntry = struct {
    distance: usize,
    tile_p: @Vector(2, u16),
};

fn swpUpdateMap(
    scratch_fba: *FixedBufferAllocator,
    sample_walk_map: []usize,
    target_region_tile_index: u16,
) !void {
    const target_tile_row = @divTrunc(target_region_tile_index, board_dim);
    const target_tile_col = target_region_tile_index % board_dim;

    const restore_end_index = scratch_fba.end_index;
    defer scratch_fba.end_index = restore_end_index;
    const scratch_ally = scratch_fba.allocator();

    var swp_entry_list = try ArrayList(SWPEntry).initCapacity(scratch_ally, board_dim * board_dim);
    swp_entry_list.appendAssumeCapacity(.{ .distance = 0, .tile_p = @Vector(2, u16){ target_tile_col, target_tile_row } });

    for (sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

    while (swp_entry_list.items.len > 0) {
        const swp_entry = swp_entry_list.orderedRemove(0);
        sample_walk_map[swp_entry.tile_p[1] * board_dim + swp_entry.tile_p[0]] = swp_entry.distance;

        // Add neighbors that haven't already been queued. i.e distance isn't max int...
        for (&[_]@Vector(2, i8){
            @Vector(2, i8){ 0, -1 },
            @Vector(2, i8){ 0, 1 },
            @Vector(2, i8){ 1, 0 },
            @Vector(2, i8){ -1, 0 },
        }) |d_tile_coords| {
            const neighbor_tile_p = @Vector(2, i8){ @intCast(swp_entry.tile_p[0]), @intCast(swp_entry.tile_p[1]) } + d_tile_coords;
            if (!(neighbor_tile_p[1] >= board_dim or
                neighbor_tile_p[1] < 0 or
                neighbor_tile_p[0] >= board_dim or
                neighbor_tile_p[0] < 0))
            {
                if (sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])] == math.maxInt(usize)) {
                    swp_entry_list.appendAssumeCapacity(.{ .distance = swp_entry.distance + 1, .tile_p = @Vector(2, u16){
                        @intCast(neighbor_tile_p[0]),
                        @intCast(neighbor_tile_p[1]),
                    } });
                    sample_walk_map[@intCast(neighbor_tile_p[1] * board_dim + neighbor_tile_p[0])] = swp_entry.distance + 1;
                }
            }
        }
    }
}

const DrawRotState = enum(u8) {
    rotate_nonce = 0,
    rotate_once,
    rotate_twice,
    rotate_thrice,

    count,
};

inline fn isoProjMatrix() rl.Matrix {
    var result = mem.zeroes(rl.Matrix);
    result.m0 = 0.5;
    result.m1 = 0.25;
    result.m4 = -0.5;
    result.m5 = 0.25;
    result.m10 = 1.0;
    result.m15 = 1.0;
    return result;
}

inline fn matrixVector2Multiply(m: rl.Matrix, p: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = m.m0 * p.x + m.m4 * p.y,
        .y = m.m1 * p.x + m.m5 * p.y,
    };
}

fn screenSpaceBoardHeight(tile_width_px: u16, tile_height_px: u16) f32 {
    return matrixVector2Multiply(
        isoProjMatrix(),
        .{
            .x = board_dim * @as(f32, @floatFromInt(tile_width_px)),
            .y = board_dim * @as(f32, @floatFromInt(tile_height_px)),
        },
    ).y + @as(f32, @floatFromInt(tile_height_px)) / 2.0;
}

fn boardOffset(
    platform_api: *platform.PlatformAPI,
    tile_width_px: u16,
    tile_height_px: u16,
) rl.Vector2 {
    return rl.Vector2{
        .x = @as(f32, @floatFromInt(platform_api.getScreenWidth())) / 2.0 - @as(f32, @floatFromInt(tile_width_px)) / 2.0,
        .y = (@as(f32, @floatFromInt(platform_api.getScreenHeight())) - screenSpaceBoardHeight(tile_width_px, tile_height_px)) / 2.0,
    };
}

fn isoInvert(
    platform_api: *platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    const untranslated_p = vector2Subtract(p, board_translation);
    const unshifted_p = vector2Subtract(untranslated_p, boardOffset(platform_api, tile_width_px, tile_height_px));
    const invert_proj_mat = platform_api.matrixInvert(isoProjMatrix());
    return matrixVector2Multiply(invert_proj_mat, unshifted_p);
}

fn isoProj(
    platform_api: *platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    const projected_p = matrixVector2Multiply(isoProjMatrix(), p);
    const shifted_p = vector2Add(projected_p, boardOffset(platform_api, tile_width_px, tile_height_px));
    const translated_p = vector2Add(shifted_p, board_translation);
    return translated_p;
}

fn isoProjGlyph(
    platform_api: *platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    return vector2Add(isoProj(platform_api, p, tile_width_px, tile_height_px, board_translation), .{
        .x = glyph_size / 2 + glyph_size / 3,
        .y = 0,
    });
}

fn drawTile(
    platform_api: *platform.PlatformAPI,
    tileset: *const Tileset,
    tile_id: u16,
    dest_pos: rl.Vector2,
    this_scale_factor: f32,
    tint: rl.Color,
) void {
    const dest_rect = rl.Rectangle{
        .x = dest_pos.x,
        .y = dest_pos.y,
        .width = @as(f32, @floatFromInt(tileset.tile_width)) * this_scale_factor,
        .height = @as(f32, @floatFromInt(tileset.tile_height)) * this_scale_factor,
    };

    const target_tile_row = @divTrunc(tile_id, tileset.columns);
    const target_tile_column = @mod(tile_id, tileset.columns);
    const source_rect = rl.Rectangle{
        .x = @as(f32, @floatFromInt(target_tile_column * tileset.tile_width)),
        .y = @as(f32, @floatFromInt(target_tile_row * tileset.tile_height)),
        .width = @floatFromInt(tileset.tile_width),
        .height = @floatFromInt(tileset.tile_height),
    };

    platform_api.drawTexturePro(tileset.texture, source_rect, dest_rect, .{ .x = 0, .y = 0 }, 0, tint);
}

/// NOTE(Caleb): WTF chill with the params.
fn drawTileFromCoords(
    platform_api: *platform.PlatformAPI,
    world: *const World,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: rl.Vector2,
    selected_tile_p: @Vector(2, i8),
    height_scale: f32,
    scale_factor: f32,
) void {
    const tile_id = world.region_data[source_row_index * board_dim + source_col_index]
        .tiles[0];
    var dest_pos = isoProj(
        platform_api,
        .{
            .x = @as(f32, @floatFromInt(dest_col_index)) * scaled_tile_dim.x,
            .y = @as(f32, @floatFromInt(dest_row_index)) * scaled_tile_dim.y,
        },
        @intFromFloat(scaled_tile_dim.x),
        @intFromFloat(scaled_tile_dim.y),
        board_translation,
    );

    // Shift selected tile up
    if (selected_tile_p[0] == @as(i32, @intCast(dest_col_index)) and
        selected_tile_p[1] == @as(i32, @intCast(dest_row_index)))
        dest_pos.y -= (scaled_tile_dim.y / 2.0) * 0.25;

    // Shift up by height and height scale
    if (world.height_map[source_row_index * board_dim + source_col_index] > 0) {
        for (0..@intCast(world.height_map[source_row_index * board_dim + source_col_index])) |_| {
            dest_pos.y -= ((scaled_tile_dim.y / 2.0) * height_scale);
            drawTile(platform_api, tileset, tile_id, dest_pos, scale_factor, rl.WHITE);
        }
    } else {
        drawTile(platform_api, tileset, tile_id, dest_pos, scale_factor, rl.WHITE);
    }

    // NOTE(caleb): I will  want to draw resoruces again soon, just keeping this code around.
    // if (height >= 5) { // Draw tree
    //     dest_pos.y -= scaled_tile_dim.y / 2.0;
    //     drawTile(&tileset, tree_tile_id, dest_pos, scale_factor, rl.WHITE);
    // }
}

inline fn sumOfInventory(ica: *ecs.ComponentArray(Inventory), ica_index: usize) usize {
    var result: usize = 0;
    result += ica.data[ica_index].rock_count;
    result += ica.data[ica_index].stick_count;
    result += ica.data[ica_index].berry_count;
    return result;
}

fn genHeightMap(
    height_map: []i16,
    point: @Vector(2, u16),
    curr_dim: u16,
    rng: Random,
    random_height_scalar: f32,
) void {
    if (curr_dim == 1) // Base case
        return;

    const half_dim = @divTrunc(curr_dim, 2);
    const half_half_dim = @divTrunc(half_dim, 2);
    var random_height: i16 = @intFromFloat(@as(
        f32,
        @floatFromInt(rng.intRangeLessThan(i8, 0, max_height)),
    ) * random_height_scalar);
    // FIXME(caleb): The smoothness of these heightmaps is pretty
    // bad when adding a random height as per the algorithm.
    if (true) {
        random_height = 0;
    }
    const next_random_height_scalar = @max(-1.0, random_height_scalar - math.pow(f32, 2, -random_height_scalar));

    // Diamond step
    const top_left_height = height_map[(point[1] - half_dim) * board_dim + (point[0] - half_dim)];
    const top_right_height = height_map[(point[1] - half_dim) * board_dim + (point[0] + half_dim)];
    const bottom_left_height = height_map[(point[1] + half_dim) * board_dim + (point[0] - half_dim)];
    const bottom_right_height = height_map[(point[1] + half_dim) * board_dim + (point[0] + half_dim)];
    height_map[point[1] * board_dim + point[0]] = @divTrunc((top_left_height + top_right_height + bottom_left_height + bottom_right_height), 4) + random_height;

    // Square step
    const left_point = @Vector(2, u16){ point[0] - half_dim, point[1] };
    height_map[left_point[1] * board_dim + left_point[0]] = @divTrunc((top_left_height + bottom_left_height + height_map[point[1] * board_dim + point[0]]), 3) + random_height;
    const top_point = @Vector(2, u16){ point[0], point[1] - half_dim };
    height_map[top_point[1] * board_dim + top_point[0]] = @divTrunc((top_left_height + top_right_height + height_map[point[1] * board_dim + point[0]]), 3) + random_height;
    const right_point = @Vector(2, u16){ point[0] + half_dim, point[1] };
    height_map[right_point[1] * board_dim + right_point[0]] = @divTrunc((top_right_height + bottom_right_height + height_map[point[1] * board_dim + point[0]]), 3) + random_height;
    const bottom_point = @Vector(2, u16){ point[0], point[1] + half_dim };
    height_map[bottom_point[1] * board_dim + bottom_point[0]] = @divTrunc((top_right_height + bottom_right_height + height_map[point[1] * board_dim + point[0]]), 3) + random_height;

    genHeightMap(height_map, @Vector(2, u16){ point[0] - half_half_dim, point[1] - half_half_dim }, half_dim, rng, next_random_height_scalar); // Top left
    genHeightMap(height_map, @Vector(2, u16){ point[0] + half_half_dim, point[1] - half_half_dim }, half_dim, rng, next_random_height_scalar); // Top right
    genHeightMap(height_map, @Vector(2, u16){ point[0] - half_half_dim, point[1] + half_half_dim }, half_dim, rng, next_random_height_scalar); // Bottom left
    genHeightMap(height_map, @Vector(2, u16){ point[0] + half_half_dim, point[1] + half_half_dim }, half_dim, rng, next_random_height_scalar); // Bottom right
}

fn genWorld(
    rng: Random,
    tileset: *const Tileset,
    world: *World,
    entity_man: *ecs.EntityManager,
    tile_p_components: *ecs.ComponentArray(@Vector(2, u16)),
    resource_kind_components: *ecs.ComponentArray(ResourceKind),
) !void {
    // Generate a hightmap per region and use it to write sub-region tiles.
    for (0..board_dim * board_dim) |board_index|
        world.height_map[board_index] = 0;
    world.height_map[0] = rng.intRangeLessThan(i8, 0, max_height); // Top Left
    world.height_map[board_dim - 1] = rng.intRangeLessThan(i8, 0, max_height); // Top right
    world.height_map[(board_dim - 1) * board_dim] = rng.intRangeLessThan(i8, 0, max_height); // Bottom left
    world.height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = rng.intRangeLessThan(i8, 0, max_height); // Bottom right
    genHeightMap(
        world.height_map,
        @splat(@divTrunc(board_dim, 2)),
        board_dim,
        rng,
        1.0, // NOTE(caleb): Random height scalar lower = rougher terrain
    );
    for (0..board_dim * board_dim) |region_tile_index| {
        const height = world.height_map[region_tile_index];
        var tile_id: u16 = undefined;
        if (height <= 0) {
            tile_id = tileset.tile_type_to_id.get(.water).?;
        } else if (height == 1) {
            tile_id = tileset.tile_type_to_id.get(.sand).?;
        } else if (height > 1 and height < 4) {
            tile_id = tileset.tile_type_to_id.get(.forest).?;
        } else if (height > 3 and height < 7) {
            tile_id = tileset.tile_type_to_id.get(.plains).?;
        } else {
            tile_id = tileset.tile_type_to_id.get(.rock).?;
        }

        for (0..board_dim) |sub_region_row_index| {
            for (0..board_dim) |sub_region_col_index| {
                const sub_region_tile_index: u16 = @intCast(sub_region_row_index * board_dim + sub_region_col_index);
                world.region_data[region_tile_index].tiles[sub_region_tile_index] = @intCast(tile_id);

                // 1 in 10000 to gen a tree // FIXME(caleb): THIS IS A HACK AND SHOULD BE REPLACED!!
                if (height >= 5 and rng.uintLessThan(u16, 10000) == 0) {
                    const tree_entity_id = entity_man.newEntity();
                    entity_man.signatures[tree_entity_id] = entity_man.entitySignature(.resource);
                    tile_p_components.add(tree_entity_id, @Vector(2, u16){ @intCast(region_tile_index), sub_region_tile_index });
                    resource_kind_components.add(tree_entity_id, .tree);
                }
                // else if (rl.GetRandomValue(0, 10) == 0) { // 1 in 200 for a pile
                //     const pile_entity_id = entity_man.newEntity();
                //     inventory_components.add(pile_entity_id, .{});
                //     tile_p_components.add(pile_entity_id, @Vector(2, u16){ @intCast(region_tile_index), sub_region_tile_index });
                //     entity_man.signatures[pile_entity_id] = pile_entity_sig;
                // }
            }
        }
    }
}

const WorkerState = enum(u8) {
    pathing_to_target,
    choose_new_target,
    idle,
};

const Inventory = packed struct {
    rock_count: u16 = 0,
    stick_count: u16 = 0,
    berry_count: u16 = 0,
};

const ResourceKind = enum(usize) {
    rock,
    stick,
    tree,
    berry,
};

export fn smallPlanetGameCode(platform_api: *platform.PlatformAPI, game_state: *platform.GameState) void {
    var entity_man: *ecs.EntityManager = undefined;
    var tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var target_tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var resource_kind_components: *ecs.ComponentArray(ResourceKind) = undefined;
    var inventory_components: *ecs.ComponentArray(Inventory) = undefined;
    var worker_state_components: *ecs.ComponentArray(WorkerState) = undefined;
    var tileset: *Tileset = undefined;
    var world: *World = undefined;

    const perm_ally = game_state.perm_fba.allocator();
    const scratch_ally = game_state.scratch_fba.allocator();

    if (!game_state.did_init) {
        // ECS setup
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
        resource_kind_components = perm_ally.create(ecs.ComponentArray(ResourceKind)) catch unreachable;
        resource_kind_components.* =
            ecs.ComponentArray(ResourceKind).init(perm_ally, max_entity_count) catch unreachable;

        game_state.inventory_components_offset = game_state.perm_fba.end_index;
        inventory_components = perm_ally.create(ecs.ComponentArray(Inventory)) catch unreachable;
        inventory_components.* =
            ecs.ComponentArray(Inventory).init(perm_ally, max_entity_count) catch unreachable;

        game_state.worker_state_components_offset = game_state.perm_fba.end_index;
        worker_state_components = perm_ally.create(ecs.ComponentArray(WorkerState)) catch unreachable;
        worker_state_components.* =
            ecs.ComponentArray(WorkerState).init(perm_ally, max_entity_count) catch unreachable;

        // Load tileset

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
            .height_map = perm_ally.alloc(
                i16,
                board_dim * board_dim,
            ) catch unreachable,
        };

        game_state.seed = 116; // NOTE(caleb): Happens to be a good seed. Nothing special otherwise.
        game_state.xoshiro_256 = rand.DefaultPrng.init(game_state.seed);

        genWorld(
            game_state.xoshiro_256.random(),
            tileset,
            world,
            entity_man,
            tile_p_components,
            resource_kind_components,
        ) catch unreachable;

        game_state.sample_walk_map = perm_ally.alloc(usize, board_dim * board_dim) catch unreachable;
        for (game_state.sample_walk_map) |*distance|
            distance.* = math.maxInt(usize);

        game_state.game_time_minute = 0;
        game_state.game_time_hour = 0;
        game_state.game_time_day = 0;
        game_state.game_time_year = 0;

        game_state.tick_granularity = @intFromEnum(TickGranularity.minute);

        game_state.debug_draw_distance_map = false;
        game_state.debug_draw_grid_lines = false;

        game_state.is_paused = false;
        game_state.pause_start_time = 0.0;
        game_state.last_tick_time = 0;

        game_state.view_mode = @intFromEnum(ViewMode.world);
        game_state.selected_tile_p = @Vector(2, i8){ -1, -1 };
        game_state.selected_region_p = @Vector(2, u8){ 0, 0 };

        game_state.height_scale = 1.0;
        game_state.scale_factor = 2.0;

        game_state.board_translation = rl.Vector2{ .x = 0, .y = 0 };

        game_state.draw_rot_state = @intFromEnum(DrawRotState.rotate_nonce);

        game_state.rl_font = platform_api.getFontDefault();

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

    var tick_granularity: TickGranularity = @enumFromInt(game_state.tick_granularity);
    var view_mode: ViewMode = @enumFromInt(game_state.view_mode);
    var draw_rot_state: DrawRotState = @enumFromInt(game_state.draw_rot_state);

    const mouse_wheel_move = platform_api.getMouseWheelMove();
    if (mouse_wheel_move != 0) {
        game_state.scale_factor += if (mouse_wheel_move == -1) -scale_inc else scale_inc;
        game_state.scale_factor = clampf32(game_state.scale_factor, 1.0, 10.0);
    }

    const scaled_tile_dim: rl.Vector2 = .{
        .x = @as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor,
        .y = @as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor,
    };

    const new_mouse_p = platform_api.getMousePosition();
    const mouse_moved = (vector2Equals(game_state.mouse_p, new_mouse_p) == 0);
    game_state.mouse_p = new_mouse_p;
    const deprojected_mouse_p = isoInvert(
        platform_api,
        vector2Subtract(game_state.mouse_p, .{ .x = scaled_tile_dim.x / 2.0, .y = 0.0 }),
        @intFromFloat(scaled_tile_dim.x),
        @intFromFloat(scaled_tile_dim.y),
        game_state.board_translation,
    );
    if (mouse_moved) {
        game_state.selected_tile_p[0] = @intFromFloat(deprojected_mouse_p.x / scaled_tile_dim.x);
        game_state.selected_tile_p[1] = @intFromFloat(deprojected_mouse_p.y / scaled_tile_dim.y);
    }

    if (platform_api.isMouseButtonDown(rl.MOUSE_BUTTON_RIGHT))
        game_state.board_translation = vector2Add(game_state.board_translation, platform_api.getMouseDelta());

    var key_pressed = platform_api.getKeyPressed();
    while (key_pressed != 0) {
        if (key_pressed == rl.KEY_R and platform_api.isKeyDown(rl.KEY_LEFT_SHIFT)) {
            draw_rot_state =
                @enumFromInt(@mod(@as(i8, @intCast(game_state.draw_rot_state)) - 1, @intFromEnum(DrawRotState.count)));
            game_state.draw_rot_state = @intFromEnum(draw_rot_state);
        } else if (key_pressed == rl.KEY_R) {
            draw_rot_state =
                @enumFromInt((game_state.draw_rot_state + 1) % @intFromEnum(DrawRotState.count));
            game_state.draw_rot_state = @intFromEnum(draw_rot_state);
        } else if (key_pressed == rl.KEY_H) {
            game_state.height_scale = if (game_state.height_scale == 1.0) 0.0 else 1.0;
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

            genWorld(
                game_state.xoshiro_256.random(),
                tileset,
                world,
                entity_man,
                tile_p_components,
                resource_kind_components,
            ) catch unreachable;
        } else if (key_pressed == rl.KEY_UP) {
            tick_granularity =
                @enumFromInt((game_state.tick_granularity + 1) % @intFromEnum(TickGranularity.count));
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
        } else if (key_pressed == rl.KEY_E) {
            view_mode = .world;
            game_state.view_mode = @intFromEnum(view_mode);
        } else if (key_pressed == rl.KEY_ENTER and view_mode == .world) {
            // Check for a valid world_tile_p
            const zero_vec: @Vector(2, i8) = @splat(0);
            const dim_vec: @Vector(2, i8) = @splat(board_dim);
            if (@reduce(.And, game_state.selected_tile_p >= zero_vec) and @reduce(.And, game_state.selected_tile_p < dim_vec)) {
                view_mode = .region;
                game_state.view_mode = @intFromEnum(view_mode);
                game_state.selected_region_p = @intCast(game_state.selected_tile_p);
            }
        } else if (key_pressed == rl.KEY_SPACE) {
            game_state.is_paused = !game_state.is_paused;
            if (game_state.is_paused) { // NOTE(caleb): Record amount of time spent paused.
                game_state.pause_start_time = platform_api.getTime();
            } else game_state.last_tick_time += platform_api.getTime() - game_state.pause_start_time;
        }
        key_pressed = platform_api.getKeyPressed();
    }

    if (!game_state.is_paused and platform_api.getTime() - game_state.last_tick_time >= tick_rate_sec) {
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
                    if (sumOfInventory(inventory_components, worker_inventory_index) > 0) {
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

                    if ((piles_on_board and sumOfInventory(inventory_components, worker_inventory_index) >= worker_carry_cap) or
                        (piles_on_board and sumOfInventory(inventory_components, worker_inventory_index) > 0 and !resources_on_board))
                    {
                        var closest_pile_d: f32 = math.floatMax(f32);
                        for (0..inventory_components.data_count) |ic_index| {
                            const entity_id = inventory_components.data_to_entity
                                .get(ic_index) orelse unreachable;
                            if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) { // Filter for piles
                                const storage_tile_p_index =
                                    tile_p_components.entity_to_data.get(entity_id) orelse unreachable;
                                const storage_tile_p = tile_p_components.data[storage_tile_p_index];
                                if (vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p) < closest_pile_d) {
                                    closest_pile_d = vector2DistanceU16(tile_p_components.data[worker_tile_p_index], storage_tile_p);
                                    target_tile_p_components.data[worker_target_tile_p_index] = storage_tile_p;
                                }
                            }
                        }
                        worker_state_components.data[wsc_index] = .pathing_to_target;
                    } else if (resources_on_board and
                        (sumOfInventory(inventory_components, worker_inventory_index) < worker_carry_cap))
                    {
                        var closest_resource_d: f32 = math.floatMax(f32);
                        for (0..resource_kind_components.data_count) |rk_component_index| {
                            const entity_id = resource_kind_components.data_to_entity
                                .get(rk_component_index) orelse unreachable;
                            const tile_p_index = tile_p_components.entity_to_data
                                .get(entity_id) orelse unreachable;
                            const tile_p = tile_p_components.data[tile_p_index];

                            if (vector2DistanceU16(tile_p_components.data[worker_tile_p_index], tile_p) < closest_resource_d) {
                                closest_resource_d = vector2DistanceU16(
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
                    swpUpdateMap(
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

        game_state.last_tick_time = platform_api.getTime();
    }

    platform_api.beginDrawing();
    platform_api.clearBackground(.{ .r = 0, .g = 0, .b = 0, .a = 255 });

    // Draw board
    switch (view_mode) {
        .world => {
            switch (draw_rot_state) {
                .rotate_nonce => {
                    for (0..board_dim) |source_row_index| {
                        for (0..board_dim) |source_col_index| {
                            drawTileFromCoords(platform_api, world, tileset, source_row_index, source_col_index, source_row_index, source_col_index, scaled_tile_dim, game_state.board_translation, game_state.selected_tile_p, game_state.height_scale, game_state.scale_factor);
                        }
                    }
                },
                .rotate_once => {
                    var dest_tile_coords = @Vector(2, usize){ 0, 0 };
                    for (0..board_dim) |source_col_index| {
                        var source_row_index: isize = board_dim - 1;
                        while (source_row_index >= 0) : (source_row_index -= 1) {
                            drawTileFromCoords(platform_api, world, tileset, @intCast(source_row_index), source_col_index, dest_tile_coords[1], dest_tile_coords[0], scaled_tile_dim, game_state.board_translation, game_state.selected_tile_p, game_state.height_scale, game_state.scale_factor);
                            dest_tile_coords[0] += 1; // Increment col
                        }
                        dest_tile_coords[1] += 1; // Increment row
                        dest_tile_coords[0] = 0; // Reset col
                    }
                },
                .rotate_twice => {
                    var dest_tile_coords = @Vector(2, usize){ 0, 0 };
                    var source_row_index: isize = board_dim - 1;
                    while (source_row_index >= 0) : (source_row_index -= 1) {
                        var source_col_index: isize = board_dim - 1;
                        while (source_col_index >= 0) : (source_col_index -= 1) {
                            drawTileFromCoords(platform_api, world, tileset, @intCast(source_row_index), @intCast(source_col_index), dest_tile_coords[1], dest_tile_coords[0], scaled_tile_dim, game_state.board_translation, game_state.selected_tile_p, game_state.height_scale, game_state.scale_factor);
                            dest_tile_coords[0] += 1; // Increment col
                        }
                        dest_tile_coords[1] += 1; // Increment row
                        dest_tile_coords[0] = 0; // Reset col
                    }
                },
                .rotate_thrice => {
                    var dest_tile_coords = @Vector(2, usize){ 0, 0 };
                    var source_col_index: isize = board_dim - 1;
                    while (source_col_index >= 0) : (source_col_index -= 1) {
                        for (0..board_dim) |source_row_index| {
                            drawTileFromCoords(platform_api, world, tileset, @intCast(source_row_index), @intCast(source_col_index), dest_tile_coords[1], dest_tile_coords[0], scaled_tile_dim, game_state.board_translation, game_state.selected_tile_p, game_state.height_scale, game_state.scale_factor);
                            dest_tile_coords[0] += 1; // Increment col
                        }
                        dest_tile_coords[1] += 1; // Increment row
                        dest_tile_coords[0] = 0; // Reset col
                    }
                },
                else => unreachable,
            }
        },
        .region => {
            for (0..board_dim) |region_row_index| {
                for (0..board_dim) |region_col_index| {
                    const tile_id = world.region_data[
                        @as(usize, @intCast(game_state.selected_region_p[1])) *
                            board_dim + @as(usize, @intCast(game_state.selected_region_p[0]))
                    ].tiles[region_row_index * board_dim + region_col_index];
                    var dest_pos = isoProj(
                        platform_api,
                        .{
                            .x = @as(f32, @floatFromInt(region_col_index)) * scaled_tile_dim.x,
                            .y = @as(f32, @floatFromInt(region_row_index)) * scaled_tile_dim.y,
                        },
                        @intFromFloat(scaled_tile_dim.x),
                        @intFromFloat(scaled_tile_dim.y),
                        game_state.board_translation,
                    );

                    // Shift selected tile up
                    if (game_state.selected_tile_p[0] == @as(i32, @intCast(region_col_index)) and
                        game_state.selected_tile_p[1] == @as(i32, @intCast(region_row_index)))
                        dest_pos.y -= (scaled_tile_dim.y / 2.0) * 0.25;

                    drawTile(platform_api, tileset, tile_id, dest_pos, game_state.scale_factor, rl.WHITE);
                }
            }
        },
    }

    if (game_state.debug_draw_grid_lines) {
        // NOTE(caleb): + 1 for outer grid lines
        for (0..board_dim + 1) |grid_row| {
            const start_p = isoProj(
                platform_api,
                .{
                    .x = 0,
                    .y = @as(f32, @floatFromInt(grid_row)),
                },
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            const end_p = isoProj(
                platform_api,
                .{
                    .x = board_dim,
                    .y = @as(f32, @floatFromInt(grid_row)),
                },
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
        }
        for (0..board_dim + 1) |grid_col| {
            const start_p = isoProj(
                platform_api,
                .{
                    .x = @floatFromInt(grid_col),
                    .y = 0.0,
                },
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            const end_p = isoProj(
                platform_api,
                .{
                    .x = @as(f32, @floatFromInt(grid_col)),
                    .y = @as(f32, @floatFromInt(board_dim)),
                },
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
        }
    }

    // Draw distance map
    if (game_state.debug_draw_distance_map) {
        for (0..1) |wsc_index| { // NOTE(caleb): Make multi maps look ok??
            const worker_id =
                worker_state_components.data_to_entity.get(wsc_index) orelse unreachable;
            const worker_target_tile_p_index = target_tile_p_components.entity_to_data
                .get(worker_id) orelse unreachable;
            swpUpdateMap(
                &game_state.scratch_fba,
                game_state.sample_walk_map,
                target_tile_p_components.data[worker_target_tile_p_index][1],
            ) catch unreachable;

            for (0..board_dim) |grid_row_index| {
                for (0..board_dim) |grid_col_index| {
                    if (game_state.sample_walk_map[grid_row_index * board_dim + grid_col_index] < 10) {
                        const tile_distance =
                            game_state.sample_walk_map[grid_row_index * board_dim + grid_col_index];

                        const is_selected_tile = (game_state.selected_tile_p[0] == @as(i32, @intCast(grid_col_index)) and
                            game_state.selected_tile_p[1] == @as(i32, @intCast(grid_row_index)));

                        const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                        var projected_p = isoProjGlyph(
                            platform_api,
                            .{
                                .x = @as(f32, @floatFromInt(grid_col_index)),
                                .y = @as(f32, @floatFromInt(grid_row_index)),
                            },
                            @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                            @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                            game_state.board_translation,
                        );
                        projected_p.y += y_offset;
                        platform_api.drawTextCodepoint(
                            game_state.rl_font,
                            @intCast(tile_distance + '0'),
                            projected_p,
                            glyph_size,
                            .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                        );
                    }
                }
            }
        }
    }

    if (false) {
        // Draw entities w/ tile p component NOTE(caleb): This breaks draw order
        for (0..tile_p_components.data_count) |tpc_index| {
            const is_selected_tile = false; // FIXME(Caleb)

            const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
            var projected_p = isoProjGlyph(
                platform_api,
                tile_p_components.data[tpc_index],
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            projected_p.y += y_offset;

            const entity_id = tile_p_components.data_to_entity.get(tpc_index) orelse unreachable;
            if (entity_man.hasComponent(entity_id, .inventory) and !entity_man.hasComponent(entity_id, .worker_state)) {
                platform_api.drawTextCodepoint(
                    game_state.rl_font,
                    @intCast('p'),
                    projected_p,
                    glyph_size,
                    if (is_selected_tile) rl.YELLOW else .{ .r = 200, .g = 200, .b = 0, .a = 255 },
                );
            } else if (entity_man.hasComponent(entity_id, .resource_kind)) {
                const resource_kind_index = resource_kind_components.entity_to_data
                    .get(entity_id) orelse unreachable;
                switch (resource_kind_components.data[resource_kind_index]) {
                    .rock => {
                        platform_api.drawTextCodepoint(
                            game_state.rl_font,
                            @intCast('r'),
                            projected_p,
                            glyph_size,
                            if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                        );
                    },
                    .stick => {
                        platform_api.drawTextCodepoint(
                            game_state.rl_font,
                            @intCast('s'),
                            projected_p,
                            glyph_size,
                            if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                        );
                    },
                    .berry => {
                        platform_api.drawTextCodepoint(
                            game_state.rl_font,
                            @intCast('b'),
                            projected_p,
                            glyph_size,
                            if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                        );
                    },
                }
            } else if (entity_man.hasComponent(entity_id, .worker_state)) {} // Drawn seperately
            else unreachable;
        }

        // HACK(caleb): draw workers seperately.
        for (0..worker_state_components.data_count) |wsc_index| {
            const worker_entity_id =
                worker_state_components.data_to_entity.get(wsc_index) orelse unreachable;
            const worker_tile_p_index =
                tile_p_components.entity_to_data.get(worker_entity_id) orelse unreachable;
            const is_selected_tile = false; // FIXME
            const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
            var projected_p = isoProjGlyph(
                platform_api,
                tile_p_components.data[worker_tile_p_index],
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor),
                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor),
                game_state.board_translation,
            );
            projected_p.y += y_offset;
            platform_api.drawTextCodepoint(game_state.rl_font, @intCast('@'), projected_p, glyph_size, rl.ORANGE);
        }
    }

    // Info about selected tile
    {
        var offset_y: f32 = 0.0;
        for (0..tile_p_components.data_count) |tpc_index| {
            if (false) { //FIXME) {
                const restore_end_index = game_state.scratch_fba.end_index;
                defer game_state.scratch_fba.end_index = restore_end_index;

                const entity_id = tile_p_components.data_to_entity
                    .get(tpc_index) orelse unreachable;

                var infoz: [:0]const u8 = undefined;
                if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.worker))) {
                    infoz = fmt.allocPrintZ(
                        scratch_ally,
                        "WORKER",
                        .{},
                    ) catch unreachable;
                } else if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.pile))) {
                    infoz = fmt.allocPrintZ(
                        scratch_ally,
                        "PILE",
                        .{},
                    ) catch unreachable;
                } else if (entity_man.signatures[entity_id].eql(entity_man.entitySignature(.resource))) {
                    infoz = fmt.allocPrintZ(
                        scratch_ally,
                        "RESOURCE",
                        .{},
                    ) catch unreachable;
                }
                platform_api.drawTextEx(game_state.rl_font, infoz, .{
                    .x = 0.0,
                    .y = @as(f32, @floatFromInt(platform_api.getScreenHeight() - @divFloor(
                        platform_api.getScreenHeight(),
                        5,
                    ))) + offset_y,
                }, glyph_size, 1.0, rl.WHITE);
                offset_y += 40.0;
            }
        }
    }

    // Pause text
    if (game_state.is_paused) {
        const paused_width = platform_api.measureText("PAUSED", glyph_size * 2);
        platform_api.drawTextEx(game_state.rl_font, "PAUSED", .{
            .x = @floatFromInt(@divFloor(platform_api.getScreenWidth(), 2) - @divFloor(paused_width, 2)),
            .y = @floatFromInt(platform_api.getScreenHeight() - @divFloor(platform_api.getScreenHeight(), 5)),
        }, glyph_size * 2, 1.0, rl.WHITE);
    }

    // Debug info
    {
        const restore_end_index = game_state.scratch_fba.end_index;
        defer game_state.scratch_fba.end_index = restore_end_index;

        const game_timez = fmt.allocPrintZ(
            scratch_ally,
            "Time: {d}::{d}::{d}::{d}",
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
        for (&[_][:0]const u8{
            game_timez,
            resource_countz,
            entity_countz,
        }, 0..) |strz, strz_index| {
            platform_api.drawTextEx(game_state.rl_font, strz, .{
                .x = 0, //@as(f32, @floatFromInt(board_dim)) * tile_width_px,
                .y = @as(f32, @floatFromInt(strz_index)) * 40,
            }, glyph_size, 1.0, rl.WHITE);
        }
    }

    platform_api.endDrawing();
}
