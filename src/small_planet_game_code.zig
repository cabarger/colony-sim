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

//- TODO(Caleb): Replace raylib Vector2 with builtin @Vector

const std = @import("std");
const rl = @import("rl.zig");
const platform = @import("small_planet_platform.zig");
const ecs = @import("ecs.zig");
const base_math = @import("base/base_math.zig");

const math = std.math;
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const rand = std.rand;

const Tileset = @import("Tileset.zig");
const Matrix2x2I8 = base_math.Matrix2x2I8;

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StaticBitSet = std.StaticBitSet;
const Random = rand.Random;

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const assert = std.debug.assert;

const board_dim = 17; //33; // NOTE(caleb): Must = (power of 2) + 1
const max_height = 10;
const scale_inc: f32 = 0.25;
const glyph_size = 18;
const tick_rate_sec = 0.25;
const worker_carry_cap = 3;

const RegionData = struct {
    tiles: [board_dim * board_dim]u8,
    height_map: [board_dim * board_dim]i16,
};

const World = struct {
    region_data: []RegionData,
    height_map: [board_dim * board_dim]i16,
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

const SWPEntry = struct {
    distance: usize,
    tile_p: @Vector(2, u16),
};

const DrawRotState = enum(u8) {
    rotate_nonce = 0,
    rotate_once,
    rotate_twice,
    rotate_thrice,

    count,
};

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

// Raylib vector math

inline fn toRaylibVector2(comptime T: type, vec: @Vector(2, T)) rl.Vector2 {
    return rl.Vector2{ .x = vec[0], .y = vec[1] };
}

inline fn vector2Subtract(lhs: rl.Vector2, rhs: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = lhs.x - rhs.x,
        .y = lhs.y - rhs.y,
    };
}

inline fn vector2Add(lhs: rl.Vector2, rhs: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = lhs.x + rhs.x,
        .y = lhs.y + rhs.y,
    };
}

inline fn vector2Equals(lhs: rl.Vector2, rhs: rl.Vector2) c_int {
    if (lhs.x == rhs.x and lhs.y == rhs.y)
        return 0;
    return 1;
}

inline fn vector2DistanceU16(v1: @Vector(2, u16), v2: @Vector(2, u16)) f32 {
    const result = @sqrt(@as(f32, @floatFromInt((v1[0] - v2[0]) * (v1[0] - v2[0]) + (v1[1] - v2[1]) * (v1[1] - v2[1]))));
    return result;
}

inline fn matrixVector2Multiply(m: rl.Matrix, p: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = m.m0 * p.x + m.m4 * p.y,
        .y = m.m1 * p.x + m.m5 * p.y,
    };
}

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

// NOTE(caleb): This by itself isn't enough for a 90 board rotation
// I subtract board_dim - 1 to the y component prior to the multiply.
// The de-rotation adds board_dim - 1 after the multiply.
const rotate_once_matrix = Matrix2x2I8{
    .m0 = 0,
    .m1 = 1,
    .m2 = -1,
    .m3 = 0,
};
const derotate_once_matrix = Matrix2x2I8.inverse(rotate_once_matrix);

// NOTE(caleb): If I wanted to save time I could also precompute rotation matrices:
// (and their inverses) twice and thrice.

/// Depending on the rotation of the board, selected_tile_p won't reflect the
/// correct tile in memory this function gives the de-rotated tile_p.
inline fn canonicalTileP(tile_p: @Vector(2, i8), draw_rot_state: DrawRotState) @Vector(2, i8) {
    var result = tile_p;
    for (0..@intFromEnum(draw_rot_state)) |_| {
        result = Matrix2x2I8.vectorMultiply(derotate_once_matrix, result) +
            @Vector(2, i8){ 0, @intCast(board_dim - 1) };
    }
    return result;
}

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
    platform_api: *const platform.PlatformAPI,
    tile_width_px: u16,
    tile_height_px: u16,
) rl.Vector2 {
    return rl.Vector2{
        .x = @as(f32, @floatFromInt(platform_api.getScreenWidth())) / 2.0 - @as(f32, @floatFromInt(tile_width_px)) / 2.0,
        .y = (@as(f32, @floatFromInt(platform_api.getScreenHeight())) - screenSpaceBoardHeight(tile_width_px, tile_height_px)) / 2.0,
    };
}

fn isoInvert(
    platform_api: *const platform.PlatformAPI,
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
    platform_api: *const platform.PlatformAPI,
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
    platform_api: *const platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    return vector2Add(isoProj(platform_api, p, tile_width_px, tile_height_px, board_translation), .{
        .x = 0.0, //glyph_size / 2 + glyph_size / 3,
        .y = 0.0,
    });
}

fn drawTile(
    platform_api: *const platform.PlatformAPI,
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
    platform_api: *const platform.PlatformAPI,
    tile_id: u8,
    height_map: []const i16,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: rl.Vector2,
    selected_tile_p: @Vector(2, i8),
    draw_3d: bool,
    scale_factor: f32,
) void {
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

    if (draw_3d) {
        for (0..@intCast(height_map[source_row_index * board_dim + source_col_index] + 1)) |_| {
            drawTile(platform_api, tileset, tile_id, dest_pos, scale_factor, rl.WHITE);
            dest_pos.y -= scaled_tile_dim.y / 2.0;
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

fn drawWorldTileFromCoords(
    platform_api: *const platform.PlatformAPI,
    world: *const World,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: rl.Vector2,
    selected_tile_p: @Vector(2, i8),
    draw_3d: bool,
    scale_factor: f32,
) void {
    const tile_id = world.region_data[source_row_index * board_dim + source_col_index].tiles[0];
    drawTileFromCoords(platform_api, tile_id, &world.height_map, tileset, source_row_index, source_col_index, dest_row_index, dest_col_index, scaled_tile_dim, board_translation, selected_tile_p, draw_3d, scale_factor);
}

fn drawRegionTileFromCoords(
    platform_api: *const platform.PlatformAPI,
    region_data: *const RegionData,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: rl.Vector2,
    selected_tile_p: @Vector(2, i8),
    draw_3d: bool,
    scale_factor: f32,
) void {
    const tile_id = region_data.tiles[source_row_index * board_dim + source_col_index];
    drawTileFromCoords(platform_api, tile_id, &region_data.height_map, tileset, source_row_index, source_col_index, dest_row_index, dest_col_index, scaled_tile_dim, board_translation, selected_tile_p, draw_3d, scale_factor);
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
    // Generate world heightmap
    for (0..board_dim * board_dim) |board_index|
        world.height_map[board_index] = 0;
    world.height_map[0] = rng.intRangeLessThan(i8, 0, max_height); // Top Left
    world.height_map[board_dim - 1] = rng.intRangeLessThan(i8, 0, max_height); // Top right
    world.height_map[(board_dim - 1) * board_dim] = rng.intRangeLessThan(i8, 0, max_height); // Bottom left
    world.height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = rng.intRangeLessThan(i8, 0, max_height); // Bottom right
    genHeightMap(
        &world.height_map,
        @splat(@divTrunc(board_dim, 2)),
        board_dim,
        rng,
        1.0, // NOTE(caleb): Random height scalar lower = rougher terrain
    );

    for (0..board_dim) |world_row_index| {
        for (0..board_dim) |world_col_index| {
            const world_tile_index: u16 = @intCast(world_row_index * board_dim + world_col_index);

            // Regional heightmap
            world.region_data[world_tile_index].height_map[0] = // Top Left
                if (world_row_index > 0 and world_col_index > 0)
                world.height_map[(world_row_index - 1) * board_dim + world_col_index - 1]
            else
                rng.intRangeLessThan(i8, 0, max_height);
            world.region_data[world_tile_index].height_map[board_dim - 1] = // Top right
                if (world_row_index > 0 and world_col_index < board_dim - 1)
                world.height_map[(world_row_index - 1) * board_dim + world_col_index + 1]
            else
                rng.intRangeLessThan(i8, 0, max_height);
            world.region_data[world_tile_index].height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = // Bottom right
                if (world_row_index < board_dim - 1 and world_col_index < board_dim - 1)
                world.height_map[(world_row_index + 1) * board_dim + world_col_index + 1]
            else
                rng.intRangeLessThan(i8, 0, max_height);
            world.region_data[world_tile_index].height_map[(board_dim - 1) * board_dim] = // Bottom left
                if (world_row_index < board_dim - 1 and world_col_index > 0)
                world.height_map[(world_row_index + 1) * board_dim + world_col_index - 1]
            else
                rng.intRangeLessThan(i8, 0, max_height);
            genHeightMap(
                &world.region_data[world_tile_index].height_map,
                @splat(@divTrunc(board_dim, 2)),
                board_dim,
                rng,
                1.0, // NOTE(caleb): Random height scalar lower = rougher terrain
            );

            for (0..board_dim) |region_row_index| {
                for (0..board_dim) |region_col_index| {
                    const region_tile_index: u16 = @intCast(region_row_index * board_dim + region_col_index);
                    const height = world.region_data[world_tile_index].height_map[region_tile_index];
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
                    world.region_data[world_tile_index].tiles[region_tile_index] = @intCast(tile_id);

                    // 1 in 10000 to gen a tree // FIXME(caleb): THIS IS A HACK AND SHOULD BE REPLACED!!
                    if (height >= 5 and rng.uintLessThan(u16, 10000) == 0) {
                        const tree_entity_id = entity_man.newEntity();
                        entity_man.signatures[tree_entity_id] = entity_man.entitySignature(.resource);
                        tile_p_components.add(tree_entity_id, @Vector(2, u16){ @intCast(world_tile_index), region_tile_index });
                        resource_kind_components.add(tree_entity_id, .tree);
                    }
                    // else if (rl.GetRandomValue(0, 10) == 0) { // 1 in 200 for a pile
                    //     const pile_entity_id = entity_man.newEntity();
                    //     inventory_components.add(pile_entity_id, .{});
                    //     tile_p_components.add(pile_entity_id, @Vector(2, u16){ @intCast(world_tile_index), sub_region_tile_index });
                    //     entity_man.signatures[pile_entity_id] = pile_entity_sig;
                    // }
                }
            }
        }
    }
}

inline fn validCoords(coords: @Vector(2, i8)) bool {
    var result = false;
    if (@reduce(.And, coords >= @as(@Vector(2, i8), @splat(0))) and
        @reduce(.And, coords < @as(@Vector(2, i8), @splat(board_dim))))
        result = true;
    return result;
}

export fn spUpdateAndRender(platform_api: *const platform.PlatformAPI, game_state: *platform.GameState) void {
    const perm_ally = game_state.perm_fba.allocator();
    const scratch_ally = game_state.scratch_fba.allocator();

    var entity_man: *ecs.EntityManager = undefined;
    var tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var target_tile_p_components: *ecs.ComponentArray(@Vector(2, u16)) = undefined;
    var resource_kind_components: *ecs.ComponentArray(ResourceKind) = undefined;
    var inventory_components: *ecs.ComponentArray(Inventory) = undefined;
    var worker_state_components: *ecs.ComponentArray(WorkerState) = undefined;
    var tileset: *Tileset = undefined;
    var world: *World = undefined;

    //- cabarger: This is done once during the first game code load
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
            .height_map = undefined,
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
        game_state.debug_draw_tile_height = false;
        game_state.debug_draw_tile_hitboxes = false;

        game_state.is_paused = false;
        game_state.pause_start_time = 0.0;
        game_state.last_tick_time = 0;

        game_state.view_mode = @intFromEnum(ViewMode.world);
        game_state.selected_tile_p = @Vector(2, i8){ -1, -1 };
        game_state.selected_region_p = @Vector(2, u8){ 0, 0 };

        game_state.draw_3d = true;
        game_state.scale_factor = 2.0;

        game_state.board_translation = rl.Vector2{ .x = 0, .y = 0 };

        game_state.draw_rot_state = @intFromEnum(DrawRotState.rotate_nonce);

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

    var tick_granularity: TickGranularity = @enumFromInt(game_state.tick_granularity);
    var view_mode: ViewMode = @enumFromInt(game_state.view_mode);
    var draw_rot_state: DrawRotState = @enumFromInt(game_state.draw_rot_state);

    const mouse_wheel_move = platform_api.getMouseWheelMove();
    if (mouse_wheel_move != 0) {
        game_state.scale_factor += if (mouse_wheel_move == -1) -scale_inc else scale_inc;
        game_state.scale_factor = base_math.clampf32(game_state.scale_factor, 0.25, 10.0);
    }

    const scaled_tile_dim: rl.Vector2 = .{
        .x = @as(f32, @floatFromInt(tileset.tile_width)) * game_state.scale_factor,
        .y = @as(f32, @floatFromInt(tileset.tile_height)) * game_state.scale_factor,
    };

    const new_mouse_p = platform_api.getMousePosition();
    const mouse_moved = (vector2Equals(game_state.mouse_p, new_mouse_p) != 0);
    game_state.mouse_p = new_mouse_p;

    if (mouse_moved) {
        if (game_state.draw_3d) {
            game_state.selected_tile_p = .{ -1, -1 };
            outer: for (0..board_dim) |row_index| {
                for (0..board_dim) |col_index| {
                    const canonical_board_p: @Vector(2, usize) = @intCast(
                        canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), @enumFromInt(game_state.draw_rot_state)),
                    );
                    const height = switch (@as(ViewMode, @enumFromInt(game_state.view_mode))) {
                        .region => world.region_data[game_state.selected_region_p[1] * board_dim + game_state.selected_region_p[0]]
                            .height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                        .world => world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                    };
                    var projected_p = isoProj(
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
                        game_state.selected_tile_p[0] = @intCast(col_index);
                        game_state.selected_tile_p[1] = @intCast(row_index);
                        break :outer;
                    }
                }
            }
        } else {
            const deprojected_mouse_p = isoInvert(
                platform_api,
                vector2Subtract(game_state.mouse_p, .{ .x = scaled_tile_dim.x / 2.0, .y = 0.0 }),
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            game_state.selected_tile_p[0] = @intFromFloat(deprojected_mouse_p.x / scaled_tile_dim.x);
            game_state.selected_tile_p[1] = @intFromFloat(deprojected_mouse_p.y / scaled_tile_dim.y);
        }
    }

    if (platform_api.isMouseButtonDown(rl.MOUSE_BUTTON_RIGHT))
        game_state.board_translation = vector2Add(game_state.board_translation, platform_api.getMouseDelta());

    var key_pressed = platform_api.getKeyPressed();
    while (key_pressed != 0) { //- cabarger: Handle input
        if (key_pressed == rl.KEY_R and platform_api.isKeyDown(rl.KEY_LEFT_SHIFT)) {
            draw_rot_state =
                @enumFromInt(@mod(@as(i8, @intCast(game_state.draw_rot_state)) - 1, @intFromEnum(DrawRotState.count)));
            game_state.draw_rot_state = @intFromEnum(draw_rot_state);
        } else if (key_pressed == rl.KEY_R) {
            draw_rot_state =
                @enumFromInt((game_state.draw_rot_state + 1) % @intFromEnum(DrawRotState.count));
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
                    canonicalTileP(game_state.selected_tile_p, @enumFromInt(game_state.draw_rot_state)),
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
    if (!game_state.is_paused and time_now - game_state.last_tick_time >= tick_rate_sec) {
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
                        canonicalTileP(@intCast(@Vector(2, usize){ dest_col_index, dest_row_index }), draw_rot_state),
                    );
                    drawWorldTileFromCoords(
                        platform_api,
                        world,
                        tileset,
                        source_tile_coords[1],
                        source_tile_coords[0],
                        dest_row_index,
                        dest_col_index,
                        scaled_tile_dim,
                        game_state.board_translation,
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
                        canonicalTileP(@intCast(@Vector(2, usize){ dest_col_index, dest_row_index }), draw_rot_state),
                    );
                    drawRegionTileFromCoords(
                        platform_api,
                        &world.region_data[region_data_index],
                        tileset,
                        source_tile_coords[1],
                        source_tile_coords[0],
                        dest_row_index,
                        dest_col_index,
                        scaled_tile_dim,
                        game_state.board_translation,
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
            const start_p = isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x / 2.0,
                    .y = scaled_tile_dim.y * @as(f32, @floatFromInt(grid_row)) - scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            const end_p = isoProj(
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
            const start_p = isoProj(
                platform_api,
                .{
                    .x = scaled_tile_dim.x * @as(f32, @floatFromInt(grid_col)) + scaled_tile_dim.x / 2.0,
                    .y = -scaled_tile_dim.y / 2.0,
                },
                @intFromFloat(scaled_tile_dim.x),
                @intFromFloat(scaled_tile_dim.y),
                game_state.board_translation,
            );
            const end_p = isoProj(
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
                    canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), @enumFromInt(game_state.draw_rot_state)),
                );
                const height = switch (@as(ViewMode, @enumFromInt(game_state.view_mode))) {
                    .region => world.region_data[game_state.selected_region_p[1] * board_dim + game_state.selected_region_p[0]]
                        .height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                    .world => world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                };
                var projected_p = isoProjGlyph(
                    platform_api,
                    .{
                        .x = @as(f32, @floatFromInt(col_index)) * scaled_tile_dim.x + scaled_tile_dim.x / 2.0 + scaled_tile_dim.x / 3.0,
                        .y = @as(f32, @floatFromInt(row_index)) * scaled_tile_dim.y,
                    },
                    @intFromFloat(scaled_tile_dim.x),
                    @intFromFloat(scaled_tile_dim.y),
                    game_state.board_translation,
                );
                if (game_state.draw_3d)
                    projected_p.y -= @as(f32, @floatFromInt(height)) * (scaled_tile_dim.y / 2.0);
                platform_api.drawTextCodepoint(
                    game_state.rl_font,
                    @intCast(height + '0'),
                    projected_p,
                    glyph_size,
                    .{ .r = 0, .g = 0, .b = 255, .a = 255 },
                );
            }
        }
    }

    if (game_state.debug_draw_tile_hitboxes) {
        for (0..board_dim) |row_index| {
            for (0..board_dim) |col_index| {
                const canonical_board_p: @Vector(2, usize) = @intCast(
                    canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), @enumFromInt(game_state.draw_rot_state)),
                );
                const height = switch (@as(ViewMode, @enumFromInt(game_state.view_mode))) {
                    .region => world.region_data[game_state.selected_region_p[1] * board_dim + game_state.selected_region_p[0]]
                        .height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                    .world => world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]],
                };
                var projected_p = isoProj(
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
                .y = @as(f32, @floatFromInt(strz_index)) * glyph_size,
            }, glyph_size, 1.0, rl.WHITE);
        }
    }
    platform_api.endDrawing();
}
