//!
//! sp_map.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/25/24
//! barg8397@vandals.uidaho.edu
//!

const std = @import("std");
const base = @import("base");

const ecs = @import("ecs.zig");
const sp_sim = @import("sp_sim.zig");
const sp_render = @import("sp_render.zig");

const base_math = base.base_math;
const rand = std.rand;
const math = std.math;

const Tileset = @import("Tileset.zig");
const ArrayList = std.ArrayList;
const Random = rand.Random;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;

pub const board_dim = 17; //33; // NOTE(caleb): Must = (power of 2) + 1
pub const max_height = 10;

pub const TileId = enum(u8) {
    soil = 0,
    rock,
    water,
    sand,
    tree,
    plains,
    forest,
};

pub const World = struct {
    tiles: [board_dim * board_dim]u8,
    height_map: [board_dim * board_dim]i16,
};

const SWPEntry = struct {
    distance: usize,
    tile_p: @Vector(2, u16),
};

pub fn swpUpdateMap(
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
pub const rotate_once_matrix = base_math.Matrix2x2(i8){
    .m0 = 0,
    .m1 = 1,
    .m2 = -1,
    .m3 = 0,
};
const derotate_once_matrix = base_math.Matrix2x2(i8).inverse(rotate_once_matrix);

// NOTE(caleb): If I wanted to save time I could also precompute rotation matrices:
// (and their inverses) twice and thrice.

/// Depending on the rotation of the board, selected_tile_p won't reflect the
/// correct tile in memory this function gives the de-rotated tile_p.
pub inline fn canonicalTileP(tile_p: @Vector(2, i8), draw_rot_state: sp_render.DrawRotState) @Vector(2, i8) {
    var result = tile_p;
    for (0..@intFromEnum(draw_rot_state)) |_| {
        result = base_math.Matrix2x2(i8).vectorMultiply(derotate_once_matrix, result) +
            @Vector(2, i8){ 0, @intCast(board_dim - 1) };
    }
    return result;
}

pub fn genHeightMap(
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

pub fn genWorld(
    rng: Random,
    tileset: *const Tileset,
    world: *World,
    entity_man: *ecs.EntityManager,
    tile_p_components: *ecs.ComponentArray(@Vector(2, u16)),
    resource_kind_components: *ecs.ComponentArray(sp_sim.ResourceKind),
) !void {
    _ = resource_kind_components;
    _ = tile_p_components;
    _ = entity_man;

    //- cabarger: Generate heightmap
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

    //- cabarger: Assign tile id's based on height
    for (0..board_dim) |row_index| {
        for (0..board_dim) |col_index| {
            const height = world.height_map[row_index * board_dim + col_index];
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
            world.tiles[row_index * board_dim + col_index] = @intCast(tile_id);
        }
    }
}

pub inline fn validCoords(coords: @Vector(2, i8)) bool {
    var result = false;
    if (@reduce(.And, coords >= @as(@Vector(2, i8), @splat(0))) and
        @reduce(.And, coords < @as(@Vector(2, i8), @splat(board_dim))))
        result = true;
    return result;
}
