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

const math = std.math;
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const rand = std.rand;

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

// TODO(caleb): Rename me (this is an enum over all sprites)
const TileType = enum(u8) {
    soil = 0,
    rock,
    water,
    sand,
    tree,
    plains,
    forest,
};

/// Store info about the tileset, and tile properties.
const Tileset = struct {
    columns: u16,
    tile_count: u16,
    tile_width: u16,
    tile_height: u16,

    tile_id_to_type: AutoHashMap(u16, TileType),
    tile_type_to_id: AutoHashMap(TileType, u16),

    texture: rl.Texture,

    pub fn init(
        tileset_fba: *FixedBufferAllocator,
        scratch_fba: *FixedBufferAllocator,
        tsj_path: []const u8,
    ) !Tileset {
        const tileset_ally = tileset_fba.allocator();
        const scratch_ally = scratch_fba.allocator();
        const restore_end_index = scratch_fba.end_index;
        defer scratch_fba.end_index = restore_end_index;

        var raw_ts_json: []u8 = undefined;
        {
            const ts_f = try fs.cwd().openFile(tsj_path, .{});
            defer ts_f.close();
            raw_ts_json =
                try ts_f.reader().readAllAlloc(scratch_ally, 1024 * 1);
        }
        var parsed_ts_data = try std.json.parseFromSliceLeaky(
            std.json.Value,
            scratch_ally,
            raw_ts_json,
            .{},
        );

        const ts_image_pathz =
            try fs.path.joinZ(
            scratch_ally,
            &.{
                fs.path.dirname(tsj_path).?,
                parsed_ts_data.object.get("image").?.string,
            },
        );

        var ts = Tileset{
            .texture = rl.LoadTexture(ts_image_pathz),
            .columns = @intCast(parsed_ts_data.object.get("columns").?.integer),
            .tile_width = @intCast(parsed_ts_data.object.get("tilewidth").?.integer),
            .tile_height = @intCast(parsed_ts_data.object.get("tileheight").?.integer),
            .tile_count = @intCast(parsed_ts_data.object.get("tilecount").?.integer),
            .tile_id_to_type = AutoHashMap(u16, TileType).init(tileset_ally),
            .tile_type_to_id = AutoHashMap(TileType, u16).init(tileset_ally),
        };
        try ts.tile_id_to_type.ensureTotalCapacity(ts.tile_count);
        try ts.tile_type_to_id.ensureTotalCapacity(ts.tile_count);

        // Fill out id <=> type maps
        const tile_data = parsed_ts_data.object.get("tiles").?.array;
        for (tile_data.items) |tile| {
            const tile_id: u16 = @intCast(tile.object.get("id").?.integer);
            const tile_type_str = tile.object.get("type").?.string;
            var tile_type: TileType = undefined;
            if (std.mem.eql(u8, tile_type_str, "rock")) {
                tile_type = .rock;
            } else if (std.mem.eql(u8, tile_type_str, "soil")) {
                tile_type = .soil;
            } else if (std.mem.eql(u8, tile_type_str, "water")) {
                tile_type = .water;
            } else if (std.mem.eql(u8, tile_type_str, "sand")) {
                tile_type = .sand;
            } else if (std.mem.eql(u8, tile_type_str, "tree")) {
                tile_type = .tree;
            } else if (std.mem.eql(u8, tile_type_str, "plains")) {
                tile_type = .plains;
            } else if (std.mem.eql(u8, tile_type_str, "forest")) {
                tile_type = .forest;
            } else {
                std.debug.print("Unhandled tile type: {s}\n", .{tile_type_str});
                unreachable;
            }

            ts.tile_id_to_type.putAssumeCapacity(tile_id, tile_type);
            ts.tile_type_to_id.putAssumeCapacity(tile_type, tile_id);
        }

        return ts;
    }
};

const ComponentKind = enum(u8) {
    tile_p = 0,
    resource_kind,
    inventory,
    worker_state,

    count, // NOTHING BELOW THIS LINE
};

const EntityKind = enum(u8) {
    worker = 0,
    pile,
    resource,

    count, // NOTHING BELOW THIS LINE
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

const EntityManager = struct {
    free_entities: ArrayList(usize),
    signatures: []StaticBitSet(@intFromEnum(ComponentKind.count)),
    signature_table: [@intFromEnum(EntityKind.count)]StaticBitSet(@intFromEnum(ComponentKind.count)),

    pub fn init(
        ally: mem.Allocator,
        entity_count: usize,
    ) !EntityManager {
        var result = EntityManager{
            .free_entities = try ArrayList(usize).initCapacity(ally, entity_count), // TODO(caleb): SinglyLinkedList
            .signatures = try ally.alloc(StaticBitSet(@intFromEnum(ComponentKind.count)), entity_count),
            .signature_table = undefined,
        };
        for (0..entity_count) |free_entity_index|
            result.free_entities.insertAssumeCapacity(free_entity_index, free_entity_index);
        for (result.signatures) |*mask|
            mask.setRangeValue(
                .{
                    .start = 0,
                    .end = @intFromEnum(ComponentKind.count),
                },
                false,
            );

        // Entity signatures
        const worker_sig_index = @intFromEnum(EntityKind.worker);
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.worker_state));
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.tile_p));
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.inventory));

        const pile_sig_index = @intFromEnum(EntityKind.pile);
        result.signature_table[pile_sig_index].set(@intFromEnum(ComponentKind.inventory));
        result.signature_table[pile_sig_index].set(@intFromEnum(ComponentKind.tile_p));

        const resource_sig_index = @intFromEnum(EntityKind.resource);
        result.signature_table[resource_sig_index].set(@intFromEnum(ComponentKind.resource_kind));
        result.signature_table[resource_sig_index].set(@intFromEnum(ComponentKind.tile_p));

        return result;
    }

    pub inline fn entitySignature(entity_man: *const EntityManager, entity_kind: EntityKind) StaticBitSet(@intFromEnum(ComponentKind.count)) {
        return entity_man.signature_table[@intFromEnum(entity_kind)];
    }

    pub fn newEntity(entity_man: *EntityManager) usize {
        assert(entity_man.free_entities.items.len > 0);
        const entity_id = entity_man.free_entities.pop();
        entity_man.signatures[entity_id].setRangeValue(
            .{ .start = 0, .end = @intFromEnum(ComponentKind.count) },
            false,
        );
        return entity_id;
    }

    pub inline fn componentMaskSet(
        entity_man: *EntityManager,
        entity_id: usize,
        kind: ComponentKind,
    ) void {
        entity_man.signatures[entity_id]
            .set(@intFromEnum(kind));
    }

    pub inline fn release(entity_man: *EntityManager, entity_id: usize) void {
        entity_man.free_entities.appendAssumeCapacity(entity_id);
    }

    pub fn hasComponent(
        entity_man: *EntityManager,
        entity_id: usize,
        component_kind: ComponentKind,
    ) bool {
        return entity_man.signatures[entity_id].isSet(@intFromEnum(component_kind));
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
            var result = @This(){
                .data = try ally.alloc(T, entity_count),
                .data_count = 0,
                .entity_to_data = AutoHashMap(usize, usize).init(ally),
                .data_to_entity = AutoHashMap(usize, usize).init(ally),
            };
            try result.entity_to_data.ensureTotalCapacity(@intCast(entity_count));
            try result.data_to_entity.ensureTotalCapacity(@intCast(entity_count));
            return result;
        }

        pub fn add(component_array: *@This(), entity_id: usize, item: T) void {
            component_array.data[component_array.data_count] = item;
            component_array.data_to_entity.putAssumeCapacity(
                component_array.data_count,
                entity_id,
            );
            component_array.entity_to_data.putAssumeCapacity(
                entity_id,
                component_array.data_count,
            );
            component_array.data_count += 1;
        }

        pub fn remove(
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

        pub fn reset(component_array: *@This()) void {
            component_array.entity_to_data.clearRetainingCapacity();
            component_array.data_to_entity.clearRetainingCapacity();
            component_array.data_count = 0;
        }
    };
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

fn boardOffset(tile_width_px: u16, tile_height_px: u16) rl.Vector2 {
    return rl.Vector2{
        .x = @as(f32, @floatFromInt(rl.GetScreenWidth())) / 2.0 - @as(f32, @floatFromInt(tile_width_px)) / 2.0,
        .y = (@as(f32, @floatFromInt(rl.GetScreenHeight())) - screenSpaceBoardHeight(tile_width_px, tile_height_px)) / 2.0,
    };
}

fn isoInvert(
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    const untranslated_p = rl.Vector2Subtract(p, board_translation);
    const unshifted_p = rl.Vector2Subtract(untranslated_p, boardOffset(tile_width_px, tile_height_px));
    const invert_proj_mat = rl.MatrixInvert(isoProjMatrix());
    return matrixVector2Multiply(invert_proj_mat, unshifted_p);
}

fn isoProj(
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    const projected_p = matrixVector2Multiply(isoProjMatrix(), p);
    const shifted_p = rl.Vector2Add(projected_p, boardOffset(tile_width_px, tile_height_px));
    const translated_p = rl.Vector2Add(shifted_p, board_translation);
    return translated_p;
}

fn isoProjGlyph(
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: rl.Vector2,
) rl.Vector2 {
    return rl.Vector2Add(isoProj(p, tile_width_px, tile_height_px, board_translation), .{
        .x = glyph_size / 2 + glyph_size / 3,
        .y = 0,
    });
}

fn drawTile(
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

    rl.DrawTexturePro(tileset.texture, source_rect, dest_rect, .{ .x = 0, .y = 0 }, 0, tint);
}

/// NOTE(Caleb): WTF chill with the params.
fn drawTileFromCoords(
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
            drawTile(tileset, tile_id, dest_pos, scale_factor, rl.WHITE);
        }
    } else {
        drawTile(tileset, tile_id, dest_pos, scale_factor, rl.WHITE);
    }

    // NOTE(caleb): I will  want to draw resoruces again soon, just keeping this code around.
    // if (height >= 5) { // Draw tree
    //     dest_pos.y -= scaled_tile_dim.y / 2.0;
    //     drawTile(&tileset, tree_tile_id, dest_pos, scale_factor, rl.WHITE);
    // }
}

inline fn sumOfInventory(ica: *ComponentArray(Inventory), ica_index: usize) usize {
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
    entity_man: *EntityManager,
    tile_p_components: *ComponentArray(@Vector(2, u16)),
    resource_kind_components: *ComponentArray(ResourceKind),
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

export fn smallPlanetGameCode() void {
    std.debug.print("Test hot code reloading #4\n", .{});
}
