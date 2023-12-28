//!
//! main.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 12/06/23
//! barg8397@vandals.uidaho.edu
//!

// TODO(caleb):
// - (other colonies)

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

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const assert = std.debug.assert;

const board_dim = 33;
const max_height = 20;
var height_scale: f32 = 0.0;
const scale_factor = 2;
const glyph_size = 30;
const tick_rate_sec = 0.15;

const TileType = enum(u8) {
    grass = 0,
    water,
    sand,
    tree,
};

/// Store info about the tileset, and tile properties.
const Tileset = struct {
    columns: u16,
    tile_count: u16,
    tile_width: u8,
    tile_height: u8,
    tile_id_to_type: AutoHashMap(u16, TileType),
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
        };
        try ts.tile_id_to_type.ensureTotalCapacity(ts.tile_count);

        // Fill out id to type map
        const tile_data = parsed_ts_data.object.get("tiles").?.array;
        for (tile_data.items) |tile| {
            const tile_id: u16 = @intCast(tile.object.get("id").?.integer);
            const tile_type = tile.object.get("type").?.string;
            if (std.mem.eql(u8, tile_type, "grass")) {
                ts.tile_id_to_type.putAssumeCapacity(tile_id, .grass);
            } else if (std.mem.eql(u8, tile_type, "water")) {
                ts.tile_id_to_type.putAssumeCapacity(tile_id, .water);
            } else if (std.mem.eql(u8, tile_type, "sand")) {
                ts.tile_id_to_type.putAssumeCapacity(tile_id, .sand);
            } else if (std.mem.eql(u8, tile_type, "tree")) {
                ts.tile_id_to_type.putAssumeCapacity(tile_id, .tree);
            } else {
                unreachable;
            }
        }

        return ts;
    }
};

const TextureAtlas = struct {
    rows: u16,
    columns: u16,
    tile_indicies: []u16,
    /// NOTE(caleb): Atlas data is offset by first_gid
    first_gid: u16,

    pub fn init(
        tilemap_fba: *FixedBufferAllocator,
        scratch_fba: *FixedBufferAllocator,
        texture_atlas_path: []const u8,
    ) !TextureAtlas {
        const restore_end_index = scratch_fba.end_index;
        defer scratch_fba.end_index = restore_end_index;
        const tilemap_ally = tilemap_fba.allocator();
        const scratch_ally = scratch_fba.allocator();

        var atlas_json_bytes: []u8 = undefined;
        {
            const atlas_f = try fs.cwd().openFile(texture_atlas_path, .{});
            defer atlas_f.close();
            atlas_json_bytes =
                try atlas_f.reader().readAllAlloc(scratch_ally, 1024 * 1);
        }
        var parsed_atlas = try std.json.parseFromSliceLeaky(
            std.json.Value,
            scratch_ally,
            atlas_json_bytes,
            .{},
        );

        const layers = parsed_atlas.object.get("layers").?.array;
        if (layers.items.len == 1) { // Read first layer data
            const first_layer_obj = layers.items[0].object;
            const columns: u16 = @intCast(first_layer_obj.get("width").?.integer);
            const rows: u16 = @intCast(first_layer_obj.get("height").?.integer);
            var tile_indicies = try tilemap_ally.alloc(u16, columns * rows);
            for (first_layer_obj.get("data").?.array.items, 0..) |tile_index, tile_indicies_index|
                tile_indicies[tile_indicies_index] = @intCast(tile_index.integer);

            const tilesets = parsed_atlas.object.get("tilesets").?.array;
            if (tilesets.items.len == 1) { // Read first tileset data
                const first_tileset_obj = tilesets.items[0].object;

                return TextureAtlas{
                    .columns = columns,
                    .rows = rows,
                    .tile_indicies = tile_indicies,
                    .first_gid = @intCast(first_tileset_obj.get("firstgid").?.integer),
                };
            } else {
                unreachable; // Tileset len != 1
            }
        } else {
            unreachable; // Layer len != 1
        }
    }
};

const ComponentKind = enum(u8) {
    tile_p = 0,
    resource_kind,
    inventory,
    worker_state,

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
    berry,
};

const component_count = @intFromEnum(ComponentKind.count);

const EntityManager = struct {
    free_entities: ArrayList(usize),
    signatures: []StaticBitSet(component_count),

    pub fn init(
        ally: mem.Allocator,
        entity_count: usize,
    ) !EntityManager {
        var result = EntityManager{
            .free_entities = try ArrayList(usize).initCapacity(ally, entity_count), // TODO(caleb): SinglyLinkedList
            .signatures = try ally.alloc(StaticBitSet(component_count), entity_count),
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
        return result;
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

    pub inline fn hasComponent(
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
    };
}

const SWPEntry = struct {
    distance: usize,
    tile_p: rl.Vector2,
};

fn swpUpdateMap(
    scratch_fba: *FixedBufferAllocator,
    sample_walk_map: []usize,
    target_p: rl.Vector2,
) !void {
    const restore_end_index = scratch_fba.end_index;
    defer scratch_fba.end_index = restore_end_index;
    const scratch_ally = scratch_fba.allocator();

    var swp_entry_list = try ArrayList(SWPEntry).initCapacity(scratch_ally, board_dim * board_dim);
    swp_entry_list.appendAssumeCapacity(.{ .distance = 0, .tile_p = target_p });

    for (sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

    while (swp_entry_list.items.len > 0) {
        const swp_entry = swp_entry_list.orderedRemove(0);
        sample_walk_map[@intFromFloat(swp_entry.tile_p.y * board_dim + swp_entry.tile_p.x)] = swp_entry.distance;

        // Add neighbors that haven't already been queued. i.e distance isn't max int...
        for (&[_]rl.Vector2{
            rl.Vector2{ .x = 0.0, .y = -1.0 },
            rl.Vector2{ .x = 0.0, .y = 1.0 },
            rl.Vector2{ .x = 1.0, .y = 0.0 },
            rl.Vector2{ .x = -1.0, .y = 0.0 },
        }) |d_tile_coords| {
            const neighbor_tile_p = rl.Vector2Add(swp_entry.tile_p, d_tile_coords);
            if (!(neighbor_tile_p.y >= board_dim or
                neighbor_tile_p.y < 0 or
                neighbor_tile_p.x >= board_dim or
                neighbor_tile_p.x < 0))
            {
                if (sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_dim + neighbor_tile_p.x)] == math.maxInt(usize)) {
                    swp_entry_list.appendAssumeCapacity(.{ .distance = swp_entry.distance + 1, .tile_p = neighbor_tile_p });
                    sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_dim + neighbor_tile_p.x)] = swp_entry.distance + 1;
                }
            }
        }
    }
}

inline fn isoProjMatrix(tile_width_px: u8, tile_height_px: u8) rl.Matrix {
    var result = mem.zeroes(rl.Matrix);
    result.m0 = 0.5 * @as(f32, @floatFromInt(tile_width_px));
    result.m1 = 0.25 * @as(f32, @floatFromInt(tile_height_px));
    result.m4 = -0.5 * @as(f32, @floatFromInt(tile_width_px));
    result.m5 = 0.25 * @as(f32, @floatFromInt(tile_height_px));
    result.m10 = 1;
    result.m15 = 1;
    return result;
}

/// [m0 m4] [x] m0*x + m4*y
/// [m1 m5] [y] m1*x + m5*y
inline fn matrixVector2Multiply(m: rl.Matrix, p: rl.Vector2) rl.Vector2 {
    return rl.Vector2{
        .x = m.m0 * p.x + m.m4 * p.y,
        .y = m.m1 * p.x + m.m5 * p.y,
    };
}

fn isoProj(p: rl.Vector2, tile_width_px: u8, tile_height_px: u8) rl.Vector2 {
    return rl.Vector2Add(matrixVector2Multiply(isoProjMatrix(tile_width_px, tile_height_px), p), .{
        .x = @as(f32, @floatFromInt(rl.GetScreenWidth())) / 2.0,
        .y = (@as(f32, @floatFromInt(rl.GetScreenHeight())) - screenSpaceBoardHeight(tile_width_px, tile_height_px)) / 2.0,
    });
}

fn isoProjGlyph(p: rl.Vector2, tile_width_px: u8, tile_height_px: u8) rl.Vector2 {
    return rl.Vector2Add(isoProj(p, tile_width_px, tile_height_px), .{
        .x = glyph_size / 2 + glyph_size / 3,
        .y = 0,
    });
}

fn screenSpaceBoardHeight(tile_width_px: u8, tile_height_px: u8) f32 {
    return matrixVector2Multiply(
        isoProjMatrix(tile_width_px, tile_height_px),
        .{ .x = board_dim, .y = board_dim },
    ).y + @as(f32, @floatFromInt(tile_height_px)) / 2.0;
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

    rl.DrawTexturePro(
        tileset.texture,
        source_rect,
        dest_rect,
        .{ .x = 0, .y = 0 },
        0,
        tint,
    );
}

inline fn sumOfInventory(ica: *ComponentArray(Inventory), ica_index: usize) usize {
    var result: usize = 0;
    result += ica.data[ica_index].rock_count;
    result += ica.data[ica_index].stick_count;
    result += ica.data[ica_index].berry_count;
    return result;
}

fn genHeightMap(
    board_height_map: []i16,
    point: @Vector(2, u16),
    curr_dim: u16,
) void {
    if (curr_dim == 1) // Base case
        return;

    const half_dim = @divTrunc(curr_dim, 2);
    const half_half_dim = @divTrunc(half_dim, 2);

    // Diamond step
    const top_left_height = board_height_map[(point[1] - half_dim) * board_dim + (point[0] - half_dim)];
    const top_right_height = board_height_map[(point[1] - half_dim) * board_dim + (point[0] + half_dim)];
    const bottom_left_height = board_height_map[(point[1] + half_dim) * board_dim + (point[0] - half_dim)];
    const bottom_right_height = board_height_map[(point[1] + half_dim) * board_dim + (point[0] + half_dim)];
    board_height_map[point[1] * board_dim + point[0]] = @divTrunc((top_left_height + top_right_height + bottom_left_height + bottom_right_height), 4);

    // Square step
    const left_point = @Vector(2, u16){ point[0] - half_dim, point[1] };
    board_height_map[left_point[1] * board_dim + left_point[0]] = @divTrunc((top_left_height + bottom_left_height + board_height_map[point[1] * board_dim + point[0]]), 3);
    const top_point = @Vector(2, u16){ point[0], point[1] - half_dim };
    board_height_map[top_point[1] * board_dim + top_point[0]] = @divTrunc((top_left_height + top_right_height + board_height_map[point[1] * board_dim + point[0]]), 3);
    const right_point = @Vector(2, u16){ point[0] + half_dim, point[1] };
    board_height_map[right_point[1] * board_dim + right_point[0]] = @divTrunc((top_right_height + bottom_right_height + board_height_map[point[1] * board_dim + point[0]]), 3);
    const bottom_point = @Vector(2, u16){ point[0], point[1] + half_dim };
    board_height_map[bottom_point[1] * board_dim + bottom_point[0]] = @divTrunc((top_right_height + bottom_right_height + board_height_map[point[1] * board_dim + point[0]]), 3);

    genHeightMap(board_height_map, @Vector(2, u16){ point[0] - half_half_dim, point[1] - half_half_dim }, half_dim); // Top left
    genHeightMap(board_height_map, @Vector(2, u16){ point[0] + half_half_dim, point[1] - half_half_dim }, half_dim); // Top right
    genHeightMap(board_height_map, @Vector(2, u16){ point[0] - half_half_dim, point[1] + half_half_dim }, half_dim); // Bottom left
    genHeightMap(board_height_map, @Vector(2, u16){ point[0] + half_half_dim, point[1] + half_half_dim }, half_dim); // Bottom right
}

pub fn main() !void {
    ////////////////////////////////////
    // raylib init
    rl.InitWindow(1600, 1200, "small-planet");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();
    const rl_font = rl.GetFontDefault();

    ////////////////////////////////////
    // Allocator setup
    var scratch_mem = std.heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable;
    var scratch_fba = FixedBufferAllocator.init(scratch_mem);

    var perm_mem = std.heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable;
    var perm_fba = FixedBufferAllocator.init(perm_mem);

    ////////////////////////////////////
    // ECS stuff
    const max_entity_count = 2000;
    var entity_man = try EntityManager.init(
        perm_fba.allocator(),
        max_entity_count,
    );

    var tile_p_components =
        try ComponentArray(rl.Vector2).init(
        perm_fba.allocator(),
        max_entity_count,
    );

    var target_tile_p_components =
        try ComponentArray(rl.Vector2).init(
        perm_fba.allocator(),
        max_entity_count,
    );

    var resource_kind_components =
        try ComponentArray(ResourceKind).init(
        perm_fba.allocator(),
        max_entity_count,
    );

    var inventory_components =
        try ComponentArray(Inventory).init(
        perm_fba.allocator(),
        max_entity_count,
    );

    var worker_state_components =
        try ComponentArray(WorkerState).init(
        perm_fba.allocator(),
        max_entity_count,
    );

    // Entity signatures
    var worker_entity_sig = StaticBitSet(component_count).initEmpty();
    worker_entity_sig.set(@intFromEnum(ComponentKind.worker_state));
    worker_entity_sig.set(@intFromEnum(ComponentKind.tile_p));
    worker_entity_sig.set(@intFromEnum(ComponentKind.inventory));

    var pile_entity_sig = StaticBitSet(component_count).initEmpty();
    pile_entity_sig.set(@intFromEnum(ComponentKind.inventory));
    pile_entity_sig.set(@intFromEnum(ComponentKind.tile_p));

    var resource_entity_sig = StaticBitSet(component_count).initEmpty();
    resource_entity_sig.set(@intFromEnum(ComponentKind.resource_kind));
    resource_entity_sig.set(@intFromEnum(ComponentKind.tile_p));

    ////////////////////////////////////
    // Load tileset
    const tileset = try Tileset.init(&perm_fba, &scratch_fba, "./assets/art/small-planet.tsj");
    var grass_tile_id: u16 = undefined;
    var water_tile_id: u16 = undefined;
    var sand_tile_id: u16 = undefined;
    var tree_tile_id: u16 = undefined;
    var id_to_type_iter = tileset.tile_id_to_type.iterator();
    while (id_to_type_iter.next()) |id_to_type_entry| {
        switch (id_to_type_entry.value_ptr.*) {
            .grass => grass_tile_id = id_to_type_entry.key_ptr.*,
            .water => water_tile_id = id_to_type_entry.key_ptr.*,
            .sand => sand_tile_id = id_to_type_entry.key_ptr.*,
            .tree => tree_tile_id = id_to_type_entry.key_ptr.*,
        }
    }

    ////////////////////////////////////

    var rng_seed: u64 = 0;
    var rng = std.rand.DefaultPrng.init(rng_seed);

    ////////////////////////////////////
    // NOTE(caleb): Dirty board *gen* code
    var board_height_map =
        try perm_fba.allocator().alloc(i16, board_dim * board_dim);
    for (0..board_dim * board_dim) |board_index|
        board_height_map[board_index] = 0;

    board_height_map[0] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Top Left
    board_height_map[board_dim - 1] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Top right
    board_height_map[(board_dim - 1) * board_dim] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom left
    board_height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom right
    genHeightMap(board_height_map, @Vector(2, u16){
        @divTrunc(board_dim, 2),
        @divTrunc(board_dim, 2),
    }, board_dim);

    // TODO(caleb): World chunks. Sim regions...
    // var board: [board_dim * board_dim]Tile = undefined;
    for (0..board_dim) |board_row_index| {
        for (0..board_dim) |board_col_index| {
            // 1 in 10 to gen a rock
            if (rl.GetRandomValue(0, 10) == 0) {
                const rock_entity_id = entity_man.newEntity();
                tile_p_components.add(rock_entity_id, .{
                    .x = @floatFromInt(board_col_index),
                    .y = @floatFromInt(board_row_index),
                });
                resource_kind_components.add(rock_entity_id, .rock);
                entity_man.signatures[rock_entity_id] = resource_entity_sig;
            } else if (rl.GetRandomValue(0, 10) == 0) { // 1 in 10 for a stick
                const stick_entity_id = entity_man.newEntity();
                tile_p_components.add(stick_entity_id, .{
                    .x = @floatFromInt(board_col_index),
                    .y = @floatFromInt(board_row_index),
                });
                resource_kind_components.add(stick_entity_id, .stick);
                entity_man.signatures[stick_entity_id] = resource_entity_sig;
            } else if (rl.GetRandomValue(0, 10) == 0) { // 1 in 10 for a berry
                const berry_entity_id = entity_man.newEntity();
                tile_p_components.add(berry_entity_id, .{
                    .x = @floatFromInt(board_col_index),
                    .y = @floatFromInt(board_row_index),
                });
                resource_kind_components.add(berry_entity_id, .berry);
                entity_man.signatures[berry_entity_id] = resource_entity_sig;
            } else if (rl.GetRandomValue(0, 200) == 0) { // 1 in 200 for a pile
                const pile_entity_id = entity_man.newEntity();
                inventory_components.add(pile_entity_id, .{});
                tile_p_components.add(pile_entity_id, .{
                    .x = @floatFromInt(board_col_index),
                    .y = @floatFromInt(board_row_index),
                });
                entity_man.signatures[pile_entity_id] = pile_entity_sig;
            }
        }
    }

    for (0..3) |_| { // Worker entity
        const worker_entity_id = entity_man.newEntity();
        worker_state_components.add(worker_entity_id, .choose_new_target);
        tile_p_components.add(worker_entity_id, .{
            .x = @floatFromInt(board_dim / 2),
            .y = @floatFromInt(board_dim / 2),
        });
        target_tile_p_components.add(worker_entity_id, .{ .x = 0.0, .y = 0.0 });
        inventory_components.add(worker_entity_id, .{});
        entity_man.signatures[worker_entity_id] = worker_entity_sig;
    }

    const worker_carry_cap = 3;
    var selected_tile_tile_p = rl.Vector2{ .x = 0.0, .y = 0.0 };

    var sample_walk_map: [board_dim * board_dim]usize = undefined;
    for (&sample_walk_map) |*distance|
        distance.* = math.maxInt(usize);

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
                height_scale += 0.025;
                selected_tile_tile_p.y -= 1;
            } else if (key_pressed == rl.KEY_DOWN) {
                selected_tile_tile_p.y += 1;
                height_scale -= 0.025;
            } else if (key_pressed == rl.KEY_LEFT) {
                rng_seed -= 1;
                std.debug.print("SEED: {d}\n", .{rng_seed});
                rng = std.rand.DefaultPrng.init(rng_seed);

                board_height_map[0] = 0; //rng.random().intRangeLessThan(i16, -max_height, max_height); // Top Left
                board_height_map[board_dim - 1] = 1; //rng.random().intRangeLessThan(i16, -max_height, max_height); // Top right
                board_height_map[(board_dim - 1) * board_dim] = 0; //rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom left
                board_height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = 0; //rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom right

                genHeightMap(board_height_map, @Vector(2, u16){
                    @divTrunc(board_dim, 2),
                    @divTrunc(board_dim, 2),
                }, board_dim);
                selected_tile_tile_p.x -= 1;
            } else if (key_pressed == rl.KEY_RIGHT) {
                rng_seed += 1;
                std.debug.print("SEED: {d}\n", .{rng_seed});
                rng = std.rand.DefaultPrng.init(rng_seed);

                board_height_map[0] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Top Left
                board_height_map[board_dim - 1] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Top right
                board_height_map[(board_dim - 1) * board_dim] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom left
                board_height_map[(board_dim - 1) * board_dim + (board_dim - 1)] = rng.random().intRangeLessThan(i16, -max_height, max_height); // Bottom right

                genHeightMap(board_height_map, @Vector(2, u16){
                    @divTrunc(board_dim, 2),
                    @divTrunc(board_dim, 2),
                }, board_dim);
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
                        // NOTE(caleb): I think the worker should know what it is doing...
                        // For now just have specific behaviors for the current tile.
                        if (sumOfInventory(&inventory_components, worker_inventory_index) > 0) {
                            for (0..inventory_components.data_count) |ic_index| {
                                const entity_id = inventory_components.data_to_entity
                                    .get(ic_index) orelse unreachable;
                                if (entity_man.signatures[entity_id].eql(pile_entity_sig)) { // Filter for piles
                                    const storage_tile_p_index = tile_p_components.entity_to_data
                                        .get(entity_id) orelse unreachable;
                                    if (rl.Vector2Equals(
                                        tile_p_components.data[worker_tile_p_index],
                                        tile_p_components.data[storage_tile_p_index],
                                    ) != 0) {
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
                            if (entity_man.signatures[entity_id].eql(pile_entity_sig)) {
                                piles_on_board = true;
                                break;
                            }
                        }

                        if ((piles_on_board and sumOfInventory(&inventory_components, worker_inventory_index) >= worker_carry_cap) or
                            (piles_on_board and sumOfInventory(&inventory_components, worker_inventory_index) > 0 and !resources_on_board))
                        {
                            var closest_pile_d: f32 = math.floatMax(f32);
                            for (0..inventory_components.data_count) |ic_index| {
                                const entity_id = inventory_components.data_to_entity
                                    .get(ic_index) orelse unreachable;
                                if (entity_man.signatures[entity_id].eql(pile_entity_sig)) { // Filter for piles
                                    const storage_tile_p_index =
                                        tile_p_components.entity_to_data.get(entity_id) orelse unreachable;
                                    const storage_tile_p = tile_p_components.data[storage_tile_p_index];
                                    if (rl.Vector2Distance(tile_p_components.data[worker_tile_p_index], storage_tile_p) < closest_pile_d) {
                                        closest_pile_d = rl.Vector2Distance(tile_p_components.data[worker_tile_p_index], storage_tile_p);
                                        target_tile_p_components.data[worker_target_tile_p_index] = storage_tile_p;
                                    }
                                }
                            }
                            worker_state_components.data[wsc_index] = .pathing_to_target;
                        } else if (resources_on_board and
                            (sumOfInventory(&inventory_components, worker_inventory_index) < worker_carry_cap))
                        {
                            var closest_resource_d: f32 = math.floatMax(f32);
                            for (0..resource_kind_components.data_count) |rk_component_index| {
                                const entity_id = resource_kind_components.data_to_entity
                                    .get(rk_component_index) orelse unreachable;
                                const tile_p_index = tile_p_components.entity_to_data
                                    .get(entity_id) orelse unreachable;
                                const tile_p = tile_p_components.data[tile_p_index];

                                if (rl.Vector2Distance(tile_p_components.data[worker_tile_p_index], tile_p) < closest_resource_d) {
                                    closest_resource_d = rl.Vector2Distance(
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
                        try swpUpdateMap(
                            &perm_fba,
                            &sample_walk_map,
                            target_tile_p_components.data[worker_target_tile_p_index],
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
                            const neighbor_tile_p = rl.Vector2Add(tile_p_components.data[worker_tile_p_index], d_tile_coords);
                            if (!(neighbor_tile_p.y >= board_dim or
                                neighbor_tile_p.y < 0 or
                                neighbor_tile_p.x >= board_dim or
                                neighbor_tile_p.x < 0))
                            {
                                if (sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_dim + neighbor_tile_p.x)] < closest_tile_distance) {
                                    closest_tile_distance = sample_walk_map[@intFromFloat(neighbor_tile_p.y * board_dim + neighbor_tile_p.x)];
                                    next_worker_tile_p = neighbor_tile_p;
                                }
                            }
                        }
                        tile_p_components.data[worker_tile_p_index] = next_worker_tile_p;
                        if (rl.Vector2Equals(
                            tile_p_components.data[worker_tile_p_index],
                            target_tile_p_components.data[worker_target_tile_p_index],
                        ) != 0) {
                            for (0..tile_p_components.data_count) |tp_component_index| {
                                if ((rl.Vector2Equals(
                                    tile_p_components.data[worker_tile_p_index],
                                    tile_p_components.data[tp_component_index],
                                ) != 0)) {
                                    const entity_id = tile_p_components.data_to_entity
                                        .get(tp_component_index) orelse unreachable;
                                    if (entity_man.signatures[entity_id].eql(resource_entity_sig)) {
                                        const rkc_index = resource_kind_components.entity_to_data
                                            .get(entity_id) orelse unreachable;
                                        switch (resource_kind_components.data[rkc_index]) {
                                            .rock => inventory_components.data[worker_inventory_index].rock_count += 1,
                                            .stick => inventory_components.data[worker_inventory_index].stick_count += 1,
                                            .berry => inventory_components.data[worker_inventory_index].berry_count += 1,
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
            // if (game_time_hour < 7 or game_time_hour >= 23) {
            //     worker.state = .idle;
            // } else if (worker.state == .idle) { // NOTE(caleb): worker should go back to what it was doing.
            //     worker.state = .choose_new_target;
            // }

            last_tick_time = rl.GetTime();
        }

        rl.BeginDrawing();
        rl.ClearBackground(.{ .r = 0, .g = 0, .b = 0, .a = 255 });

        // Draw board
        for (0..board_dim) |row_index| {
            for (0..board_dim) |col_index| {
                const height = board_height_map[row_index * board_dim + col_index];
                var dest_pos = isoProj(
                    .{
                        .x = @floatFromInt(col_index),
                        .y = @floatFromInt(row_index),
                    },
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                var tile_id: u16 = undefined;
                if (height > 0) {
                    dest_pos.y -= (@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor / 2) * @as(f32, @floatFromInt(height)) * height_scale;
                    if (height == 1) {
                        tile_id = sand_tile_id;
                    } else {
                        tile_id = grass_tile_id;
                    }
                } else {
                    tile_id = water_tile_id;
                }
                drawTile(
                    &tileset,
                    tile_id,
                    dest_pos,
                    scale_factor,
                    rl.WHITE,
                );

                if (height >= 5) { // Draw tree
                    dest_pos.y -= (@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor / 2);
                    drawTile(
                        &tileset,
                        tree_tile_id,
                        dest_pos,
                        scale_factor,
                        rl.WHITE,
                    );
                }
            }
        }

        if (debug_draw_grid_lines) {
            // NOTE(caleb): + 1 for outer grid lines
            for (0..board_dim + 1) |grid_row| {
                const start_p = isoProj(
                    .{
                        .x = 0,
                        .y = @as(f32, @floatFromInt(grid_row)),
                    },
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                const end_p = isoProj(
                    .{
                        .x = board_dim,
                        .y = @as(f32, @floatFromInt(grid_row)),
                    },
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                rl.DrawLineEx(start_p, end_p, 1, rl.RED);
            }
            for (0..board_dim + 1) |grid_col| {
                const start_p = isoProj(
                    .{
                        .x = @floatFromInt(grid_col),
                        .y = 0.0,
                    },
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                const end_p = isoProj(
                    .{
                        .x = @as(f32, @floatFromInt(grid_col)),
                        .y = @as(f32, @floatFromInt(board_dim)),
                    },
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                rl.DrawLineEx(start_p, end_p, 1, rl.RED);
            }
        }

        // Draw distance map
        if (debug_draw_distance_map) {
            for (0..1) |wsc_index| { // NOTE(caleb): Make multi maps look ok??
                const worker_id =
                    worker_state_components.data_to_entity.get(wsc_index) orelse unreachable;
                const worker_target_tile_p_index = target_tile_p_components.entity_to_data
                    .get(worker_id) orelse unreachable;
                try swpUpdateMap(
                    &scratch_fba,
                    &sample_walk_map,
                    target_tile_p_components.data[worker_target_tile_p_index],
                );

                for (0..board_dim) |grid_row_index| {
                    for (0..board_dim) |grid_col_index| {
                        if (sample_walk_map[grid_row_index * board_dim + grid_col_index] < 10) {
                            const tile_distance =
                                sample_walk_map[grid_row_index * board_dim + grid_col_index];

                            const is_selected_tile = rl.Vector2Equals(.{
                                .x = @floatFromInt(grid_col_index),
                                .y = @floatFromInt(grid_row_index),
                            }, selected_tile_tile_p) != 0;

                            const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                            var projected_p = isoProjGlyph(
                                .{
                                    .x = @as(f32, @floatFromInt(grid_col_index)),
                                    .y = @as(f32, @floatFromInt(grid_row_index)),
                                },
                                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                                @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                            );
                            projected_p.y += y_offset;
                            rl.DrawTextCodepoint(
                                rl_font,
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
                const is_selected_tile = rl.Vector2Equals(tile_p_components.data[tpc_index], selected_tile_tile_p) != 0;
                const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                var projected_p = isoProjGlyph(
                    tile_p_components.data[tpc_index],
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                projected_p.y += y_offset;

                const entity_id = tile_p_components.data_to_entity.get(tpc_index) orelse unreachable;
                if (entity_man.hasComponent(entity_id, .inventory) and !entity_man.hasComponent(entity_id, .worker_state)) {
                    rl.DrawTextCodepoint(
                        rl_font,
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
                            rl.DrawTextCodepoint(
                                rl_font,
                                @intCast('r'),
                                projected_p,
                                glyph_size,
                                if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                            );
                        },
                        .stick => {
                            rl.DrawTextCodepoint(
                                rl_font,
                                @intCast('s'),
                                projected_p,
                                glyph_size,
                                if (is_selected_tile) rl.WHITE else .{ .r = 200, .g = 200, .b = 200, .a = 255 },
                            );
                        },
                        .berry => {
                            rl.DrawTextCodepoint(
                                rl_font,
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
                const is_selected_tile = rl.Vector2Equals(tile_p_components.data[worker_tile_p_index], selected_tile_tile_p) != 0;
                const y_offset: f32 = if (is_selected_tile) -10.0 else 0.0;
                var projected_p = isoProjGlyph(
                    tile_p_components.data[worker_tile_p_index],
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_width)) * scale_factor),
                    @intFromFloat(@as(f32, @floatFromInt(tileset.tile_height)) * scale_factor),
                );
                projected_p.y += y_offset;
                rl.DrawTextCodepoint(rl_font, @intCast('@'), projected_p, glyph_size, rl.ORANGE);
            }
        }

        // Info about selected tile
        {
            var offset_y: f32 = 0.0;
            for (0..tile_p_components.data_count) |tpc_index| {
                if (rl.Vector2Equals(selected_tile_tile_p, tile_p_components.data[tpc_index]) != 0) {
                    const restore_end_index = scratch_fba.end_index;
                    defer scratch_fba.end_index = restore_end_index;
                    const scratch_ally = scratch_fba.allocator();

                    const entity_id = tile_p_components.data_to_entity
                        .get(tpc_index) orelse unreachable;

                    var infoz: [:0]const u8 = undefined;
                    if (entity_man.signatures[entity_id].eql(worker_entity_sig)) {
                        infoz = try fmt.allocPrintZ(
                            scratch_ally,
                            "WORKER",
                            .{},
                        );
                    } else if (entity_man.signatures[entity_id].eql(pile_entity_sig)) {
                        infoz = try fmt.allocPrintZ(
                            scratch_ally,
                            "PILE",
                            .{},
                        );
                    } else if (entity_man.signatures[entity_id].eql(resource_entity_sig)) {
                        infoz = try fmt.allocPrintZ(
                            scratch_ally,
                            "RESOURCE",
                            .{},
                        );
                    }
                    rl.DrawTextEx(rl_font, infoz, .{
                        .x = 0.0,
                        .y = @as(f32, @floatFromInt(rl.GetScreenHeight() - @divFloor(
                            rl.GetScreenHeight(),
                            5,
                        ))) + offset_y,
                    }, glyph_size, 1.0, rl.WHITE);
                    offset_y += 40.0;
                }
            }
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
            const restore_end_index = scratch_fba.end_index;
            defer scratch_fba.end_index = restore_end_index;
            const scratch_ally = scratch_fba.allocator();

            const game_timez = try fmt.allocPrintZ(
                scratch_ally,
                "Time: {d}::{d}::{d}",
                .{ game_time_day, game_time_hour, game_time_minute },
            );
            const entity_countz = try fmt.allocPrintZ(
                scratch_ally,
                "Entity count: {d}",
                .{entity_man.free_entities.capacity - entity_man.free_entities.items.len},
            );
            const resource_countz = try fmt.allocPrintZ(
                scratch_ally,
                "Resource count: {d}",
                .{resource_kind_components.data_count},
            );
            for (&[_][:0]const u8{
                game_timez,
                resource_countz,
                entity_countz,
            }, 0..) |strz, strz_index| {
                rl.DrawTextEx(rl_font, strz, .{
                    .x = 0, //@as(f32, @floatFromInt(board_dim)) * tile_width_px,
                    .y = @as(f32, @floatFromInt(strz_index)) * 40,
                }, glyph_size, 1.0, rl.WHITE);
            }
        }

        rl.EndDrawing();
    }

    rl.CloseWindow();
}
