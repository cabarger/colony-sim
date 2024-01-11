//!
//! Tileset.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/10/24
//! barg8397@vandals.uidaho.edu
//!

const std = @import("std");
const rl = @import("rl.zig");

const fs = std.fs;

const AutoHashMap = std.AutoHashMap;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;

const Tileset = @This();

// TODO(caleb): Rename me (this is an enum over all sprites)
pub const TileType = enum(u8) {
    soil = 0,
    rock,
    water,
    sand,
    tree,
    plains,
    forest,
};

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
