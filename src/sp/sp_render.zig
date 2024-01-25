//!
//! sp_render.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/25/24
//! barg8397@vandals.uidaho.edu
//!
//! Draw code
//!

const std = @import("std");
const base = @import("base");
const third_party = @import("third_party");

const sp_map = @import("sp_map.zig");
const sp_platform = @import("sp_platform.zig");

const base_math = base.base_math;
const rand = std.rand;
const rl = third_party.rl;

const Tileset = @import("Tileset.zig");
const Matrix2x2 = base_math.Matrix2x2;
const World = sp_map.World;
const RegionData = sp_map.RegionData;
const Random = rand.Random;

const board_dim = sp_map.board_dim;
pub const scale_inc: f32 = 0.25;
pub const glyph_size = 18;

pub const DrawRotState = enum(u8) {
    rotate_nonce = 0,
    rotate_once,
    rotate_twice,
    rotate_thrice,

    count,
};

pub inline fn isoProjMatrix() Matrix2x2(f32) {
    return Matrix2x2(f32){
        .m0 = 0.5,
        .m1 = 0.25,
        .m2 = -0.5,
        .m3 = 0.25,
    };
}

pub const iso_proj_matrix = isoProjMatrix();

pub fn screenSpaceBoardHeight(tile_width_px: u16, tile_height_px: u16) f32 {
    return isoProjMatrix().vectorMultiply(.{
        board_dim * @as(f32, @floatFromInt(tile_width_px)),
        board_dim * @as(f32, @floatFromInt(tile_height_px)),
    })[1] + @as(f32, @floatFromInt(tile_height_px)) / 2.0;
}

pub fn boardOffset(
    platform_api: *const sp_platform.PlatformAPI,
    tile_width_px: u16,
    tile_height_px: u16,
) @Vector(2, f32) {
    return .{
        @as(f32, @floatFromInt(platform_api.getScreenWidth())) / 2.0 - @as(f32, @floatFromInt(tile_width_px)) / 2.0,
        (@as(f32, @floatFromInt(platform_api.getScreenHeight())) - screenSpaceBoardHeight(tile_width_px, tile_height_px)) / 2.0,
    };
}

pub fn isoInvert(
    platform_api: *const sp_platform.PlatformAPI,
    p: @Vector(2, f32),
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: @Vector(2, f32),
) rl.Vector2 {
    const untranslated_p = p - board_translation;
    const unshifted_p = untranslated_p - boardOffset(platform_api, tile_width_px, tile_height_px);
    return @bitCast(iso_proj_matrix.inverse().vectorMultiply(unshifted_p));
}

pub fn isoProj(
    platform_api: *const sp_platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: @Vector(2, f32),
) rl.Vector2 {
    const projected_p = isoProjMatrix().vectorMultiply(@bitCast(p));
    const shifted_p = projected_p + boardOffset(platform_api, tile_width_px, tile_height_px);
    const translated_p = shifted_p + board_translation;
    return @bitCast(translated_p);
}

pub fn isoProjGlyph(
    platform_api: *const sp_platform.PlatformAPI,
    p: rl.Vector2,
    tile_width_px: u16,
    tile_height_px: u16,
    board_translation: @Vector(2, f32),
) rl.Vector2 {
    return @bitCast(@as(@Vector(2, f32), @bitCast(isoProj(platform_api, p, tile_width_px, tile_height_px, board_translation))) + @Vector(2, f32){
        0.0, //glyph_size / 2 + glyph_size / 3,
        0.0,
    });
}

pub fn drawTile(
    platform_api: *const sp_platform.PlatformAPI,
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
pub fn drawTileFromCoords(
    platform_api: *const sp_platform.PlatformAPI,
    tile_id: u8,
    height_map: []const i16,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: @Vector(2, f32),
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

pub fn drawWorldTileFromCoords(
    platform_api: *const sp_platform.PlatformAPI,
    world: *const World,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: @Vector(2, f32),
    selected_tile_p: @Vector(2, i8),
    draw_3d: bool,
    scale_factor: f32,
) void {
    const tile_id = world.region_data[source_row_index * board_dim + source_col_index].tiles[0];
    drawTileFromCoords(platform_api, tile_id, &world.height_map, tileset, source_row_index, source_col_index, dest_row_index, dest_col_index, scaled_tile_dim, board_translation, selected_tile_p, draw_3d, scale_factor);
}

pub fn drawRegionTileFromCoords(
    platform_api: *const sp_platform.PlatformAPI,
    region_data: *const RegionData,
    tileset: *const Tileset,
    source_row_index: usize,
    source_col_index: usize,
    dest_row_index: usize,
    dest_col_index: usize,
    scaled_tile_dim: rl.Vector2,
    board_translation: @Vector(2, f32),
    selected_tile_p: @Vector(2, i8),
    draw_3d: bool,
    scale_factor: f32,
) void {
    const tile_id = region_data.tiles[source_row_index * board_dim + source_col_index];
    drawTileFromCoords(platform_api, tile_id, &region_data.height_map, tileset, source_row_index, source_col_index, dest_row_index, dest_col_index, scaled_tile_dim, board_translation, selected_tile_p, draw_3d, scale_factor);
}
