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
const sp_sim = @import("sp_sim.zig");
const ecs = @import("ecs.zig");

const base_math = base.base_math;
const rand = std.rand;
const rl = third_party.rl;
const fmt = std.fmt;

const Tileset = @import("Tileset.zig");
const Mat3x3 = base_math.Mat3x3;
const World = sp_map.World;
const RegionData = sp_map.RegionData;
const Random = rand.Random;

const board_dim = sp_map.board_dim;
pub const scale_inc: f32 = 0.25;
pub const glyph_size = 28;

pub const DrawRotState = enum(u8) {
    rotate_nonce = 0,
    rotate_once,
    rotate_twice,
    rotate_thrice,

    count,
};

pub const DrawInfo = struct {
    scaled_tile_dim: @Vector(2, f32),
    draw_rot_state: DrawRotState,
    board_translation: @Vector(2, f32),
    draw_3d: bool,
    scale_factor: f32,
};

pub inline fn isoProjMatrix(
    platform_api: *const sp_platform.PlatformAPI,
    tile_width_px: u16,
    tile_height_px: u16,
) Mat3x3(f32) {
    var offset_p = boardOffset(platform_api, tile_width_px, tile_height_px);

    var result: Mat3x3(f32) = undefined;
    result.m[0] = 0.5;
    result.m[1] = 0.25;
    result.m[2] = 0.0;
    result.m[3] = -0.5;
    result.m[4] = 0.25;
    result.m[5] = 0.0;
    result.m[6] = offset_x;
    result.m[7] = offset_y;
    result.m[8] = 1.0;
    return result;
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

pub fn drawBoard(
    draw_info: *const DrawInfo,
    platform_api: *const sp_platform.PlatformAPI,
    world: *sp_map.World,
    tileset: *const Tileset,
    selected_tile_p: @Vector(2, i8),
) void {
    for (0..board_dim) |dest_row_index| {
        for (0..board_dim) |dest_col_index| {
            var source_tile_coords: @Vector(2, usize) = @intCast(
                sp_map.canonicalTileP(
                    @intCast(@Vector(2, usize){ dest_col_index, dest_row_index }),
                    draw_info.draw_rot_state,
                ),
            );
            var dest_pos = isoProj(
                platform_api,
                .{
                    .x = @as(f32, @floatFromInt(dest_col_index)) * draw_info.scaled_tile_dim[0],
                    .y = @as(f32, @floatFromInt(dest_row_index)) * draw_info.scaled_tile_dim[1],
                },
                @intFromFloat(draw_info.scaled_tile_dim[0]),
                @intFromFloat(draw_info.scaled_tile_dim[1]),
                draw_info.board_translation,
            );

            // Shift selected tile up
            if (@reduce(.And, selected_tile_p == @Vector(2, i32){ @intCast(dest_col_index), @intCast(dest_row_index) }))
                dest_pos.y -= (draw_info.scaled_tile_dim[1] / 2.0) * 0.25;

            if (draw_info.draw_3d) {
                for (0..@intCast(world.height_map[source_tile_coords[1] * board_dim + source_tile_coords[0]] + 1)) |_| {
                    drawTile(platform_api, tileset, world.tiles[source_tile_coords[1] * board_dim + source_tile_coords[0]], dest_pos, draw_info.scale_factor, rl.WHITE);
                    dest_pos.y -= draw_info.scaled_tile_dim[1] / 2.0;
                }
            } else {
                drawTile(platform_api, tileset, world.tiles[source_tile_coords[1] * board_dim + source_tile_coords[0]], dest_pos, draw_info.scale_factor, rl.WHITE);
            }
        }
    }
}

pub fn debugDrawGridLines(
    draw_info: *const DrawInfo,
    platform_api: *const sp_platform.PlatformAPI,
) void {
    // NOTE(caleb): + 1 for outer grid lines
    for (0..board_dim + 1) |grid_row| {
        const start_p = isoProj(
            platform_api,
            .{
                .x = draw_info.scaled_tile_dim[0] / 2.0,
                .y = draw_info.scaled_tile_dim[1] * @as(f32, @floatFromInt(grid_row)) - draw_info.scaled_tile_dim[1] / 2.0,
            },
            @intFromFloat(draw_info.scaled_tile_dim[0]),
            @intFromFloat(draw_info.scaled_tile_dim[1]),
            draw_info.board_translation,
        );
        const end_p = isoProj(
            platform_api,
            .{
                .x = draw_info.scaled_tile_dim[0] * @as(f32, @floatFromInt(board_dim)) + draw_info.scaled_tile_dim[0] / 2.0,
                .y = draw_info.scaled_tile_dim[1] * @as(f32, @floatFromInt(grid_row)) - draw_info.scaled_tile_dim[1] / 2.0,
            },
            @intFromFloat(draw_info.scaled_tile_dim[0]),
            @intFromFloat(draw_info.scaled_tile_dim[1]),
            draw_info.board_translation,
        );
        platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
    }
    for (0..board_dim + 1) |grid_col| {
        const start_p = isoProj(
            platform_api,
            .{
                .x = draw_info.scaled_tile_dim[0] * @as(f32, @floatFromInt(grid_col)) + draw_info.scaled_tile_dim[0] / 2.0,
                .y = -draw_info.scaled_tile_dim[1] / 2.0,
            },
            @intFromFloat(draw_info.scaled_tile_dim[0]),
            @intFromFloat(draw_info.scaled_tile_dim[1]),
            draw_info.board_translation,
        );
        const end_p = isoProj(
            platform_api,
            .{
                .x = draw_info.scaled_tile_dim[0] * @as(f32, @floatFromInt(grid_col)) + draw_info.scaled_tile_dim[0] / 2.0,
                .y = draw_info.scaled_tile_dim[1] * @as(f32, @floatFromInt(board_dim)) - draw_info.scaled_tile_dim[1] / 2.0,
            },
            @intFromFloat(draw_info.scaled_tile_dim[0]),
            @intFromFloat(draw_info.scaled_tile_dim[1]),
            draw_info.board_translation,
        );
        platform_api.drawLineEx(start_p, end_p, 1, rl.RED);
    }
}

pub fn debugDrawTileHeights(
    draw_info: *const DrawInfo,
    platform_api: *const sp_platform.PlatformAPI,
    world: *sp_map.World,
    font: rl.Font,
) void {
    for (0..board_dim) |row_index| {
        for (0..board_dim) |col_index| {
            const canonical_board_p: @Vector(2, usize) = @intCast(
                sp_map.canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), draw_info.draw_rot_state),
            );
            const height = world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]];
            var projected_p = isoProjGlyph(
                platform_api,
                .{
                    .x = @as(f32, @floatFromInt(col_index)) * draw_info.scaled_tile_dim[0] + draw_info.scaled_tile_dim[0] / 2.0 + draw_info.scaled_tile_dim[0] / 3.0,
                    .y = @as(f32, @floatFromInt(row_index)) * draw_info.scaled_tile_dim[1],
                },
                @intFromFloat(draw_info.scaled_tile_dim[0]),
                @intFromFloat(draw_info.scaled_tile_dim[1]),
                @bitCast(draw_info.board_translation),
            );
            if (draw_info.draw_3d)
                projected_p.y -= @as(f32, @floatFromInt(height)) * (draw_info.scaled_tile_dim[1] / 2.0);
            platform_api.drawTextCodepoint(
                font,
                @intCast(height + '0'),
                projected_p,
                glyph_size,
                .{ .r = 0, .g = 0, .b = 255, .a = 255 },
            );
        }
    }
}

pub fn debugDrawTileHitboxes(
    draw_info: *const DrawInfo,
    platform_api: *const sp_platform.PlatformAPI,
    world: *sp_map.World,
) void {
    for (0..board_dim) |row_index| {
        for (0..board_dim) |col_index| {
            const canonical_board_p: @Vector(2, usize) = @intCast(
                sp_map.canonicalTileP(@intCast(@Vector(2, usize){ col_index, row_index }), draw_info.draw_rot_state),
            );
            const height = world.height_map[canonical_board_p[1] * board_dim + canonical_board_p[0]];
            var projected_p = isoProj(
                platform_api,
                .{
                    .x = @as(f32, @floatFromInt(col_index)) * draw_info.scaled_tile_dim[0],
                    .y = @as(f32, @floatFromInt(row_index)) * draw_info.scaled_tile_dim[1],
                },
                @intFromFloat(draw_info.scaled_tile_dim[0]),
                @intFromFloat(draw_info.scaled_tile_dim[1]),
                @bitCast(draw_info.board_translation),
            );
            projected_p.y -= @as(f32, @floatFromInt(height)) * (draw_info.scaled_tile_dim[1] / 2.0);
            platform_api.drawRectangleLinesEx(.{
                .x = projected_p.x,
                .y = projected_p.y,
                .width = draw_info.scaled_tile_dim[0],
                .height = draw_info.scaled_tile_dim[1] / 2.0,
            }, 1, rl.GREEN);
        }
    }
}

pub fn drawTitleScreenText(
    platform_api: *const sp_platform.PlatformAPI,
    font: rl.Font,
) void {
    const title_screen_text_width = platform_api.measureText("TITLE SCREEN", glyph_size * 2);
    platform_api.drawTextEx(font, "TITLE SCREEN", .{
        .x = @floatFromInt(@divFloor(platform_api.getScreenWidth(), 2) - @divFloor(title_screen_text_width, 2)),
        .y = @floatFromInt(platform_api.getScreenHeight() - @divFloor(platform_api.getScreenHeight(), 5)),
    }, glyph_size * 2, 1.0, rl.WHITE);
}

pub fn drawPauseText(
    platform_api: *const sp_platform.PlatformAPI,
    font: rl.Font,
) void {
    const paused_width = platform_api.measureText("PAUSED", glyph_size * 2);
    platform_api.drawTextEx(font, "PAUSED", .{
        .x = @floatFromInt(@divFloor(platform_api.getScreenWidth(), 2) - @divFloor(paused_width, 2)),
        .y = @floatFromInt(platform_api.getScreenHeight() - @divFloor(platform_api.getScreenHeight(), 5)),
    }, glyph_size * 2, 1.0, rl.WHITE);
}

pub fn drawDebugInfo(
    platform_api: *const sp_platform.PlatformAPI,
    game_state: *sp_platform.GameState,
    entity_man: *ecs.EntityManager,
    resource_kind_components: *ecs.ComponentArray(sp_sim.ResourceKind),
) void {
    const scratch_ally = game_state.scratch_fba.allocator();
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
        selected_tile_p,
    }, 0..) |strz, strz_index| {
        platform_api.drawTextEx(game_state.rl_font, strz, .{
            .x = 0, //@as(f32, @floatFromInt(board_dim)) * tile_width_px,
            .y = @as(f32, @floatFromInt(strz_index)) * glyph_size,
        }, glyph_size, 1.0, rl.WHITE);
    }
}
