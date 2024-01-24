//!
//! rl_small_planet.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 12/06/23
//! barg8397@vandals.uidaho.edu
//!
//! Raylib platform layer for small planet
//!

const builtin = @import("builtin");
const std = @import("std");
const third_party = @import("third_party");

const sp_platform = @import("sp_platform.zig");

const rl = third_party.rl;
const fs = std.fs;
const heap = std.heap;

const FixedBufferAllocator = heap.FixedBufferAllocator;

pub extern "c" fn dlerror() ?[*:0]const u8;

const LibraryHandle = if (builtin.os.tag == .windows) std.os.windows.HMODULE else *anyopaque;
fn loadLibrary(scratch_fba: *FixedBufferAllocator, path: []const u8) !LibraryHandle {
    var result: LibraryHandle = undefined;
    const restore_end_index = scratch_fba.end_index;
    defer scratch_fba.end_index = restore_end_index;
    switch (builtin.os.tag) {
        .windows => {
            const lpstr_game_code_path =
                try scratch_fba.allocator().alloc(u16, path.len + 1);
            for (path, 0..) |byte, byte_index|
                lpstr_game_code_path[byte_index] = byte;
            lpstr_game_code_path[path.len] = 0;

            result = try std.os.windows.LoadLibraryW(@ptrCast(lpstr_game_code_path.ptr));
        },
        else => {
            const pathz = try scratch_fba.allocator().dupeZ(u8, path);
            var result2 = std.c.dlopen(pathz, 0x1);
            if (result2 == null) {
                std.debug.print("{s}\n", .{dlerror().?});
                unreachable;
            }
            result = result2.?;
        },
    }
    return result;
}

fn unloadLibrary(library_handle: LibraryHandle) void {
    switch (builtin.os.tag) {
        .windows => std.os.windows.FreeLibrary(library_handle),
        else => _ = std.c.dlclose(library_handle),
    }
}

const LibraryFunction = if (builtin.os.tag == .windows) std.os.windows.FARPROC else *anyopaque;
fn loadLibraryFunction(
    scratch_fba: *FixedBufferAllocator,
    library_handle: LibraryHandle,
    function_name: []const u8,
) !LibraryFunction {
    const restore_end_index = scratch_fba.end_index;
    defer scratch_fba.end_index = restore_end_index;
    var result: LibraryFunction = undefined;
    switch (builtin.os.tag) {
        .windows => {
            result = std.os.windows.kernel32.GetProcAddress(library_handle, function_name.ptr) orelse unreachable;
        },
        else => {
            const function_namez = scratch_fba.allocator().dupeZ(u8, function_name) catch unreachable;
            result = std.c.dlsym(library_handle, function_namez) orelse unreachable;
        },
    }
    return result;
}

pub fn main() !void {
    // Window/Audio init
    rl.InitWindow(800, 600, "small-planet");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();

    var perm_mem = heap.page_allocator.alloc(u8, 1024 * 1024 * 5) catch unreachable;
    var perm_fba = FixedBufferAllocator.init(perm_mem);

    var scratch_mem = heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable;
    var scratch_fba = FixedBufferAllocator.init(scratch_mem);

    var platform_mem = heap.page_allocator.alloc(u8, 1024) catch unreachable;
    var platform_fba = FixedBufferAllocator.init(platform_mem);

    const platform_api = platformAPIInit();

    var game_state: sp_platform.GameState = undefined;
    game_state.did_init = false;
    game_state.perm_fba = perm_fba;
    game_state.scratch_fba = scratch_fba;

    const track1 = rl.LoadMusicStream("assets/music/track_1.wav");
    rl.PlayMusicStream(track1);
    rl.SetMusicVolume(track1, 0.0);

    const rel_lib_path = "./zig-out/lib/";
    const active_game_code_path = try fs.path.join(
        platform_fba.allocator(),
        &[_][]const u8{
            rel_lib_path,
            if (builtin.os.tag == .windows) "active-game-code.dll" else "libactive-game-code.so",
        },
    );
    const game_code_path = try fs.path.join(
        platform_fba.allocator(),
        &[_][]const u8{
            rel_lib_path,
            if (builtin.os.tag == .windows) "game-code.dll" else "libgame-code.so",
        },
    );

    var game_code_file_ctime = (try fs.cwd().statFile(game_code_path)).ctime;
    const lib_dir = try fs.cwd().openDir(rel_lib_path, .{});
    try fs.cwd().copyFile(game_code_path, lib_dir, fs.path.basename(active_game_code_path), .{});

    var game_code_library: LibraryHandle = try loadLibrary(&platform_fba, active_game_code_path);
    var game_code_fn_ptr: LibraryFunction = try loadLibraryFunction(&platform_fba, game_code_library, "spUpdateAndRender");
    var spUpdateAndRender: *fn (*const sp_platform.PlatformAPI, *sp_platform.GameState) void = @ptrCast(game_code_fn_ptr);

    while (!rl.WindowShouldClose()) {
        // Detect new game code lib and load it.
        const creation_time_now = (try std.fs.cwd().statFile(game_code_path)).ctime;
        if (creation_time_now != game_code_file_ctime) {
            unloadLibrary(game_code_library);
            try fs.cwd().copyFile(game_code_path, lib_dir, fs.path.basename(active_game_code_path), .{});
            game_code_library = try loadLibrary(&platform_fba, active_game_code_path);
            game_code_fn_ptr = try loadLibraryFunction(&platform_fba, game_code_library, "spUpdateAndRender");
            spUpdateAndRender = @ptrCast(game_code_fn_ptr);
            game_code_file_ctime = creation_time_now;
        }

        rl.UpdateMusicStream(track1);
        spUpdateAndRender(&platform_api, &game_state);
    }
    rl.CloseWindow();
}

fn platformAPIInit() sp_platform.PlatformAPI {
    return sp_platform.PlatformAPI{
        .loadTexture = loadTexture,
        .getFontDefault = getFontDefault,
        .getMouseWheelMove = getMouseWheelMove,
        .getMousePosition = getMousePosition,
        .isMouseButtonDown = isMouseButtonDown,
        .isKeyDown = isKeyDown,
        .getScreenWidth = getScreenWidth,
        .getScreenHeight = getScreenHeight,
        .matrixInvert = matrixInvert,
        .drawTexturePro = drawTexturePro,
        .getMouseDelta = getMouseDelta,
        .getTime = getTime,
        .getKeyPressed = getKeyPressed,
        .beginDrawing = beginDrawing,
        .clearBackground = clearBackground,
        .drawLineEx = drawLineEx,
        .drawTextCodepoint = drawTextCodepoint,
        .drawTextEx = drawTextEx,
        .drawRectangleRec = drawRectangleRec,
        .drawRectangleLinesEx = drawRectangleLinesEx,
        .endDrawing = endDrawing,
        .measureText = measureText,
        .getFPS = getFPS,
        .loadFont = loadFont,
        .checkCollisionPointRec = checkCollisionPointRec,
    };
}

fn loadTexture(path: [:0]const u8) rl.Texture {
    return rl.LoadTexture(path);
}

fn getFontDefault() rl.Font {
    return rl.GetFontDefault();
}

fn getMouseWheelMove() f32 {
    return rl.GetMouseWheelMove();
}

fn getMousePosition() rl.Vector2 {
    return rl.GetMousePosition();
}

fn isMouseButtonDown(mouse_button: c_int) bool {
    return rl.IsMouseButtonDown(mouse_button);
}

fn isKeyDown(key: c_int) bool {
    return rl.IsKeyDown(key);
}

fn getScreenWidth() c_int {
    return rl.GetScreenWidth();
}

fn getScreenHeight() c_int {
    return rl.GetScreenHeight();
}

fn matrixInvert(matrix: rl.Matrix) rl.Matrix {
    return rl.MatrixInvert(matrix);
}

fn drawTexturePro(texture: rl.Texture, source: rl.Rectangle, dest: rl.Rectangle, origin: rl.Vector2, rotation: f32, tint: rl.Color) void {
    return rl.DrawTexturePro(texture, source, dest, origin, rotation, tint);
}

fn getMouseDelta() rl.Vector2 {
    return rl.GetMouseDelta();
}

fn getTime() f64 {
    return rl.GetTime();
}

fn getKeyPressed() c_int {
    return rl.GetKeyPressed();
}

fn beginDrawing() void {
    rl.BeginDrawing();
}

fn clearBackground(color: rl.Color) void {
    rl.ClearBackground(color);
}

fn drawLineEx(start: rl.Vector2, end: rl.Vector2, thick: f32, tint: rl.Color) void {
    return rl.DrawLineEx(start, end, thick, tint);
}

fn drawTextCodepoint(font: rl.Font, code_point: c_int, p: rl.Vector2, size: f32, color: rl.Color) void {
    rl.DrawTextCodepoint(font, code_point, p, size, color);
}

fn drawTextEx(font: rl.Font, text: [*:0]const u8, p: rl.Vector2, size: f32, spacing: f32, color: rl.Color) void {
    rl.DrawTextEx(font, text, p, size, spacing, color);
}

fn drawRectangleRec(rec: rl.Rectangle, color: rl.Color) void {
    rl.DrawRectangleRec(rec, color);
}

fn drawRectangleLinesEx(rec: rl.Rectangle, line_thick: f32, color: rl.Color) void {
    rl.DrawRectangleLinesEx(rec, line_thick, color);
}

fn checkCollisionPointRec(point: rl.Vector2, rec: rl.Rectangle) bool {
    return rl.CheckCollisionPointRec(point, rec);
}

fn endDrawing() void {
    rl.EndDrawing();
}

fn measureText(text: [*:0]const u8, glyph_size: c_int) c_int {
    return rl.MeasureText(text, glyph_size);
}

fn getFPS() c_int {
    return rl.GetFPS();
}

fn loadFont(font_path: [*:0]const u8) rl.Font {
    return rl.LoadFont(font_path);
}
