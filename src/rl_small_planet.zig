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
const rl = @import("rl.zig");
const platform = @import("small_planet_platform.zig");

const fs = std.fs;
const heap = std.heap;

const FixedBufferAllocator = heap.FixedBufferAllocator;

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
            std.debug.print("{s}\n", .{pathz});
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

fn closeLibrary(library_handle: LibraryHandle) void {
    switch (builtin.os.tag) {
        .windows => std.os.windows.FreeLibrary(library_handle),
        else => unreachable,
    }
}

const LibraryFunction = if (builtin.os.tag == .windows) std.os.windows.FARPROC else *anyopaque;
fn loadLibraryFunction(library_handle: LibraryHandle, function_name: []const u8) ?LibraryFunction {
    var result: ?LibraryFunction = null;
    switch (builtin.os.tag) {
        .windows => {
            result = std.os.windows.kernel32.GetProcAddress(library_handle, function_name);
        },
        else => unreachable,
    }
    return result;
}

pub fn main() !void {
    // Window/Audio init
    rl.InitWindow(1600, 1200, "small-planet");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();

    var perm_mem = heap.page_allocator.alloc(u8, 1024 * 1024 * 5) catch unreachable;
    var perm_fba = FixedBufferAllocator.init(perm_mem);

    var scratch_mem = heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable;
    var scratch_fba = FixedBufferAllocator.init(scratch_mem);

    var platform_api = platform.PlatformAPI{
        .loadTexture = loadTexture,
        .getFontDefault = getFontDefault,
        .getMouseWheelMove = getMouseWheelMove,
        .getMousePosition = getMousePosition,
    };

    var game_state: platform.GameState = undefined;
    game_state.did_init = false;
    game_state.perm_fba = perm_fba;
    game_state.scratch_fba = scratch_fba;

    const track1 = rl.LoadMusicStream("assets/music/track_1.wav");
    rl.PlayMusicStream(track1);
    rl.SetMusicVolume(track1, 0.0);

    const rel_lib_path = "./zig-out/lib/";

    const active_game_code_path = try fs.path.join(
        perm_fba.allocator(),
        &[_][]const u8{ rel_lib_path, try std.mem.join(
            perm_fba.allocator(),
            ".",
            &.{
                "libactive-sp-game-code",
                if (builtin.os.tag == .windows) "dll" else "so",
            },
        ) },
    );

    const game_code_path = try fs.path.join(
        perm_fba.allocator(),
        &[_][]const u8{ rel_lib_path, try std.mem.join(
            perm_fba.allocator(),
            ".",
            &.{
                "libsp-game-code",
                if (builtin.os.tag == .windows) "dll" else "so",
            },
        ) },
    );

    if (true) {
        std.debug.print("{s}\n", .{game_code_path});
        std.debug.print("{s}\n", .{active_game_code_path});
        // unreachable;
    }

    var game_code_file_ctime = (try fs.cwd().statFile(game_code_path)).ctime;
    const lib_dir = try fs.cwd().openDir(rel_lib_path, .{});
    try fs.cwd().copyFile(game_code_path, lib_dir, fs.path.basename(active_game_code_path), .{});

    var game_code_library: LibraryHandle = try loadLibrary(&scratch_fba, active_game_code_path);
    var game_code_fn_ptr: LibraryFunction = loadLibraryFunction(game_code_library, "smallPlanetGameCode") orelse unreachable;
    var smallPlanetGameCode: *fn (*platform.PlatformAPI, *platform.GameState) void = @ptrCast(game_code_fn_ptr);

    while (!rl.WindowShouldClose()) {
        // Detect new game code lib and load it.
        if ((try std.fs.cwd().statFile(game_code_path)).ctime != game_code_file_ctime) {
            closeLibrary(game_code_library);
            try fs.cwd().copyFile(game_code_path, lib_dir, fs.path.basename(active_game_code_path), .{});
            game_code_library = try loadLibrary(&scratch_fba, active_game_code_path);
            game_code_fn_ptr = loadLibraryFunction(game_code_library, "smallPlanetGameCode") orelse unreachable;
            smallPlanetGameCode = @ptrCast(game_code_fn_ptr);
        }

        rl.UpdateMusicStream(track1);
        smallPlanetGameCode(&platform_api, &game_state);
    }
    rl.CloseWindow();
}
