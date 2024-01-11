//!
//! win32_small_planet.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 12/06/23
//! barg8397@vandals.uidaho.edu
//!
//! Windows platform layer for small planet
//!

const std = @import("std");
const rl = @import("rl.zig");
const platform = @import("small_planet_platform.zig");

const fs = std.fs;
const heap = std.heap;

const FixedBufferAllocator = heap.FixedBufferAllocator;

pub fn main() !void {
    // Window/Audio init
    rl.InitWindow(1600, 1200, "small-planet");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();
    // const rl_font = rl.GetFontDefault();

    var perm_mem = heap.page_allocator.alloc(u8, 1024 * 1024 * 5) catch unreachable;
    var scratch_mem = heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable;

    var game_state: platform.GameState = undefined;
    game_state.did_init = false;
    game_state.perm_fba = FixedBufferAllocator.init(perm_mem);
    game_state.scratch_fba = FixedBufferAllocator.init(scratch_mem);

    const track1 = rl.LoadMusicStream("assets/music/track_1.wav");
    rl.PlayMusicStream(track1);
    rl.SetMusicVolume(track1, 0.0);

    // FIXME(caleb): Use relpath form lib_dir in the future...
    const active_game_code_path = "zig-out/lib/active-sp-game-code.dll";
    const game_code_path = "zig-out/lib/sp-game-code.dll";
    var game_code_file_ctime = (try fs.cwd().statFile(game_code_path)).ctime;

    const lib_dir_path = "zig-out/lib/";
    const lib_dir = try fs.cwd().openDir(lib_dir_path, .{});
    try fs.cwd().copyFile(game_code_path, lib_dir, "active-sp-game-code.dll", .{});

    // FIXME(caleb): !!!!DANGER PLATFORM SPECIFIC CODE!!!!
    const lpstr_game_code_path = try std.heap.page_allocator.alloc(u16, active_game_code_path.len + 1);
    for (active_game_code_path, 0..) |byte, byte_index|
        lpstr_game_code_path[byte_index] = byte;
    lpstr_game_code_path[active_game_code_path.len] = 0;

    var game_code_hmodule = try std.os.windows.LoadLibraryW(@ptrCast(lpstr_game_code_path.ptr));
    var game_code_fn_proc_address = std.os.windows.kernel32.GetProcAddress(game_code_hmodule, "smallPlanetGameCode") orelse unreachable;
    var smallPlanetGameCode: *fn (*platform.GameState) void = @ptrCast(game_code_fn_proc_address);

    while (!rl.WindowShouldClose()) {
        // Detect new game code lib and load it.
        if ((try std.fs.cwd().statFile(game_code_path)).ctime != game_code_file_ctime) {
            std.os.windows.FreeLibrary(game_code_hmodule);
            try fs.cwd().copyFile(game_code_path, lib_dir, "active-sp-game-code.dll", .{});
            game_code_hmodule = try std.os.windows.LoadLibraryW(@ptrCast(lpstr_game_code_path.ptr));
            game_code_fn_proc_address = std.os.windows.kernel32.GetProcAddress(game_code_hmodule, "smallPlanetGameCode") orelse unreachable;
            smallPlanetGameCode = @ptrCast(game_code_fn_proc_address);
        }

        rl.UpdateMusicStream(track1);
        smallPlanetGameCode(&game_state);
    }
    rl.CloseWindow();
}
