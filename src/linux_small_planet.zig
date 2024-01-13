//!
//! linux_small_planet.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 12/11/23
//! barg8397@vandals.uidaho.edu
//!
//! Linux platform layer for small planet
//!

const std = @import("std");
const rl = @import("rl.zig");
const platform = @import("small_planet_platform.zig");

const c = std.c;
const fs = std.fs;
const heap = std.heap;

const FixedBufferAllocator = heap.FixedBufferAllocator;

const RTLD_LAZY = 0x00001; // Lazy function call binding.  */
const RTLD_NOW = 0x00002; // Immediate function call binding.  */
const RTLD_BINDING_MASK = 0x3; // Mask of binding time value.  */
const RTLD_NOLOAD = 0x00004; // Do not load the object.  */
const RTLD_DEEPBIND = 0x00008; // Use deep binding.  */

pub fn main() !void {
    // Window/Audio init
    rl.InitWindow(1600, 1200, "small-planet");
    rl.SetWindowState(rl.FLAG_WINDOW_RESIZABLE);
    rl.InitAudioDevice();

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
    const active_game_code_path = "zig-out/lib/active-sp-game-code.so";
    const game_code_path = "zig-out/lib/sp-game-code.so";
    var game_code_file_ctime = (try fs.cwd().statFile(game_code_path)).ctime;

    const lib_dir_path = "zig-out/lib/";
    const lib_dir = try fs.cwd().openDir(lib_dir_path, .{});
    try fs.cwd().copyFile(game_code_path, lib_dir, "active-sp-game-code.so", .{});

    const active_game_code_pathz = perm_mem.allocator().dupeZ(active_game_code_path);

    var game_code_lib = try c.dlopen(
        active_game_code_pathz,
        RTLD_LAZY,
    );

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
