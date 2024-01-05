const std = @import("std");
const rl = @import("raylib/src/build.zig");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const build_game_code = b.option(bool, "game-code", "Build game code?") orelse false;
    if (build_game_code) {
        const sp_game_code = b.addSharedLibrary(.{
            .name = "sp-game-code", // NOTE(caleb): Explain the tmp prefix so future me knows why I did things this way.
            .root_source_file = .{ .path = "src/small_planet.zig" },
            .target = target,
            .optimize = optimize,
        });
        b.installArtifact(sp_game_code);
    }

    const build_platform_code = b.option(bool, "platform-code", "Build platform code?") orelse false;
    if (build_platform_code) {
        const exe = b.addExecutable(.{
            .name = "sp",
            // In this case the main source file is merely a path, however, in more
            // complicated build scripts, this could be a generated file.
            .root_source_file = .{ .path = "src/small_planet_platform.zig" },
            .target = target,
            .optimize = optimize,
        });
        const raylib = rl.addRaylib(b, target, optimize, .{});
        exe.addIncludePath(.{ .path = "raylib/src/" });
        exe.linkLibrary(raylib);

        // This declares intent for the executable to be installed into the
        // standard location when the user invokes the "install" step (the default
        // step when running `zig build`).
        b.installArtifact(exe);
    }
}
