const std = @import("std");
const rl = @import("raylib/src/build.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const third_party_module = b.createModule(.{
        .source_file = .{ .path = "src/third_party/third_party.zig" },
    });
    const base_module = b.createModule(.{
        .source_file = .{ .path = "src/base/base.zig" },
    });

    const build_game_code = b.option(bool, "game-code", "When true builds game code") orelse false;
    if (build_game_code) {
        const game_code = b.addSharedLibrary(.{
            .name = "game-code", // NOTE(caleb): Explain the tmp prefix so future me knows why I did things this way.
            .root_source_file = .{ .path = "src/sp/sp_game_code.zig" },
            .target = target,
            .optimize = optimize,
        });
        game_code.linkLibC();
        game_code.addIncludePath(.{ .path = "raylib/src/" });
        game_code.addModule("third_party", third_party_module);
        game_code.addModule("base", base_module);
        b.installArtifact(game_code);
    }

    const build_platform_code = b.option(bool, "platform-code", "When true builds platform code") orelse false;
    if (build_platform_code) {
        const platform_code = b.addExecutable(.{
            .name = "small-planet",
            .root_source_file = .{ .path = "src/sp/rl_sp.zig" },
            .target = target,
            .optimize = optimize,
        });
        const raylib = rl.addRaylib(b, target, optimize, .{});
        platform_code.linkLibC();
        platform_code.addIncludePath(.{ .path = "raylib/src/" });
        platform_code.addModule("third_party", third_party_module);
        platform_code.addModule("base", base_module);
        platform_code.linkSystemLibrary("dl");
        platform_code.linkLibrary(raylib);
        b.installArtifact(platform_code);
    }
}
