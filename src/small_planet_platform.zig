//!
//! small_planet_platform.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/10/24
//! barg8397@vandals.uidaho.edu
//!

const std = @import("std");
const rl = @import("rl.zig");

const math = std.math;
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const rand = std.rand;

const FixedBufferAllocator = std.heap.FixedBufferAllocator;

pub const PlatformAPI = struct {
    loadTexture: *const fn ([:0]const u8) rl.Texture,
};

// TODO(caleb): Function type for smallPlanetGameCode()
pub const GameState = struct {
    did_init: bool,
    perm_fba: FixedBufferAllocator,
    scratch_fba: FixedBufferAllocator,

    // A word on offsets...
    // These are offsets to data residing in perm memory that platform layer
    // doesn't know about. (hents why they aren't stored here directly)

    entity_man_offset: usize,
    tile_p_components_offset: usize,
    target_tile_p_components_offset: usize,
    resource_kind_components_offset: usize,
    inventory_components_offset: usize,
    worker_state_components_offset: usize,
    tileset_offset: usize,
    world_offset: usize,

    // Types which are shared between platform and game code are placed here directly
    // as seen bellow.

    seed: u64,
    xoshiro_256: rand.Xoshiro256,
    sample_walk_map: []usize,

    game_time_minute: usize,
    game_time_hour: usize,
    game_time_day: usize,
    game_time_year: usize,

    tick_granularity: u8,

    debug_draw_distance_map: bool,
    debug_draw_grid_lines: bool,

    is_paused: bool,
    pause_start_time: f64,
    last_tick_time: f64,

    view_mode: u8,
    selected_tile_p: @Vector(2, i8),
    selected_region_p: @Vector(2, u8),

    height_scale: f32,
    scale_factor: f32,

    board_translation: rl.Vector2,

    // NOTE(caleb): Determines draw order of board tiles, giving the effect
    // of a rotation.
    draw_rot_state: u8,

    rl_font: rl.Font,

    // Inputs TODO(caleb): Pull these out into a GameInput struct.
    mouse_p: rl.Vector2,
};
