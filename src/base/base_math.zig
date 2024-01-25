//!
//! base_math.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/23/24
//! barg8397@vandals.uidaho.edu
//!

const std = @import("std");

const meta = std.meta;

pub inline fn clampF32(value: f32, min: f32, max: f32) f32 {
    return @max(min, @min(max, value));
}

pub inline fn vector2DistanceU16(v1: @Vector(2, u16), v2: @Vector(2, u16)) f32 {
    const result = @sqrt(@as(f32, @floatFromInt((v1[0] - v2[0]) * (v1[0] - v2[0]) + (v1[1] - v2[1]) * (v1[1] - v2[1]))));
    return result;
}

pub fn Matrix2x2(comptime T: type) type {
    return struct {
        const Self = @This();

        m0: T,
        m1: T,
        m2: T,
        m3: T,

        pub fn inverse(m: Self) Self {
            comptime if (!meta.trait.isSignedInt(T) and !meta.trait.isFloat(T)) {
                @compileError("Wrong type");
            };
            const is_float: bool = comptime meta.trait.isFloat(T);

            var determinant: f32 = if (is_float)
                1.0 / (m.m0 * m.m3 - m.m1 * m.m2)
            else
                1.0 / @as(f32, @floatFromInt(m.m0 * m.m3 - m.m1 * m.m2));

            //- cabarger: Adjugate * determinant
            if (is_float) {
                return Self{
                    .m0 = m.m3 * determinant,
                    .m1 = -m.m1 * determinant,
                    .m2 = -m.m2 * determinant,
                    .m3 = m.m0 * determinant,
                };
            }
            return Self{
                .m0 = @intFromFloat(m.m3 * determinant),
                .m1 = @intFromFloat(-m.m1 * determinant),
                .m2 = @intFromFloat(-m.m2 * determinant),
                .m3 = @intFromFloat(m.m0 * determinant),
            };
        }

        pub fn vectorMultiply(m: Self, v: @Vector(2, T)) @Vector(2, T) {
            return @Vector(2, T){ m.m0 * v[0] + m.m2 * v[1], m.m1 * v[0] + m.m3 * v[1] };
        }
    };
}
