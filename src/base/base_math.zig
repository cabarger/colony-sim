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

pub fn Mat3x3(comptime T: type) type {
    return struct {
        const Self = @This();

        m: [9]T,

        pub fn inverse(m: Self) Self {
            const is_float = comptime meta.trait.isFloat(T);
            comptime if (!meta.trait.isSignedInt(T) and !is_float) {
                @compileError("Wrong type");
            };
            var result: Self = undefined;
            const determinant: f32 = if (is_float)
                1.0 / (m.m[0] * m.m[3] - m.m[1] * m.m[2])
            else
                1.0 / @as(f32, @floatFromInt(m.m[0] * m.m[3] - m.m[1] * m.m[2]));

            //- cabarger: Adjugate * determinant
            if (is_float) {
                result.m[0] = m.m[3] * determinant;
                result.m[1] = -m.m[1] * determinant;
                result.m[2] = -m.m[2] * determinant;
                result.m[3] = m.m[0] * determinant;
            } else {
                result.m[0] = @intFromFloat(m.m[3] * determinant);
                result.m[1] = @intFromFloat(-m.m[1] * determinant);
                result.m[2] = @intFromFloat(-m.m[2] * determinant);
                result.m[3] = @intFromFloat(m.m[0] * determinant);
            }

            return result;
        }

        pub fn vectorMultiply(m: Self, v: @Vector(2, T)) @Vector(2, T) {
            return @Vector(2, T){ m.m[0] * v[0] + m.m[2] * v[1], m.m[1] * v[0] + m.m[3] * v[1] };
        }
    };
}
