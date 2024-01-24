//!
//! base_math.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/23/24
//! barg8397@vandals.uidaho.edu
//!

pub inline fn clampf32(value: f32, min: f32, max: f32) f32 {
    return @max(min, @min(max, value));
}

pub const Matrix2x2I8 = struct {
    m0: i8,
    m1: i8,
    m2: i8,
    m3: i8,

    pub fn inverse(m: Matrix2x2I8) Matrix2x2I8 {
        const determinant: f32 = 1.0 / @as(f32, @floatFromInt(m.m0 * m.m3 - m.m1 * m.m2));
        const adjugate = Matrix2x2I8{
            .m0 = m.m3,
            .m1 = -m.m1,
            .m2 = -m.m2,
            .m3 = m.m0,
        };
        return Matrix2x2I8{
            .m0 = @intFromFloat(@as(f32, @floatFromInt(adjugate.m0)) * determinant),
            .m1 = @intFromFloat(@as(f32, @floatFromInt(adjugate.m1)) * determinant),
            .m2 = @intFromFloat(@as(f32, @floatFromInt(adjugate.m2)) * determinant),
            .m3 = @intFromFloat(@as(f32, @floatFromInt(adjugate.m3)) * determinant),
        };
    }

    pub fn vectorMultiply(m: Matrix2x2I8, v: @Vector(2, i8)) @Vector(2, i8) {
        return @Vector(2, i8){ m.m0 * v[0] + m.m2 * v[1], m.m1 * v[0] + m.m3 * v[1] };
    }
};
