//!
//! ecs.zig
//!
//! Polymorphic Games
//! zig 0.11.0
//! Caleb Barger
//! 01/10/24
//! barg8397@vandals.uidaho.edu
//!

const std = @import("std");

const mem = std.mem;

const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const StaticBitSet = std.StaticBitSet;

const assert = std.debug.assert;

pub const EntityKind = enum(u8) {
    worker = 0,
    pile,
    resource,

    count, // NOTHING BELOW THIS LINE
};

pub const ComponentKind = enum(u8) {
    tile_p = 0,
    resource_kind,
    inventory,
    worker_state,

    count, // NOTHING BELOW THIS LINE
};

pub const EntityManager = struct {
    free_entities: ArrayList(usize),
    signatures: []StaticBitSet(@intFromEnum(ComponentKind.count)),
    signature_table: [@intFromEnum(EntityKind.count)]StaticBitSet(@intFromEnum(ComponentKind.count)),

    pub fn init(
        ally: mem.Allocator,
        entity_count: usize,
    ) !EntityManager {
        var result = EntityManager{
            .free_entities = try ArrayList(usize).initCapacity(ally, entity_count), // TODO(caleb): SinglyLinkedList
            .signatures = try ally.alloc(StaticBitSet(@intFromEnum(ComponentKind.count)), entity_count),
            .signature_table = undefined,
        };
        for (0..entity_count) |free_entity_index|
            result.free_entities.insertAssumeCapacity(free_entity_index, free_entity_index);
        for (result.signatures) |*mask|
            mask.setRangeValue(
                .{
                    .start = 0,
                    .end = @intFromEnum(ComponentKind.count),
                },
                false,
            );

        // Entity signatures
        const worker_sig_index = @intFromEnum(EntityKind.worker);
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.worker_state));
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.tile_p));
        result.signature_table[worker_sig_index].set(@intFromEnum(ComponentKind.inventory));

        const pile_sig_index = @intFromEnum(EntityKind.pile);
        result.signature_table[pile_sig_index].set(@intFromEnum(ComponentKind.inventory));
        result.signature_table[pile_sig_index].set(@intFromEnum(ComponentKind.tile_p));

        const resource_sig_index = @intFromEnum(EntityKind.resource);
        result.signature_table[resource_sig_index].set(@intFromEnum(ComponentKind.resource_kind));
        result.signature_table[resource_sig_index].set(@intFromEnum(ComponentKind.tile_p));

        return result;
    }

    pub inline fn reset(entity_man: *EntityManager) void {
        while (entity_man.free_entities.capacity != entity_man.free_entities.items.len)
            entity_man.free_entities.insertAssumeCapacity(entity_man.free_entities.items.len, entity_man.free_entities.items.len);
    }

    pub inline fn entitySignature(entity_man: *const EntityManager, entity_kind: EntityKind) StaticBitSet(@intFromEnum(ComponentKind.count)) {
        return entity_man.signature_table[@intFromEnum(entity_kind)];
    }

    pub fn newEntity(entity_man: *EntityManager) usize {
        assert(entity_man.free_entities.items.len > 0);
        const entity_id = entity_man.free_entities.pop();
        entity_man.signatures[entity_id].setRangeValue(
            .{ .start = 0, .end = @intFromEnum(ComponentKind.count) },
            false,
        );
        return entity_id;
    }

    pub inline fn componentMaskSet(
        entity_man: *EntityManager,
        entity_id: usize,
        kind: ComponentKind,
    ) void {
        entity_man.signatures[entity_id]
            .set(@intFromEnum(kind));
    }

    pub inline fn release(entity_man: *EntityManager, entity_id: usize) void {
        entity_man.free_entities.appendAssumeCapacity(entity_id);
    }

    pub fn hasComponent(
        entity_man: *EntityManager,
        entity_id: usize,
        component_kind: ComponentKind,
    ) bool {
        return entity_man.signatures[entity_id].isSet(@intFromEnum(component_kind));
    }
};

pub fn ComponentArray(comptime T: type) type {
    return struct {
        /// Component data
        data: []T,
        /// Component data count
        data_count: usize,

        /// Entity id => data index
        entity_to_data: AutoHashMap(usize, usize),

        /// Data index => entity id
        data_to_entity: AutoHashMap(usize, usize),

        pub fn init(
            ally: mem.Allocator,
            entity_count: usize,
        ) !@This() {
            var result = @This(){
                .data = try ally.alloc(T, entity_count),
                .data_count = 0,
                .entity_to_data = AutoHashMap(usize, usize).init(ally),
                .data_to_entity = AutoHashMap(usize, usize).init(ally),
            };
            try result.entity_to_data.ensureTotalCapacity(@intCast(entity_count));
            try result.data_to_entity.ensureTotalCapacity(@intCast(entity_count));
            return result;
        }

        pub fn add(component_array: *@This(), entity_id: usize, item: T) void {
            component_array.data[component_array.data_count] = item;
            component_array.data_to_entity.putAssumeCapacity(
                component_array.data_count,
                entity_id,
            );
            component_array.entity_to_data.putAssumeCapacity(
                entity_id,
                component_array.data_count,
            );
            component_array.data_count += 1;
        }

        pub fn remove(
            component_array: *@This(),
            entity_id: usize,
        ) void {
            const component_index = component_array.entity_to_data
                .get(entity_id) orelse unreachable;
            const last_slot_entity_id = component_array.data_to_entity
                .get(component_array.data_count - 1) orelse unreachable;

            // Overwrite the data
            component_array.data[component_index] =
                component_array.data[component_array.data_count - 1];
            component_array.entity_to_data.putAssumeCapacity(last_slot_entity_id, component_index);
            component_array.data_to_entity.putAssumeCapacity(component_index, last_slot_entity_id);
            component_array.data_count -= 1;
        }

        pub fn reset(component_array: *@This()) void {
            component_array.entity_to_data.clearRetainingCapacity();
            component_array.data_to_entity.clearRetainingCapacity();
            component_array.data_count = 0;
        }
    };
}
