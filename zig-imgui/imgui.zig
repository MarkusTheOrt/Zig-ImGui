//! ==========================================================
//! This file is generated from template.zig and generate.bat.
//! Do not modify it by hand.
//! ==========================================================

const std = @import("std");
const builtin = @import("builtin");
const assert = @import("std").debug.assert;
const imgui = @This();

pub const DrawCallback_ResetRenderState = @intToPtr(DrawCallback, ~@as(usize, 0));

pub const VERSION = "1.88";
pub fn CHECKVERSION() void {
    if (builtin.mode != .ReleaseFast) {
        assert(raw.igDebugCheckVersionAndDataLayout(VERSION, @sizeOf(IO), @sizeOf(Style), @sizeOf(Vec2), @sizeOf(Vec4), @sizeOf(DrawVert), @sizeOf(DrawIdx)));
    }
}

pub const FLT_MAX: f32 = @bitCast(f32, @as(u32, 0x7F7FFFFF));
pub const FLT_MIN: f32 = @bitCast(f32, @as(u32, 0x00800000));

pub const FlagsInt = u32;

pub fn FlagsMixin(comptime FlagType: type) type {
    comptime assert(@sizeOf(FlagType) == 4);
    return struct {
        pub fn toInt(self: FlagType) FlagsInt {
            return @bitCast(FlagsInt, self);
        }
        pub fn fromInt(value: FlagsInt) FlagType {
            return @bitCast(FlagType, value);
        }
        pub fn with(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) | toInt(b));
        }
        pub fn only(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) & toInt(b));
        }
        pub fn without(a: FlagType, b: FlagType) FlagType {
            return fromInt(toInt(a) & ~toInt(b));
        }
        pub fn hasAllSet(a: FlagType, b: FlagType) bool {
            return (toInt(a) & toInt(b)) == toInt(b);
        }
        pub fn hasAnySet(a: FlagType, b: FlagType) bool {
            return (toInt(a) & toInt(b)) != 0;
        }
        pub fn isEmpty(a: FlagType) bool {
            return toInt(a) == 0;
        }
        pub fn eql(a: FlagType, b: FlagType) bool {
            return toInt(a) == toInt(b);
        }
    };
}

fn destruct(comptime T: type, ptr: *T) void {
    if (@typeInfo(T) == .Struct or @typeInfo(T) == .Union) {
        if (@hasDecl(T, "deinit")) {
            ptr.deinit();
        }
    }
}

fn eql(comptime T: type, a: T, b: T) bool {
    if (@typeInfo(T) == .Struct or @typeInfo(T) == .Union) {
        if (@hasDecl(T, "eql")) {
            return a.eql(b);
        }
    }
    return a == b;
}

pub fn Vector(comptime T: type) type {
    return extern struct {
        Size: u32 = 0,
        Capacity: u32 = 0,
        Data: ?[*]T = null,

        // Provide standard typedefs but we don't use them ourselves.
        pub const value_type = T;

        // Constructors, destructor
        pub fn deinit(self: *@This()) void {
            if (self.Data) |d| raw.igMemFree(@ptrCast(*anyopaque, d));
            self.* = undefined;
        }

        pub fn clone(self: @This()) @This() {
            var cloned = @This(){};
            if (self.Size != 0) {
                cloned.resize_undefined(self.Size);
                @memcpy(@ptrCast([*]u8, cloned.Data.?), @ptrCast([*]const u8, self.Data.?), self.Size * @sizeOf(T));
            }
            return cloned;
        }

        pub fn copy(self: *@This(), other: @This()) void {
            self.Size = 0;
            if (other.Size != 0) {
                self.resize_undefined(other.Size);
                @memcpy(@ptrCast([*]u8, self.Data.?), @ptrCast([*]const u8, other.Data.?), other.Size * @sizeOf(T));
            }
        }

        pub fn from_slice(slice: []const T) @This() {
            var result = @This(){};
            if (slice.len != 0) {
                result.resize_undefined(@intCast(u32, slice.len));
                @memcpy(@ptrCast([*]u8, result.Data.?), @ptrCast([*]const u8, slice.ptr), slice.len * @sizeOf(T));
            }
            return result;
        }

        /// Important: does not destruct anything
        pub fn clear(self: *@This()) void {
            if (self.Data) |d| raw.igMemFree(@ptrCast(?*anyopaque, d));
            self.* = .{};
        }

        /// Destruct and delete all pointer values, then clear the array.
        /// T must be a pointer or optional pointer.
        pub fn clear_delete(self: *@This()) void {
            comptime var ti = @typeInfo(T);
            const is_optional = (ti == .Optional);
            if (is_optional) ti = @typeInfo(ti.Optional.child);
            if (ti != .Pointer or ti.Pointer.is_const or ti.Pointer.size != .One)
                @compileError("clear_delete() can only be called on vectors of mutable single-item pointers, cannot apply to Vector(" ++ @typeName(T) ++ ").");
            const ValueT = ti.Pointer.child;

            if (is_optional) {
                for (self.items()) |it| {
                    if (it) |_ptr| {
                        const ptr: *ValueT = _ptr;
                        destruct(ValueT, ptr);
                        raw.igMemFree(ptr);
                    }
                }
            } else {
                for (self.items()) |_ptr| {
                    const ptr: *ValueT = _ptr;
                    destruct(ValueT, ptr);
                    raw.igMemFree(@ptrCast(?*anyopaque, ptr));
                }
            }
            self.clear();
        }

        pub fn clear_destruct(self: *@This()) void {
            for (self.items()) |*ptr| {
                destruct(T, ptr);
            }
            self.clear();
        }

        pub fn empty(self: @This()) bool {
            return self.Size == 0;
        }

        pub fn size(self: @This()) u32 {
            return self.Size;
        }

        pub fn size_in_bytes(self: @This()) u32 {
            return self.Size * @sizeOf(T);
        }

        pub fn max_size(self: @This()) u32 {
            _ = self;
            return 0x7FFFFFFF / @sizeOf(T);
        }

        pub fn items(self: @This()) []T {
            return if (self.Size == 0) &[_]T{} else self.Data.?[0..self.Size];
        }

        pub fn _grow_capacity(self: @This(), sz: u32) u32 {
            const new_cap: u32 = if (self.Capacity == 0) 8 else (self.Capacity + self.Capacity / 2);
            return if (new_cap > sz) new_cap else sz;
        }

        pub fn resize_undefined(self: *@This(), new_size: u32) void {
            if (new_size > self.Capacity)
                self.reserve(self._grow_capacity(new_size));
            self.Size = new_size;
        }
        pub fn resize_splat(self: *@This(), new_size: u32, value: T) void {
            if (new_size > self.Capacity)
                self.reserve(self._grow_capacity(new_size));
            if (new_size > self.Size)
                std.mem.set(T, self.Data.?[self.Size..new_size], value);
            self.Size = new_size;
        }
        /// Resize a vector to a smaller size, guaranteed not to cause a reallocation
        pub fn shrink(self: *@This(), new_size: u32) void {
            assert(new_size <= self.Size);
            self.Size = new_size;
        }
        pub fn reserve(self: *@This(), new_capacity: u32) void {
            if (new_capacity <= self.Capacity) return;
            const new_data = @ptrCast(?[*]T, @alignCast(@alignOf(T), raw.igMemAlloc(new_capacity * @sizeOf(T))));
            if (self.Data) |sd| {
                if (self.Size != 0) {
                    @memcpy(@ptrCast([*]u8, new_data.?), @ptrCast([*]const u8, sd), self.Size * @sizeOf(T));
                }
                raw.igMemFree(@ptrCast(*anyopaque, sd));
            }
            self.Data = new_data;
            self.Capacity = new_capacity;
        }
        pub fn reserve_discard(self: *@This(), new_capacity: u32) void {
            if (new_capacity <= self.Capacity) return;
            if (self.Data) |sd| raw.igMemFree(@ptrCast(*anyopaque, sd));
            self.Data = @ptrCast(?[*]T, @alignCast(@alignOf(T), raw.igMemAlloc(new_capacity * @sizeOf(T))));
            self.Capacity = new_capacity;
        }

        // NB: It is illegal to call push_back/push_front/insert with a reference pointing inside the ImVector data itself! e.g. v.push_back(v.items()[10]) is forbidden.
        pub fn push_back(self: *@This(), v: T) void {
            if (self.Size == self.Capacity)
                self.reserve(self._grow_capacity(self.Size + 1));
            self.Data.?[self.Size] = v;
            self.Size += 1;
        }
        pub fn pop_back(self: *@This()) void {
            self.Size -= 1;
        }
        pub fn push_front(self: *@This(), v: T) void {
            if (self.Size == 0) self.push_back(v) else self.insert(0, v);
        }
        pub fn erase(self: *@This(), index: u32) void {
            assert(index < self.Size);
            self.Size -= 1;
            const len = self.Size;
            if (index < len) {
                var it = index;
                const data = self.Data.?;
                while (it < len) : (it += 1) {
                    data[it] = data[it + 1];
                }
            }
        }
        pub fn erase_range(self: *@This(), start: u32, end: u32) void {
            assert(start <= end);
            assert(end <= self.Size);
            if (start == end) return;
            const len = self.Size;
            self.Size -= (end - start);
            if (end < len) {
                var it = start;
                var end_it = end;
                const data = self.Data.?;
                while (end_it < len) : ({ it += 1; end_it += 1; }) {
                    data[it] = data[end_it];
                }
            }
        }
        pub fn erase_unsorted(self: *@This(), index: u32) void {
            assert(index < self.Size);
            self.Size -= 1;
            if (index != self.Size) {
                self.Data.?[index] = self.Data.?[self.Size];
            }
        }
        pub fn insert(self: *@This(), index: u32, v: T) void {
            assert(index <= self.Size);
            if (self.Size == self.Capacity)
                self.reserve(self._grow_capacity(self.Size + 1));
            const data = self.Data.?;
            if (index < self.Size) {
                var it = self.Size;
                while (it > index) : (it -= 1) {
                    data[it] = data[it-1];
                }
            }
            data[index] = v;
            self.Size += 1;
        }
        pub fn contains(self: @This(), v: T) bool {
            for (self.items()) |*it| {
                if (imgui.eql(T, v, it.*)) return true;
            }
            return false;
        }
        pub fn find(self: @This(), v: T) ?u32 {
            return for (self.items()) |*it, i| {
                if (imgui.eql(T, v, it.*)) break @intCast(u32, i);
            } else null;
        }
        pub fn find_erase(self: *@This(), v: T) bool {
            if (self.find(v)) |idx| {
                self.erase(idx);
                return true;
            }
            return false;
        }
        pub fn find_erase_unsorted(self: *@This(), v: T) bool {
            if (self.find(v)) |idx| {
                self.erase_unsorted(idx);
                return true;
            }
            return false;
        }

        pub fn eql(self: @This(), other: @This()) bool {
            if (self.Size != other.Size) return false;
            var i: u32 = 0;
            while (i < self.Size) : (i += 1) {
                if (!imgui.eql(T, self.Data.?[i], other.Data.?[i]))
                    return false;
            }
            return true;
        }
    };
}

pub const Vec2 = extern struct {
    x: f32 = 0,
    y: f32 = 0,
    
    pub fn init(x: f32, y: f32) Vec4 {
        return .{ .x = x, .y = y };
    }

    pub fn eql(self: Vec2, other: Vec2) bool {
        return self.x == other.x and self.y == other.y;
    }
};

pub const Vec4 = extern struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,
    w: f32 = 0,

    pub fn init(x: f32, y: f32, z: f32, w: f32) Vec4 {
        return .{ .x = x, .y = y, .z = z, .w = w };
    }

    pub fn eql(self: Vec4, other: Vec4) bool {
        return self.x == other.x
            and self.y == other.y
            and self.z == other.z
            and self.w == other.w;
    }
};

pub const Color = extern struct {
    Value: Vec4,

    pub fn initRGBA(r: f32, g: f32, b: f32, a: f32) Color {
        return .{ .Value = Vec4.init(r, g, b, a) };
    }
    pub fn initRGB(r: f32, g: f32, b: f32) Color {
        return initRGBA(r, g, b, 1.0);
    }

    pub fn initRGBAUnorm(r: u8, g: u8, b: u8, a: u8) Color {
        const inv_255: f32 = 1.0 / 255.0;
        return initRGBA(
            @intToFloat(f32, r) * inv_255,
            @intToFloat(f32, g) * inv_255,
            @intToFloat(f32, b) * inv_255,
            @intToFloat(f32, a) * inv_255,
        );
    }
    pub fn initRGBUnorm(r: u8, g: u8, b: u8) Color {
        const inv_255: f32 = 1.0 / 255.0;
        return initRGBA(
            @intToFloat(f32, r) * inv_255,
            @intToFloat(f32, g) * inv_255,
            @intToFloat(f32, b) * inv_255,
            1.0,
        );
    }

    /// Convert HSVA to RGBA color
    pub fn initHSVA(h: f32, s: f32, v: f32, a: f32) Color {
        var r: f32 = undefined;
        var g: f32 = undefined;
        var b: f32 = undefined;
        ColorConvertHSVtoRGB(h, s, v, &r, &g, &b);
        return initRGBA(r, g, b, a);
    }
    pub fn initHSV(h: f32, s: f32, v: f32) Color {
        return initHSVA(h, s, v, 1.0);
    }

    /// Convert an integer 0xaabbggrr to a floating point color
    pub fn initABGRPacked(value: u32) Color {
        return initRGBAUnorm(
            @truncate(u8, value >>  0),
            @truncate(u8, value >>  8),
            @truncate(u8, value >> 16),
            @truncate(u8, value >> 24),
        );
    }
    /// Convert from a floating point color to an integer 0xaabbggrr
    pub fn packABGR(self: Color) u32 {
        return ColorConvertFloat4ToU32(self.Value);
    }

    pub fn eql(self: Color, other: Color) bool {
        return self.Value.eql(other.Value);
    }
};

fn imguiZigAlloc(_: *anyopaque, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) std.mem.Allocator.Error![]u8 {
    _ = len_align; _ = ret_addr;
    assert(ptr_align <= @alignOf(*anyopaque)); // Alignment larger than pointers is not supported
    return @ptrCast([*]u8, raw.igMemAlloc(len) orelse return error.OutOfMemory)[0..len];
}
fn imguiZigResize(_: *anyopaque, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
    _ = len_align; _ = ret_addr;
    assert(buf_align <= @alignOf(*anyopaque)); // Alignment larger than pointers is not supported
    if (new_len > buf.len) return null;
    if (new_len == 0 and buf.len != 0) raw.igMemFree(buf.ptr);
    return new_len;
}
fn imguiZigFree(_: *anyopaque, buf: []u8, buf_align: u29, ret_addr: usize) void {
    _ = buf_align; _ = ret_addr;
    if (buf.len != 0) raw.igMemFree(buf.ptr);
}

const allocator_vtable: std.mem.Allocator.VTable = .{
    .alloc = imguiZigAlloc,
    .resize = imguiZigResize,
    .free = imguiZigFree,
};

pub const allocator: std.mem.Allocator = .{
    .ptr = undefined,
    .vtable = &allocator_vtable,
};

// ---------------- Everything above here comes from template.zig ------------------
// ---------------- Everything below here is generated -----------------------------

pub const TableColumnsSettings = opaque {};
pub const BitArrayForNamedKeys = BitArray<ImGuiKey_NamedKey_COUNT,-ImGuiKey_NamedKey_BEGIN>;
pub const DrawCallback = ?fn (parent_list: ?*const DrawList, cmd: ?*const DrawCmd) callconv(.C) void;
pub const DrawIdx = u16;
pub const FileHandle = [*c]anyopaque;
pub const ContextHookCallback = ?fn (ctx: ?*Context, hook: ?*ContextHook) callconv(.C) void;
pub const ErrorLogCallback = ?fn (user_data: ?*anyopaque, fmt: ?[*:0]const u8, ...) callconv(.C) void;
pub const ID = u32;
pub const InputTextCallback = ?fn (data: ?*InputTextCallbackData) callconv(.C) i32;
pub const MemAllocFunc = ?fn (sz: usize, user_data: ?*anyopaque) callconv(.C) ?*anyopaque;
pub const MemFreeFunc = ?fn (ptr: ?*anyopaque, user_data: ?*anyopaque) callconv(.C) void;
pub const SizeCallback = ?fn (data: ?*SizeCallbackData) callconv(.C) void;
pub const TableColumnIdx = i8;
pub const TableDrawChannelIdx = u8;
pub const PoolIdx = i32;
pub const TextureID = ?*anyopaque;
pub const Wchar = Wchar16;
pub const Wchar16 = u16;
pub const Wchar32 = u32;

pub const DrawFlagsInt = FlagsInt;
pub const DrawFlags = packed struct {
    Closed: bool = false,
    __reserved_bit_01: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    RoundCornersTopLeft: bool = false,
    RoundCornersTopRight: bool = false,
    RoundCornersBottomLeft: bool = false,
    RoundCornersBottomRight: bool = false,
    RoundCornersNone: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const RoundCornersTop: @This() = .{ .RoundCornersTopLeft=true, .RoundCornersTopRight=true };
    pub const RoundCornersBottom: @This() = .{ .RoundCornersBottomLeft=true, .RoundCornersBottomRight=true };
    pub const RoundCornersLeft: @This() = .{ .RoundCornersTopLeft=true, .RoundCornersBottomLeft=true };
    pub const RoundCornersRight: @This() = .{ .RoundCornersTopRight=true, .RoundCornersBottomRight=true };
    pub const RoundCornersAll: @This() = .{ .RoundCornersTopLeft=true, .RoundCornersTopRight=true, .RoundCornersBottomLeft=true, .RoundCornersBottomRight=true };
    pub const RoundCornersDefault_: @This() = .{ .RoundCornersTopLeft=true, .RoundCornersTopRight=true, .RoundCornersBottomLeft=true, .RoundCornersBottomRight=true };
    pub const RoundCornersMask_: @This() = .{ .RoundCornersTopLeft=true, .RoundCornersTopRight=true, .RoundCornersBottomLeft=true, .RoundCornersBottomRight=true, .RoundCornersNone=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const DrawListFlagsInt = FlagsInt;
pub const DrawListFlags = packed struct {
    AntiAliasedLines: bool = false,
    AntiAliasedLinesUseTex: bool = false,
    AntiAliasedFill: bool = false,
    AllowVtxOffset: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const FontAtlasFlagsInt = FlagsInt;
pub const FontAtlasFlags = packed struct {
    NoPowerOfTwoHeight: bool = false,
    NoMouseCursors: bool = false,
    NoBakedLines: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ActivateFlagsInt = FlagsInt;
pub const ActivateFlags = packed struct {
    PreferInput: bool = false,
    PreferTweak: bool = false,
    TryToPreserveState: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const BackendFlagsInt = FlagsInt;
pub const BackendFlags = packed struct {
    HasGamepad: bool = false,
    HasMouseCursors: bool = false,
    HasSetMousePos: bool = false,
    RendererHasVtxOffset: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ButtonFlagsInt = FlagsInt;
pub const ButtonFlags = packed struct {
    MouseButtonLeft: bool = false,
    MouseButtonRight: bool = false,
    MouseButtonMiddle: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const MouseButtonMask_: @This() = .{ .MouseButtonLeft=true, .MouseButtonRight=true, .MouseButtonMiddle=true };
    pub const MouseButtonDefault_: @This() = .{ .MouseButtonLeft=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const ColorEditFlagsInt = FlagsInt;
pub const ColorEditFlags = packed struct {
    __reserved_bit_00: bool = false,
    NoAlpha: bool = false,
    NoPicker: bool = false,
    NoOptions: bool = false,
    NoSmallPreview: bool = false,
    NoInputs: bool = false,
    NoTooltip: bool = false,
    NoLabel: bool = false,
    NoSidePreview: bool = false,
    NoDragDrop: bool = false,
    NoBorder: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    AlphaBar: bool = false,
    AlphaPreview: bool = false,
    AlphaPreviewHalf: bool = false,
    HDR: bool = false,
    DisplayRGB: bool = false,
    DisplayHSV: bool = false,
    DisplayHex: bool = false,
    Uint8: bool = false,
    Float: bool = false,
    PickerHueBar: bool = false,
    PickerHueWheel: bool = false,
    InputRGB: bool = false,
    InputHSV: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const DefaultOptions_: @This() = .{ .DisplayRGB=true, .Uint8=true, .PickerHueBar=true, .InputRGB=true };
    pub const DisplayMask_: @This() = .{ .DisplayRGB=true, .DisplayHSV=true, .DisplayHex=true };
    pub const DataTypeMask_: @This() = .{ .Uint8=true, .Float=true };
    pub const PickerMask_: @This() = .{ .PickerHueBar=true, .PickerHueWheel=true };
    pub const InputMask_: @This() = .{ .InputRGB=true, .InputHSV=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const ComboFlagsInt = FlagsInt;
pub const ComboFlags = packed struct {
    PopupAlignLeft: bool = false,
    HeightSmall: bool = false,
    HeightRegular: bool = false,
    HeightLarge: bool = false,
    HeightLargest: bool = false,
    NoArrowButton: bool = false,
    NoPreview: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const HeightMask_: @This() = .{ .HeightSmall=true, .HeightRegular=true, .HeightLarge=true, .HeightLargest=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const CondFlagsInt = FlagsInt;
pub const CondFlags = packed struct {
    Always: bool = false,
    Once: bool = false,
    FirstUseEver: bool = false,
    Appearing: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ConfigFlagsInt = FlagsInt;
pub const ConfigFlags = packed struct {
    NavEnableKeyboard: bool = false,
    NavEnableGamepad: bool = false,
    NavEnableSetMousePos: bool = false,
    NavNoCaptureKeyboard: bool = false,
    NoMouse: bool = false,
    NoMouseCursorChange: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    IsSRGB: bool = false,
    IsTouchScreen: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const DebugLogFlagsInt = FlagsInt;
pub const DebugLogFlags = packed struct {
    EventActiveId: bool = false,
    EventFocus: bool = false,
    EventPopup: bool = false,
    EventNav: bool = false,
    EventIO: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    OutputToTTY: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const EventMask_: @This() = .{ .EventActiveId=true, .EventFocus=true, .EventPopup=true, .EventNav=true, .EventIO=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const DragDropFlagsInt = FlagsInt;
pub const DragDropFlags = packed struct {
    SourceNoPreviewTooltip: bool = false,
    SourceNoDisableHover: bool = false,
    SourceNoHoldToOpenOthers: bool = false,
    SourceAllowNullID: bool = false,
    SourceExtern: bool = false,
    SourceAutoExpirePayload: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    AcceptBeforeDelivery: bool = false,
    AcceptNoDrawDefaultRect: bool = false,
    AcceptNoPreviewTooltip: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const AcceptPeekOnly: @This() = .{ .AcceptBeforeDelivery=true, .AcceptNoDrawDefaultRect=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const FocusedFlagsInt = FlagsInt;
pub const FocusedFlags = packed struct {
    ChildWindows: bool = false,
    RootWindow: bool = false,
    AnyWindow: bool = false,
    NoPopupHierarchy: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const RootAndChildWindows: @This() = .{ .ChildWindows=true, .RootWindow=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const HoveredFlagsInt = FlagsInt;
pub const HoveredFlags = packed struct {
    ChildWindows: bool = false,
    RootWindow: bool = false,
    AnyWindow: bool = false,
    NoPopupHierarchy: bool = false,
    __reserved_bit_04: bool = false,
    AllowWhenBlockedByPopup: bool = false,
    __reserved_bit_06: bool = false,
    AllowWhenBlockedByActiveItem: bool = false,
    AllowWhenOverlapped: bool = false,
    AllowWhenDisabled: bool = false,
    NoNavOverride: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const RectOnly: @This() = .{ .AllowWhenBlockedByPopup=true, .AllowWhenBlockedByActiveItem=true, .AllowWhenOverlapped=true };
    pub const RootAndChildWindows: @This() = .{ .ChildWindows=true, .RootWindow=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const InputTextFlagsInt = FlagsInt;
pub const InputTextFlags = packed struct {
    CharsDecimal: bool = false,
    CharsHexadecimal: bool = false,
    CharsUppercase: bool = false,
    CharsNoBlank: bool = false,
    AutoSelectAll: bool = false,
    EnterReturnsTrue: bool = false,
    CallbackCompletion: bool = false,
    CallbackHistory: bool = false,
    CallbackAlways: bool = false,
    CallbackCharFilter: bool = false,
    AllowTabInput: bool = false,
    CtrlEnterForNewLine: bool = false,
    NoHorizontalScroll: bool = false,
    AlwaysOverwrite: bool = false,
    ReadOnly: bool = false,
    Password: bool = false,
    NoUndoRedo: bool = false,
    CharsScientific: bool = false,
    CallbackResize: bool = false,
    CallbackEdit: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ItemFlagsInt = FlagsInt;
pub const ItemFlags = packed struct {
    NoTabStop: bool = false,
    ButtonRepeat: bool = false,
    Disabled: bool = false,
    NoNav: bool = false,
    NoNavDefaultFocus: bool = false,
    SelectableDontClosePopup: bool = false,
    MixedValue: bool = false,
    ReadOnly: bool = false,
    Inputable: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ItemStatusFlagsInt = FlagsInt;
pub const ItemStatusFlags = packed struct {
    HoveredRect: bool = false,
    HasDisplayRect: bool = false,
    Edited: bool = false,
    ToggledSelection: bool = false,
    ToggledOpen: bool = false,
    HasDeactivated: bool = false,
    Deactivated: bool = false,
    HoveredWindow: bool = false,
    FocusedByTabbing: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const ModFlagsInt = FlagsInt;
pub const ModFlags = packed struct {
    Ctrl: bool = false,
    Shift: bool = false,
    Alt: bool = false,
    Super: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const NavDirSourceFlagsInt = FlagsInt;
pub const NavDirSourceFlags = packed struct {
    RawKeyboard: bool = false,
    Keyboard: bool = false,
    PadDPad: bool = false,
    PadLStick: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const NavHighlightFlagsInt = FlagsInt;
pub const NavHighlightFlags = packed struct {
    TypeDefault: bool = false,
    TypeThin: bool = false,
    AlwaysDraw: bool = false,
    NoRounding: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const NavMoveFlagsInt = FlagsInt;
pub const NavMoveFlags = packed struct {
    LoopX: bool = false,
    LoopY: bool = false,
    WrapX: bool = false,
    WrapY: bool = false,
    AllowCurrentNavId: bool = false,
    AlsoScoreVisibleSet: bool = false,
    ScrollToEdgeY: bool = false,
    Forwarded: bool = false,
    DebugNoResult: bool = false,
    FocusApi: bool = false,
    Tabbing: bool = false,
    Activate: bool = false,
    DontSetNavHighlight: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const NextItemDataFlagsInt = FlagsInt;
pub const NextItemDataFlags = packed struct {
    HasWidth: bool = false,
    HasOpen: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const NextWindowDataFlagsInt = FlagsInt;
pub const NextWindowDataFlags = packed struct {
    HasPos: bool = false,
    HasSize: bool = false,
    HasContentSize: bool = false,
    HasCollapsed: bool = false,
    HasSizeConstraint: bool = false,
    HasFocus: bool = false,
    HasBgAlpha: bool = false,
    HasScroll: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const OldColumnFlagsInt = FlagsInt;
pub const OldColumnFlags = packed struct {
    NoBorder: bool = false,
    NoResize: bool = false,
    NoPreserveWidths: bool = false,
    NoForceWithinWindow: bool = false,
    GrowParentContentsSize: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const PopupFlagsInt = FlagsInt;
pub const PopupFlags = packed struct {
    MouseButtonRight: bool = false,
    MouseButtonMiddle: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    NoOpenOverExistingPopup: bool = false,
    NoOpenOverItems: bool = false,
    AnyPopupId: bool = false,
    AnyPopupLevel: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const MouseButtonLeft: @This() = .{};
    pub const MouseButtonMask_: @This() = .{ .MouseButtonRight=true, .MouseButtonMiddle=true, .__reserved_bit_02=true, .__reserved_bit_03=true, .__reserved_bit_04=true };
    pub const MouseButtonDefault_: @This() = .{ .MouseButtonRight=true };
    pub const AnyPopup: @This() = .{ .AnyPopupId=true, .AnyPopupLevel=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const ScrollFlagsInt = FlagsInt;
pub const ScrollFlags = packed struct {
    KeepVisibleEdgeX: bool = false,
    KeepVisibleEdgeY: bool = false,
    KeepVisibleCenterX: bool = false,
    KeepVisibleCenterY: bool = false,
    AlwaysCenterX: bool = false,
    AlwaysCenterY: bool = false,
    NoScrollParent: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const MaskX_: @This() = .{ .KeepVisibleEdgeX=true, .KeepVisibleCenterX=true, .AlwaysCenterX=true };
    pub const MaskY_: @This() = .{ .KeepVisibleEdgeY=true, .KeepVisibleCenterY=true, .AlwaysCenterY=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const SelectableFlagsInt = FlagsInt;
pub const SelectableFlags = packed struct {
    DontClosePopups: bool = false,
    SpanAllColumns: bool = false,
    AllowDoubleClick: bool = false,
    Disabled: bool = false,
    AllowItemOverlap: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const SeparatorFlagsInt = FlagsInt;
pub const SeparatorFlags = packed struct {
    Horizontal: bool = false,
    Vertical: bool = false,
    SpanAllColumns: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const SliderFlagsInt = FlagsInt;
pub const SliderFlags = packed struct {
    __reserved_bit_00: bool = false,
    __reserved_bit_01: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    AlwaysClamp: bool = false,
    Logarithmic: bool = false,
    NoRoundToFormat: bool = false,
    NoInput: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const InvalidMask_: @This() = .{ .__reserved_bit_00=true, .__reserved_bit_01=true, .__reserved_bit_02=true, .__reserved_bit_03=true, .__reserved_bit_28=true, .__reserved_bit_29=true, .__reserved_bit_30=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const TabBarFlagsInt = FlagsInt;
pub const TabBarFlags = packed struct {
    Reorderable: bool = false,
    AutoSelectNewTabs: bool = false,
    TabListPopupButton: bool = false,
    NoCloseWithMiddleMouseButton: bool = false,
    NoTabListScrollingButtons: bool = false,
    NoTooltip: bool = false,
    FittingPolicyResizeDown: bool = false,
    FittingPolicyScroll: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const FittingPolicyMask_: @This() = .{ .FittingPolicyResizeDown=true, .FittingPolicyScroll=true };
    pub const FittingPolicyDefault_: @This() = .{ .FittingPolicyResizeDown=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const TabItemFlagsInt = FlagsInt;
pub const TabItemFlags = packed struct {
    UnsavedDocument: bool = false,
    SetSelected: bool = false,
    NoCloseWithMiddleMouseButton: bool = false,
    NoPushId: bool = false,
    NoTooltip: bool = false,
    NoReorder: bool = false,
    Leading: bool = false,
    Trailing: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const TableColumnFlagsInt = FlagsInt;
pub const TableColumnFlags = packed struct {
    Disabled: bool = false,
    DefaultHide: bool = false,
    DefaultSort: bool = false,
    WidthStretch: bool = false,
    WidthFixed: bool = false,
    NoResize: bool = false,
    NoReorder: bool = false,
    NoHide: bool = false,
    NoClip: bool = false,
    NoSort: bool = false,
    NoSortAscending: bool = false,
    NoSortDescending: bool = false,
    NoHeaderLabel: bool = false,
    NoHeaderWidth: bool = false,
    PreferSortAscending: bool = false,
    PreferSortDescending: bool = false,
    IndentEnable: bool = false,
    IndentDisable: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    IsEnabled: bool = false,
    IsVisible: bool = false,
    IsSorted: bool = false,
    IsHovered: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    NoDirectResize_: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const WidthMask_: @This() = .{ .WidthStretch=true, .WidthFixed=true };
    pub const IndentMask_: @This() = .{ .IndentEnable=true, .IndentDisable=true };
    pub const StatusMask_: @This() = .{ .IsEnabled=true, .IsVisible=true, .IsSorted=true, .IsHovered=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const TableFlagsInt = FlagsInt;
pub const TableFlags = packed struct {
    Resizable: bool = false,
    Reorderable: bool = false,
    Hideable: bool = false,
    Sortable: bool = false,
    NoSavedSettings: bool = false,
    ContextMenuInBody: bool = false,
    RowBg: bool = false,
    BordersInnerH: bool = false,
    BordersOuterH: bool = false,
    BordersInnerV: bool = false,
    BordersOuterV: bool = false,
    NoBordersInBody: bool = false,
    NoBordersInBodyUntilResize: bool = false,
    SizingFixedFit: bool = false,
    SizingFixedSame: bool = false,
    SizingStretchSame: bool = false,
    NoHostExtendX: bool = false,
    NoHostExtendY: bool = false,
    NoKeepColumnsVisible: bool = false,
    PreciseWidths: bool = false,
    NoClip: bool = false,
    PadOuterX: bool = false,
    NoPadOuterX: bool = false,
    NoPadInnerX: bool = false,
    ScrollX: bool = false,
    ScrollY: bool = false,
    SortMulti: bool = false,
    SortTristate: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const BordersH: @This() = .{ .BordersInnerH=true, .BordersOuterH=true };
    pub const BordersV: @This() = .{ .BordersInnerV=true, .BordersOuterV=true };
    pub const BordersInner: @This() = .{ .BordersInnerH=true, .BordersInnerV=true };
    pub const BordersOuter: @This() = .{ .BordersOuterH=true, .BordersOuterV=true };
    pub const Borders: @This() = .{ .BordersInnerH=true, .BordersOuterH=true, .BordersInnerV=true, .BordersOuterV=true };
    pub const SizingStretchProp: @This() = .{ .SizingFixedFit=true, .SizingFixedSame=true };
    pub const SizingMask_: @This() = .{ .SizingFixedFit=true, .SizingFixedSame=true, .SizingStretchSame=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const TableRowFlagsInt = FlagsInt;
pub const TableRowFlags = packed struct {
    Headers: bool = false,
    __reserved_bit_01: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const TextFlagsInt = FlagsInt;
pub const TextFlags = packed struct {
    NoWidthForLargeClippedText: bool = false,
    __reserved_bit_01: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const TooltipFlagsInt = FlagsInt;
pub const TooltipFlags = packed struct {
    OverridePreviousTooltip: bool = false,
    __reserved_bit_01: bool = false,
    __reserved_bit_02: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const TreeNodeFlagsInt = FlagsInt;
pub const TreeNodeFlags = packed struct {
    Selected: bool = false,
    Framed: bool = false,
    AllowItemOverlap: bool = false,
    NoTreePushOnOpen: bool = false,
    NoAutoOpenOnLog: bool = false,
    DefaultOpen: bool = false,
    OpenOnDoubleClick: bool = false,
    OpenOnArrow: bool = false,
    Leaf: bool = false,
    Bullet: bool = false,
    FramePadding: bool = false,
    SpanAvailWidth: bool = false,
    SpanFullWidth: bool = false,
    NavLeftJumpsBackHere: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const CollapsingHeader: @This() = .{ .Framed=true, .NoTreePushOnOpen=true, .NoAutoOpenOnLog=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const ViewportFlagsInt = FlagsInt;
pub const ViewportFlags = packed struct {
    IsPlatformWindow: bool = false,
    IsPlatformMonitor: bool = false,
    OwnedByApp: bool = false,
    __reserved_bit_03: bool = false,
    __reserved_bit_04: bool = false,
    __reserved_bit_05: bool = false,
    __reserved_bit_06: bool = false,
    __reserved_bit_07: bool = false,
    __reserved_bit_08: bool = false,
    __reserved_bit_09: bool = false,
    __reserved_bit_10: bool = false,
    __reserved_bit_11: bool = false,
    __reserved_bit_12: bool = false,
    __reserved_bit_13: bool = false,
    __reserved_bit_14: bool = false,
    __reserved_bit_15: bool = false,
    __reserved_bit_16: bool = false,
    __reserved_bit_17: bool = false,
    __reserved_bit_18: bool = false,
    __reserved_bit_19: bool = false,
    __reserved_bit_20: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    __reserved_bit_23: bool = false,
    __reserved_bit_24: bool = false,
    __reserved_bit_25: bool = false,
    __reserved_bit_26: bool = false,
    __reserved_bit_27: bool = false,
    __reserved_bit_28: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};

    pub usingnamespace FlagsMixin(@This());
};

pub const WindowFlagsInt = FlagsInt;
pub const WindowFlags = packed struct {
    NoTitleBar: bool = false,
    NoResize: bool = false,
    NoMove: bool = false,
    NoScrollbar: bool = false,
    NoScrollWithMouse: bool = false,
    NoCollapse: bool = false,
    AlwaysAutoResize: bool = false,
    NoBackground: bool = false,
    NoSavedSettings: bool = false,
    NoMouseInputs: bool = false,
    MenuBar: bool = false,
    HorizontalScrollbar: bool = false,
    NoFocusOnAppearing: bool = false,
    NoBringToFrontOnFocus: bool = false,
    AlwaysVerticalScrollbar: bool = false,
    AlwaysHorizontalScrollbar: bool = false,
    AlwaysUseWindowPadding: bool = false,
    __reserved_bit_17: bool = false,
    NoNavInputs: bool = false,
    NoNavFocus: bool = false,
    UnsavedDocument: bool = false,
    __reserved_bit_21: bool = false,
    __reserved_bit_22: bool = false,
    NavFlattened: bool = false,
    ChildWindow: bool = false,
    Tooltip: bool = false,
    Popup: bool = false,
    Modal: bool = false,
    ChildMenu: bool = false,
    __reserved_bit_29: bool = false,
    __reserved_bit_30: bool = false,
    __reserved_bit_31: bool = false,

    pub const None: @This() = .{};
    pub const NoNav: @This() = .{ .NoNavInputs=true, .NoNavFocus=true };
    pub const NoDecoration: @This() = .{ .NoTitleBar=true, .NoResize=true, .NoScrollbar=true, .NoCollapse=true };
    pub const NoInputs: @This() = .{ .NoMouseInputs=true, .NoNavInputs=true, .NoNavFocus=true };

    pub usingnamespace FlagsMixin(@This());
};

pub const Axis = enum (i32) {
    None = -1,
    X = 0,
    Y = 1,
    _,
};

pub const ButtonFlagsPrivate = enum (i32) {
    ImGuiButtonFlags_PressedOnClick = 1 << 4,
    ImGuiButtonFlags_PressedOnClickRelease = 1 << 5,
    ImGuiButtonFlags_PressedOnClickReleaseAnywhere = 1 << 6,
    ImGuiButtonFlags_PressedOnRelease = 1 << 7,
    ImGuiButtonFlags_PressedOnDoubleClick = 1 << 8,
    ImGuiButtonFlags_PressedOnDragDropHold = 1 << 9,
    ImGuiButtonFlags_Repeat = 1 << 10,
    ImGuiButtonFlags_FlattenChildren = 1 << 11,
    ImGuiButtonFlags_AllowItemOverlap = 1 << 12,
    ImGuiButtonFlags_DontClosePopups = 1 << 13,
    ImGuiButtonFlags_AlignTextBaseLine = 1 << 15,
    ImGuiButtonFlags_NoKeyModifiers = 1 << 16,
    ImGuiButtonFlags_NoHoldingActiveId = 1 << 17,
    ImGuiButtonFlags_NoNavFocus = 1 << 18,
    ImGuiButtonFlags_NoHoveredOnFocus = 1 << 19,
    ImGuiButtonFlags_PressedOnMask_ = ImGuiButtonFlags_PressedOnClick | ImGuiButtonFlags_PressedOnClickRelease | ImGuiButtonFlags_PressedOnClickReleaseAnywhere | ImGuiButtonFlags_PressedOnRelease | ImGuiButtonFlags_PressedOnDoubleClick | ImGuiButtonFlags_PressedOnDragDropHold,
    ImGuiButtonFlags_PressedOnDefault_ = ImGuiButtonFlags_PressedOnClickRelease,
    _,
};

pub const Col = enum (i32) {
    Text = 0,
    TextDisabled = 1,
    WindowBg = 2,
    ChildBg = 3,
    PopupBg = 4,
    Border = 5,
    BorderShadow = 6,
    FrameBg = 7,
    FrameBgHovered = 8,
    FrameBgActive = 9,
    TitleBg = 10,
    TitleBgActive = 11,
    TitleBgCollapsed = 12,
    MenuBarBg = 13,
    ScrollbarBg = 14,
    ScrollbarGrab = 15,
    ScrollbarGrabHovered = 16,
    ScrollbarGrabActive = 17,
    CheckMark = 18,
    SliderGrab = 19,
    SliderGrabActive = 20,
    Button = 21,
    ButtonHovered = 22,
    ButtonActive = 23,
    Header = 24,
    HeaderHovered = 25,
    HeaderActive = 26,
    Separator = 27,
    SeparatorHovered = 28,
    SeparatorActive = 29,
    ResizeGrip = 30,
    ResizeGripHovered = 31,
    ResizeGripActive = 32,
    Tab = 33,
    TabHovered = 34,
    TabActive = 35,
    TabUnfocused = 36,
    TabUnfocusedActive = 37,
    PlotLines = 38,
    PlotLinesHovered = 39,
    PlotHistogram = 40,
    PlotHistogramHovered = 41,
    TableHeaderBg = 42,
    TableBorderStrong = 43,
    TableBorderLight = 44,
    TableRowBg = 45,
    TableRowBgAlt = 46,
    TextSelectedBg = 47,
    DragDropTarget = 48,
    NavHighlight = 49,
    NavWindowingHighlight = 50,
    NavWindowingDimBg = 51,
    ModalWindowDimBg = 52,
    _,

    pub const COUNT = 53;
};

pub const ComboFlagsPrivate = enum (i32) {
    ImGuiComboFlags_CustomPreview = 1 << 20,
    _,
};

pub const ContextHookType = enum (i32) {
    NewFramePre = 0,
    NewFramePost = 1,
    EndFramePre = 2,
    EndFramePost = 3,
    RenderPre = 4,
    RenderPost = 5,
    Shutdown = 6,
    PendingRemoval_ = 7,
    _,
};

pub const DataTypePrivate = enum (i32) {
    ImGuiDataType_String = ImGuiDataType_COUNT + 1,
    ImGuiDataType_Pointer = ImGuiDataType_COUNT + 1+1,
    ImGuiDataType_ID = ImGuiDataType_COUNT + 1+1+1,
    _,
};

pub const DataType = enum (i32) {
    S8 = 0,
    U8 = 1,
    S16 = 2,
    U16 = 3,
    S32 = 4,
    U32 = 5,
    S64 = 6,
    U64 = 7,
    Float = 8,
    Double = 9,
    _,

    pub const COUNT = 10;
};

pub const Dir = enum (i32) {
    None = -1,
    Left = 0,
    Right = 1,
    Up = 2,
    Down = 3,
    _,

    pub const COUNT = 4;
};

pub const InputEventType = enum (i32) {
    None = 0,
    MousePos = 1,
    MouseWheel = 2,
    MouseButton = 3,
    Key = 4,
    Text = 5,
    Focus = 6,
    _,

    pub const COUNT = 7;
};

pub const InputSource = enum (i32) {
    None = 0,
    Mouse = 1,
    Keyboard = 2,
    Gamepad = 3,
    Clipboard = 4,
    Nav = 5,
    _,

    pub const COUNT = 6;
};

pub const InputTextFlagsPrivate = enum (i32) {
    ImGuiInputTextFlags_Multiline = 1 << 26,
    ImGuiInputTextFlags_NoMarkEdited = 1 << 27,
    ImGuiInputTextFlags_MergedItem = 1 << 28,
    _,
};

pub const KeyPrivate = enum (i32) {
    _,

    pub const ImGuiKey_LegacyNativeKey_BEGIN = 0;
    pub const ImGuiKey_LegacyNativeKey_END = 512;
    pub const ImGuiKey_Gamepad_BEGIN = ImGuiKey_GamepadStart;
    pub const ImGuiKey_Gamepad_END = ImGuiKey_GamepadRStickRight + 1;
};

pub const Key = enum (i32) {
    None = 0,
    Tab = 512,
    LeftArrow = 513,
    RightArrow = 514,
    UpArrow = 515,
    DownArrow = 516,
    PageUp = 517,
    PageDown = 518,
    Home = 519,
    End = 520,
    Insert = 521,
    Delete = 522,
    Backspace = 523,
    Space = 524,
    Enter = 525,
    Escape = 526,
    LeftCtrl = 527,
    LeftShift = 528,
    LeftAlt = 529,
    LeftSuper = 530,
    RightCtrl = 531,
    RightShift = 532,
    RightAlt = 533,
    RightSuper = 534,
    Menu = 535,
    @"0" = 536,
    @"1" = 537,
    @"2" = 538,
    @"3" = 539,
    @"4" = 540,
    @"5" = 541,
    @"6" = 542,
    @"7" = 543,
    @"8" = 544,
    @"9" = 545,
    A = 546,
    B = 547,
    C = 548,
    D = 549,
    E = 550,
    F = 551,
    G = 552,
    H = 553,
    I = 554,
    J = 555,
    K = 556,
    L = 557,
    M = 558,
    N = 559,
    O = 560,
    P = 561,
    Q = 562,
    R = 563,
    S = 564,
    T = 565,
    U = 566,
    V = 567,
    W = 568,
    X = 569,
    Y = 570,
    Z = 571,
    F1 = 572,
    F2 = 573,
    F3 = 574,
    F4 = 575,
    F5 = 576,
    F6 = 577,
    F7 = 578,
    F8 = 579,
    F9 = 580,
    F10 = 581,
    F11 = 582,
    F12 = 583,
    Apostrophe = 584,
    Comma = 585,
    Minus = 586,
    Period = 587,
    Slash = 588,
    Semicolon = 589,
    Equal = 590,
    LeftBracket = 591,
    Backslash = 592,
    RightBracket = 593,
    GraveAccent = 594,
    CapsLock = 595,
    ScrollLock = 596,
    NumLock = 597,
    PrintScreen = 598,
    Pause = 599,
    Keypad0 = 600,
    Keypad1 = 601,
    Keypad2 = 602,
    Keypad3 = 603,
    Keypad4 = 604,
    Keypad5 = 605,
    Keypad6 = 606,
    Keypad7 = 607,
    Keypad8 = 608,
    Keypad9 = 609,
    KeypadDecimal = 610,
    KeypadDivide = 611,
    KeypadMultiply = 612,
    KeypadSubtract = 613,
    KeypadAdd = 614,
    KeypadEnter = 615,
    KeypadEqual = 616,
    GamepadStart = 617,
    GamepadBack = 618,
    GamepadFaceUp = 619,
    GamepadFaceDown = 620,
    GamepadFaceLeft = 621,
    GamepadFaceRight = 622,
    GamepadDpadUp = 623,
    GamepadDpadDown = 624,
    GamepadDpadLeft = 625,
    GamepadDpadRight = 626,
    GamepadL1 = 627,
    GamepadR1 = 628,
    GamepadL2 = 629,
    GamepadR2 = 630,
    GamepadL3 = 631,
    GamepadR3 = 632,
    GamepadLStickUp = 633,
    GamepadLStickDown = 634,
    GamepadLStickLeft = 635,
    GamepadLStickRight = 636,
    GamepadRStickUp = 637,
    GamepadRStickDown = 638,
    GamepadRStickLeft = 639,
    GamepadRStickRight = 640,
    ModCtrl = 641,
    ModShift = 642,
    ModAlt = 643,
    ModSuper = 644,
    _,

    pub const COUNT = 645;
    pub const NamedKey_BEGIN = 512;
    pub const NamedKey_END = @This().COUNT;
    pub const NamedKey_COUNT = @This().NamedKey_END - @This().NamedKey_BEGIN;
    pub const KeysData_SIZE = @This().NamedKey_COUNT;
    pub const KeysData_OFFSET = @This().NamedKey_BEGIN;
};

pub const LayoutType = enum (i32) {
    Horizontal = 0,
    Vertical = 1,
    _,
};

pub const LogType = enum (i32) {
    None = 0,
    TTY = 1,
    File = 2,
    Buffer = 3,
    Clipboard = 4,
    _,
};

pub const MouseButton = enum (i32) {
    Left = 0,
    Right = 1,
    Middle = 2,
    _,

    pub const COUNT = 5;
};

pub const MouseCursor = enum (i32) {
    None = -1,
    Arrow = 0,
    TextInput = 1,
    ResizeAll = 2,
    ResizeNS = 3,
    ResizeEW = 4,
    ResizeNESW = 5,
    ResizeNWSE = 6,
    Hand = 7,
    NotAllowed = 8,
    _,

    pub const COUNT = 9;
};

pub const NavInput = enum (i32) {
    Activate = 0,
    Cancel = 1,
    Input = 2,
    Menu = 3,
    DpadLeft = 4,
    DpadRight = 5,
    DpadUp = 6,
    DpadDown = 7,
    LStickLeft = 8,
    LStickRight = 9,
    LStickUp = 10,
    LStickDown = 11,
    FocusPrev = 12,
    FocusNext = 13,
    TweakSlow = 14,
    TweakFast = 15,
    KeyLeft_ = 16,
    KeyRight_ = 17,
    KeyUp_ = 18,
    KeyDown_ = 19,
    _,

    pub const COUNT = 20;
};

pub const NavLayer = enum (i32) {
    Main = 0,
    Menu = 1,
    _,

    pub const COUNT = 2;
};

pub const NavReadMode = enum (i32) {
    Down = 0,
    Pressed = 1,
    Released = 2,
    Repeat = 3,
    RepeatSlow = 4,
    RepeatFast = 5,
    _,
};

pub const PlotType = enum (i32) {
    Lines = 0,
    Histogram = 1,
    _,
};

pub const PopupPositionPolicy = enum (i32) {
    Default = 0,
    ComboBox = 1,
    Tooltip = 2,
    _,
};

pub const SelectableFlagsPrivate = enum (i32) {
    ImGuiSelectableFlags_NoHoldingActiveID = 1 << 20,
    ImGuiSelectableFlags_SelectOnNav = 1 << 21,
    ImGuiSelectableFlags_SelectOnClick = 1 << 22,
    ImGuiSelectableFlags_SelectOnRelease = 1 << 23,
    ImGuiSelectableFlags_SpanAvailWidth = 1 << 24,
    ImGuiSelectableFlags_DrawHoveredWhenHeld = 1 << 25,
    ImGuiSelectableFlags_SetNavIdOnHover = 1 << 26,
    ImGuiSelectableFlags_NoPadWithHalfSpacing = 1 << 27,
    _,
};

pub const SliderFlagsPrivate = enum (i32) {
    ImGuiSliderFlags_Vertical = 1 << 20,
    ImGuiSliderFlags_ReadOnly = 1 << 21,
    _,
};

pub const SortDirection = enum (i32) {
    None = 0,
    Ascending = 1,
    Descending = 2,
    _,
};

pub const StyleVar = enum (i32) {
    Alpha = 0,
    DisabledAlpha = 1,
    WindowPadding = 2,
    WindowRounding = 3,
    WindowBorderSize = 4,
    WindowMinSize = 5,
    WindowTitleAlign = 6,
    ChildRounding = 7,
    ChildBorderSize = 8,
    PopupRounding = 9,
    PopupBorderSize = 10,
    FramePadding = 11,
    FrameRounding = 12,
    FrameBorderSize = 13,
    ItemSpacing = 14,
    ItemInnerSpacing = 15,
    IndentSpacing = 16,
    CellPadding = 17,
    ScrollbarSize = 18,
    ScrollbarRounding = 19,
    GrabMinSize = 20,
    GrabRounding = 21,
    TabRounding = 22,
    ButtonTextAlign = 23,
    SelectableTextAlign = 24,
    _,

    pub const COUNT = 25;
};

pub const TabBarFlagsPrivate = enum (i32) {
    ImGuiTabBarFlags_DockNode = 1 << 20,
    ImGuiTabBarFlags_IsFocused = 1 << 21,
    ImGuiTabBarFlags_SaveSettings = 1 << 22,
    _,
};

pub const TabItemFlagsPrivate = enum (i32) {
    ImGuiTabItemFlags_SectionMask_ = ImGuiTabItemFlags_Leading | ImGuiTabItemFlags_Trailing,
    ImGuiTabItemFlags_NoCloseButton = 1 << 20,
    ImGuiTabItemFlags_Button = 1 << 21,
    _,
};

pub const TableBgTarget = enum (i32) {
    None = 0,
    RowBg0 = 1,
    RowBg1 = 2,
    CellBg = 3,
    _,
};

pub const TreeNodeFlagsPrivate = enum (i32) {
    ImGuiTreeNodeFlags_ClipLabelForTrailingButton = 1 << 20,
    _,
};

pub const BitVector = extern struct {
    Storage: Vector(u32),

    /// Clear(self: *BitVector) void
    pub const Clear = raw.ImBitVector_Clear;

    /// ClearBit(self: *BitVector, n: i32) void
    pub const ClearBit = raw.ImBitVector_ClearBit;

    /// Create(self: *BitVector, sz: i32) void
    pub const Create = raw.ImBitVector_Create;

    /// SetBit(self: *BitVector, n: i32) void
    pub const SetBit = raw.ImBitVector_SetBit;

    /// TestBit(self: *const BitVector, n: i32) bool
    pub const TestBit = raw.ImBitVector_TestBit;
};

pub const DrawChannel = extern struct {
    _CmdBuffer: Vector(DrawCmd),
    _IdxBuffer: Vector(DrawIdx),
};

pub const DrawCmd = extern struct {
    ClipRect: Vec4,
    TextureId: TextureID,
    VtxOffset: u32,
    IdxOffset: u32,
    ElemCount: u32,
    UserCallback: DrawCallback,
    UserCallbackData: ?*anyopaque,

    /// GetTexID(self: *const DrawCmd) TextureID
    pub const GetTexID = raw.ImDrawCmd_GetTexID;

    /// init_ImDrawCmd(self: ?*anyopaque) void
    pub const init_ImDrawCmd = raw.ImDrawCmd_ImDrawCmd;

    /// deinit(self: *DrawCmd) void
    pub const deinit = raw.ImDrawCmd_destroy;
};

pub const DrawCmdHeader = extern struct {
    ClipRect: Vec4,
    TextureId: TextureID,
    VtxOffset: u32,
};

pub const DrawData = extern struct {
    Valid: bool,
    CmdListsCount: i32,
    TotalIdxCount: i32,
    TotalVtxCount: i32,
    CmdLists: ?[*]*DrawList,
    DisplayPos: Vec2,
    DisplaySize: Vec2,
    FramebufferScale: Vec2,

    /// Clear(self: *DrawData) void
    pub const Clear = raw.ImDrawData_Clear;

    /// DeIndexAllBuffers(self: *DrawData) void
    pub const DeIndexAllBuffers = raw.ImDrawData_DeIndexAllBuffers;

    /// init_ImDrawData(self: ?*anyopaque) void
    pub const init_ImDrawData = raw.ImDrawData_ImDrawData;

    pub inline fn ScaleClipRects(self: *DrawData, fb_scale: Vec2) void {
        return raw.ImDrawData_ScaleClipRects(self, &fb_scale);
    }

    /// deinit(self: *DrawData) void
    pub const deinit = raw.ImDrawData_destroy;
};

pub const DrawDataBuilder = extern struct {
    Layers: [2]Vector(?*DrawList),

    /// Clear(self: *DrawDataBuilder) void
    pub const Clear = raw.ImDrawDataBuilder_Clear;

    /// ClearFreeMemory(self: *DrawDataBuilder) void
    pub const ClearFreeMemory = raw.ImDrawDataBuilder_ClearFreeMemory;

    /// FlattenIntoSingleLayer(self: *DrawDataBuilder) void
    pub const FlattenIntoSingleLayer = raw.ImDrawDataBuilder_FlattenIntoSingleLayer;

    /// GetDrawListCount(self: *const DrawDataBuilder) i32
    pub const GetDrawListCount = raw.ImDrawDataBuilder_GetDrawListCount;
};

pub const DrawList = extern struct {
    CmdBuffer: Vector(DrawCmd),
    IdxBuffer: Vector(DrawIdx),
    VtxBuffer: Vector(DrawVert),
    Flags: DrawListFlags align(4),
    _VtxCurrentIdx: u32,
    _Data: ?*const DrawListSharedData,
    _OwnerName: ?[*:0]const u8,
    _VtxWritePtr: ?[*]DrawVert,
    _IdxWritePtr: ?[*]DrawIdx,
    _ClipRectStack: Vector(Vec4),
    _TextureIdStack: Vector(TextureID),
    _Path: Vector(Vec2),
    _CmdHeader: DrawCmdHeader,
    _Splitter: DrawListSplitter,
    _FringeScale: f32,

    pub inline fn AddBezierCubicExt(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, col: u32, thickness: f32, num_segments: i32) void {
        return raw.ImDrawList_AddBezierCubic(self, &p1, &p2, &p3, &p4, col, thickness, num_segments);
    }
    pub inline fn AddBezierCubic(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, col: u32, thickness: f32) void {
        return @This().AddBezierCubicExt(self, p1, p2, p3, p4, col, thickness, 0);
    }

    pub inline fn AddBezierQuadraticExt(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, col: u32, thickness: f32, num_segments: i32) void {
        return raw.ImDrawList_AddBezierQuadratic(self, &p1, &p2, &p3, col, thickness, num_segments);
    }
    pub inline fn AddBezierQuadratic(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, col: u32, thickness: f32) void {
        return @This().AddBezierQuadraticExt(self, p1, p2, p3, col, thickness, 0);
    }

    /// AddCallback(self: *DrawList, callback: DrawCallback, callback_data: ?*anyopaque) void
    pub const AddCallback = raw.ImDrawList_AddCallback;

    pub inline fn AddCircleExt(self: *DrawList, center: Vec2, radius: f32, col: u32, num_segments: i32, thickness: f32) void {
        return raw.ImDrawList_AddCircle(self, &center, radius, col, num_segments, thickness);
    }
    pub inline fn AddCircle(self: *DrawList, center: Vec2, radius: f32, col: u32) void {
        return @This().AddCircleExt(self, center, radius, col, 0, 1.0);
    }

    pub inline fn AddCircleFilledExt(self: *DrawList, center: Vec2, radius: f32, col: u32, num_segments: i32) void {
        return raw.ImDrawList_AddCircleFilled(self, &center, radius, col, num_segments);
    }
    pub inline fn AddCircleFilled(self: *DrawList, center: Vec2, radius: f32, col: u32) void {
        return @This().AddCircleFilledExt(self, center, radius, col, 0);
    }

    /// AddConvexPolyFilled(self: *DrawList, points: ?[*]const Vec2, num_points: i32, col: u32) void
    pub const AddConvexPolyFilled = raw.ImDrawList_AddConvexPolyFilled;

    /// AddDrawCmd(self: *DrawList) void
    pub const AddDrawCmd = raw.ImDrawList_AddDrawCmd;

    pub inline fn AddImageExt(self: *DrawList, user_texture_id: TextureID, p_min: Vec2, p_max: Vec2, uv_min: Vec2, uv_max: Vec2, col: u32) void {
        return raw.ImDrawList_AddImage(self, user_texture_id, &p_min, &p_max, &uv_min, &uv_max, col);
    }
    pub inline fn AddImage(self: *DrawList, user_texture_id: TextureID, p_min: Vec2, p_max: Vec2) void {
        return @This().AddImageExt(self, user_texture_id, p_min, p_max, .{.x=0,.y=0}, .{.x=1,.y=1}, 4294967295);
    }

    pub inline fn AddImageQuadExt(self: *DrawList, user_texture_id: TextureID, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, uv1: Vec2, uv2: Vec2, uv3: Vec2, uv4: Vec2, col: u32) void {
        return raw.ImDrawList_AddImageQuad(self, user_texture_id, &p1, &p2, &p3, &p4, &uv1, &uv2, &uv3, &uv4, col);
    }
    pub inline fn AddImageQuad(self: *DrawList, user_texture_id: TextureID, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2) void {
        return @This().AddImageQuadExt(self, user_texture_id, p1, p2, p3, p4, .{.x=0,.y=0}, .{.x=1,.y=0}, .{.x=1,.y=1}, .{.x=0,.y=1}, 4294967295);
    }

    pub inline fn AddImageRoundedExt(self: *DrawList, user_texture_id: TextureID, p_min: Vec2, p_max: Vec2, uv_min: Vec2, uv_max: Vec2, col: u32, rounding: f32, flags: DrawFlags) void {
        return raw.ImDrawList_AddImageRounded(self, user_texture_id, &p_min, &p_max, &uv_min, &uv_max, col, rounding, flags.toInt());
    }
    pub inline fn AddImageRounded(self: *DrawList, user_texture_id: TextureID, p_min: Vec2, p_max: Vec2, uv_min: Vec2, uv_max: Vec2, col: u32, rounding: f32) void {
        return @This().AddImageRoundedExt(self, user_texture_id, p_min, p_max, uv_min, uv_max, col, rounding, .{});
    }

    pub inline fn AddLineExt(self: *DrawList, p1: Vec2, p2: Vec2, col: u32, thickness: f32) void {
        return raw.ImDrawList_AddLine(self, &p1, &p2, col, thickness);
    }
    pub inline fn AddLine(self: *DrawList, p1: Vec2, p2: Vec2, col: u32) void {
        return @This().AddLineExt(self, p1, p2, col, 1.0);
    }

    pub inline fn AddNgonExt(self: *DrawList, center: Vec2, radius: f32, col: u32, num_segments: i32, thickness: f32) void {
        return raw.ImDrawList_AddNgon(self, &center, radius, col, num_segments, thickness);
    }
    pub inline fn AddNgon(self: *DrawList, center: Vec2, radius: f32, col: u32, num_segments: i32) void {
        return @This().AddNgonExt(self, center, radius, col, num_segments, 1.0);
    }

    pub inline fn AddNgonFilled(self: *DrawList, center: Vec2, radius: f32, col: u32, num_segments: i32) void {
        return raw.ImDrawList_AddNgonFilled(self, &center, radius, col, num_segments);
    }

    pub inline fn AddPolyline(self: *DrawList, points: ?[*]const Vec2, num_points: i32, col: u32, flags: DrawFlags, thickness: f32) void {
        return raw.ImDrawList_AddPolyline(self, points, num_points, col, flags.toInt(), thickness);
    }

    pub inline fn AddQuadExt(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, col: u32, thickness: f32) void {
        return raw.ImDrawList_AddQuad(self, &p1, &p2, &p3, &p4, col, thickness);
    }
    pub inline fn AddQuad(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, col: u32) void {
        return @This().AddQuadExt(self, p1, p2, p3, p4, col, 1.0);
    }

    pub inline fn AddQuadFilled(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, col: u32) void {
        return raw.ImDrawList_AddQuadFilled(self, &p1, &p2, &p3, &p4, col);
    }

    pub inline fn AddRectExt(self: *DrawList, p_min: Vec2, p_max: Vec2, col: u32, rounding: f32, flags: DrawFlags, thickness: f32) void {
        return raw.ImDrawList_AddRect(self, &p_min, &p_max, col, rounding, flags.toInt(), thickness);
    }
    pub inline fn AddRect(self: *DrawList, p_min: Vec2, p_max: Vec2, col: u32) void {
        return @This().AddRectExt(self, p_min, p_max, col, 0.0, .{}, 1.0);
    }

    pub inline fn AddRectFilledExt(self: *DrawList, p_min: Vec2, p_max: Vec2, col: u32, rounding: f32, flags: DrawFlags) void {
        return raw.ImDrawList_AddRectFilled(self, &p_min, &p_max, col, rounding, flags.toInt());
    }
    pub inline fn AddRectFilled(self: *DrawList, p_min: Vec2, p_max: Vec2, col: u32) void {
        return @This().AddRectFilledExt(self, p_min, p_max, col, 0.0, .{});
    }

    pub inline fn AddRectFilledMultiColor(self: *DrawList, p_min: Vec2, p_max: Vec2, col_upr_left: u32, col_upr_right: u32, col_bot_right: u32, col_bot_left: u32) void {
        return raw.ImDrawList_AddRectFilledMultiColor(self, &p_min, &p_max, col_upr_left, col_upr_right, col_bot_right, col_bot_left);
    }

    pub inline fn AddText_Vec2Ext(self: *DrawList, pos: Vec2, col: u32, text_begin: ?[*]const u8, text_end: ?[*]const u8) void {
        return raw.ImDrawList_AddText_Vec2(self, &pos, col, text_begin, text_end);
    }
    pub inline fn AddText_Vec2(self: *DrawList, pos: Vec2, col: u32, text_begin: ?[*]const u8) void {
        return @This().AddText_Vec2Ext(self, pos, col, text_begin, null);
    }

    pub inline fn AddText_FontPtrExt(self: *DrawList, font: ?*const Font, font_size: f32, pos: Vec2, col: u32, text_begin: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32, cpu_fine_clip_rect: ?*const Vec4) void {
        return raw.ImDrawList_AddText_FontPtr(self, font, font_size, &pos, col, text_begin, text_end, wrap_width, cpu_fine_clip_rect);
    }
    pub inline fn AddText_FontPtr(self: *DrawList, font: ?*const Font, font_size: f32, pos: Vec2, col: u32, text_begin: ?[*]const u8) void {
        return @This().AddText_FontPtrExt(self, font, font_size, pos, col, text_begin, null, 0.0, null);
    }

    pub inline fn AddTriangleExt(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, col: u32, thickness: f32) void {
        return raw.ImDrawList_AddTriangle(self, &p1, &p2, &p3, col, thickness);
    }
    pub inline fn AddTriangle(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, col: u32) void {
        return @This().AddTriangleExt(self, p1, p2, p3, col, 1.0);
    }

    pub inline fn AddTriangleFilled(self: *DrawList, p1: Vec2, p2: Vec2, p3: Vec2, col: u32) void {
        return raw.ImDrawList_AddTriangleFilled(self, &p1, &p2, &p3, col);
    }

    /// ChannelsMerge(self: *DrawList) void
    pub const ChannelsMerge = raw.ImDrawList_ChannelsMerge;

    /// ChannelsSetCurrent(self: *DrawList, n: i32) void
    pub const ChannelsSetCurrent = raw.ImDrawList_ChannelsSetCurrent;

    /// ChannelsSplit(self: *DrawList, count: i32) void
    pub const ChannelsSplit = raw.ImDrawList_ChannelsSplit;

    /// CloneOutput(self: *const DrawList) ?*DrawList
    pub const CloneOutput = raw.ImDrawList_CloneOutput;

    pub inline fn GetClipRectMax(self: *const DrawList) Vec2 {
        var out: Vec2 = undefined;
        raw.ImDrawList_GetClipRectMax(&out, self);
        return out;
    }

    pub inline fn GetClipRectMin(self: *const DrawList) Vec2 {
        var out: Vec2 = undefined;
        raw.ImDrawList_GetClipRectMin(&out, self);
        return out;
    }

    /// init_ImDrawList(self: ?*anyopaque, shared_data: ?*const DrawListSharedData) void
    pub const init_ImDrawList = raw.ImDrawList_ImDrawList;

    pub inline fn PathArcToExt(self: *DrawList, center: Vec2, radius: f32, a_min: f32, a_max: f32, num_segments: i32) void {
        return raw.ImDrawList_PathArcTo(self, &center, radius, a_min, a_max, num_segments);
    }
    pub inline fn PathArcTo(self: *DrawList, center: Vec2, radius: f32, a_min: f32, a_max: f32) void {
        return @This().PathArcToExt(self, center, radius, a_min, a_max, 0);
    }

    pub inline fn PathArcToFast(self: *DrawList, center: Vec2, radius: f32, a_min_of_12: i32, a_max_of_12: i32) void {
        return raw.ImDrawList_PathArcToFast(self, &center, radius, a_min_of_12, a_max_of_12);
    }

    pub inline fn PathBezierCubicCurveToExt(self: *DrawList, p2: Vec2, p3: Vec2, p4: Vec2, num_segments: i32) void {
        return raw.ImDrawList_PathBezierCubicCurveTo(self, &p2, &p3, &p4, num_segments);
    }
    pub inline fn PathBezierCubicCurveTo(self: *DrawList, p2: Vec2, p3: Vec2, p4: Vec2) void {
        return @This().PathBezierCubicCurveToExt(self, p2, p3, p4, 0);
    }

    pub inline fn PathBezierQuadraticCurveToExt(self: *DrawList, p2: Vec2, p3: Vec2, num_segments: i32) void {
        return raw.ImDrawList_PathBezierQuadraticCurveTo(self, &p2, &p3, num_segments);
    }
    pub inline fn PathBezierQuadraticCurveTo(self: *DrawList, p2: Vec2, p3: Vec2) void {
        return @This().PathBezierQuadraticCurveToExt(self, p2, p3, 0);
    }

    /// PathClear(self: *DrawList) void
    pub const PathClear = raw.ImDrawList_PathClear;

    /// PathFillConvex(self: *DrawList, col: u32) void
    pub const PathFillConvex = raw.ImDrawList_PathFillConvex;

    pub inline fn PathLineTo(self: *DrawList, pos: Vec2) void {
        return raw.ImDrawList_PathLineTo(self, &pos);
    }

    pub inline fn PathLineToMergeDuplicate(self: *DrawList, pos: Vec2) void {
        return raw.ImDrawList_PathLineToMergeDuplicate(self, &pos);
    }

    pub inline fn PathRectExt(self: *DrawList, rect_min: Vec2, rect_max: Vec2, rounding: f32, flags: DrawFlags) void {
        return raw.ImDrawList_PathRect(self, &rect_min, &rect_max, rounding, flags.toInt());
    }
    pub inline fn PathRect(self: *DrawList, rect_min: Vec2, rect_max: Vec2) void {
        return @This().PathRectExt(self, rect_min, rect_max, 0.0, .{});
    }

    pub inline fn PathStrokeExt(self: *DrawList, col: u32, flags: DrawFlags, thickness: f32) void {
        return raw.ImDrawList_PathStroke(self, col, flags.toInt(), thickness);
    }
    pub inline fn PathStroke(self: *DrawList, col: u32) void {
        return @This().PathStrokeExt(self, col, .{}, 1.0);
    }

    /// PopClipRect(self: *DrawList) void
    pub const PopClipRect = raw.ImDrawList_PopClipRect;

    /// PopTextureID(self: *DrawList) void
    pub const PopTextureID = raw.ImDrawList_PopTextureID;

    pub inline fn PrimQuadUV(self: *DrawList, a: Vec2, b: Vec2, c: Vec2, d: Vec2, uv_a: Vec2, uv_b: Vec2, uv_c: Vec2, uv_d: Vec2, col: u32) void {
        return raw.ImDrawList_PrimQuadUV(self, &a, &b, &c, &d, &uv_a, &uv_b, &uv_c, &uv_d, col);
    }

    pub inline fn PrimRect(self: *DrawList, a: Vec2, b: Vec2, col: u32) void {
        return raw.ImDrawList_PrimRect(self, &a, &b, col);
    }

    pub inline fn PrimRectUV(self: *DrawList, a: Vec2, b: Vec2, uv_a: Vec2, uv_b: Vec2, col: u32) void {
        return raw.ImDrawList_PrimRectUV(self, &a, &b, &uv_a, &uv_b, col);
    }

    /// PrimReserve(self: *DrawList, idx_count: i32, vtx_count: i32) void
    pub const PrimReserve = raw.ImDrawList_PrimReserve;

    /// PrimUnreserve(self: *DrawList, idx_count: i32, vtx_count: i32) void
    pub const PrimUnreserve = raw.ImDrawList_PrimUnreserve;

    pub inline fn PrimVtx(self: *DrawList, pos: Vec2, uv: Vec2, col: u32) void {
        return raw.ImDrawList_PrimVtx(self, &pos, &uv, col);
    }

    /// PrimWriteIdx(self: *DrawList, idx: DrawIdx) void
    pub const PrimWriteIdx = raw.ImDrawList_PrimWriteIdx;

    pub inline fn PrimWriteVtx(self: *DrawList, pos: Vec2, uv: Vec2, col: u32) void {
        return raw.ImDrawList_PrimWriteVtx(self, &pos, &uv, col);
    }

    pub inline fn PushClipRectExt(self: *DrawList, clip_rect_min: Vec2, clip_rect_max: Vec2, intersect_with_current_clip_rect: bool) void {
        return raw.ImDrawList_PushClipRect(self, &clip_rect_min, &clip_rect_max, intersect_with_current_clip_rect);
    }
    pub inline fn PushClipRect(self: *DrawList, clip_rect_min: Vec2, clip_rect_max: Vec2) void {
        return @This().PushClipRectExt(self, clip_rect_min, clip_rect_max, false);
    }

    /// PushClipRectFullScreen(self: *DrawList) void
    pub const PushClipRectFullScreen = raw.ImDrawList_PushClipRectFullScreen;

    /// PushTextureID(self: *DrawList, texture_id: TextureID) void
    pub const PushTextureID = raw.ImDrawList_PushTextureID;

    /// _CalcCircleAutoSegmentCount(self: *const DrawList, radius: f32) i32
    pub const _CalcCircleAutoSegmentCount = raw.ImDrawList__CalcCircleAutoSegmentCount;

    /// _ClearFreeMemory(self: *DrawList) void
    pub const _ClearFreeMemory = raw.ImDrawList__ClearFreeMemory;

    /// _OnChangedClipRect(self: *DrawList) void
    pub const _OnChangedClipRect = raw.ImDrawList__OnChangedClipRect;

    /// _OnChangedTextureID(self: *DrawList) void
    pub const _OnChangedTextureID = raw.ImDrawList__OnChangedTextureID;

    /// _OnChangedVtxOffset(self: *DrawList) void
    pub const _OnChangedVtxOffset = raw.ImDrawList__OnChangedVtxOffset;

    pub inline fn _PathArcToFastEx(self: *DrawList, center: Vec2, radius: f32, a_min_sample: i32, a_max_sample: i32, a_step: i32) void {
        return raw.ImDrawList__PathArcToFastEx(self, &center, radius, a_min_sample, a_max_sample, a_step);
    }

    pub inline fn _PathArcToN(self: *DrawList, center: Vec2, radius: f32, a_min: f32, a_max: f32, num_segments: i32) void {
        return raw.ImDrawList__PathArcToN(self, &center, radius, a_min, a_max, num_segments);
    }

    /// _PopUnusedDrawCmd(self: *DrawList) void
    pub const _PopUnusedDrawCmd = raw.ImDrawList__PopUnusedDrawCmd;

    /// _ResetForNewFrame(self: *DrawList) void
    pub const _ResetForNewFrame = raw.ImDrawList__ResetForNewFrame;

    /// _TryMergeDrawCmds(self: *DrawList) void
    pub const _TryMergeDrawCmds = raw.ImDrawList__TryMergeDrawCmds;

    /// deinit(self: *DrawList) void
    pub const deinit = raw.ImDrawList_destroy;
};

pub const DrawListSharedData = extern struct {
    TexUvWhitePixel: Vec2,
    Font: ?*Font,
    FontSize: f32,
    CurveTessellationTol: f32,
    CircleSegmentMaxError: f32,
    ClipRectFullscreen: Vec4,
    InitialFlags: DrawListFlags align(4),
    ArcFastVtx: [48]Vec2,
    ArcFastRadiusCutoff: f32,
    CircleSegmentCounts: [64]u8,
    TexUvLines: [*c]const Vec4,

    /// init_ImDrawListSharedData(self: ?*anyopaque) void
    pub const init_ImDrawListSharedData = raw.ImDrawListSharedData_ImDrawListSharedData;

    /// SetCircleTessellationMaxError(self: *DrawListSharedData, max_error: f32) void
    pub const SetCircleTessellationMaxError = raw.ImDrawListSharedData_SetCircleTessellationMaxError;

    /// deinit(self: *DrawListSharedData) void
    pub const deinit = raw.ImDrawListSharedData_destroy;
};

pub const DrawListSplitter = extern struct {
    _Current: i32,
    _Count: i32,
    _Channels: Vector(DrawChannel),

    /// Clear(self: *DrawListSplitter) void
    pub const Clear = raw.ImDrawListSplitter_Clear;

    /// ClearFreeMemory(self: *DrawListSplitter) void
    pub const ClearFreeMemory = raw.ImDrawListSplitter_ClearFreeMemory;

    /// init_ImDrawListSplitter(self: ?*anyopaque) void
    pub const init_ImDrawListSplitter = raw.ImDrawListSplitter_ImDrawListSplitter;

    /// Merge(self: *DrawListSplitter, draw_list: ?*DrawList) void
    pub const Merge = raw.ImDrawListSplitter_Merge;

    /// SetCurrentChannel(self: *DrawListSplitter, draw_list: ?*DrawList, channel_idx: i32) void
    pub const SetCurrentChannel = raw.ImDrawListSplitter_SetCurrentChannel;

    /// Split(self: *DrawListSplitter, draw_list: ?*DrawList, count: i32) void
    pub const Split = raw.ImDrawListSplitter_Split;

    /// deinit(self: *DrawListSplitter) void
    pub const deinit = raw.ImDrawListSplitter_destroy;
};

pub const DrawVert = extern struct {
    pos: Vec2,
    uv: Vec2,
    col: u32,
};

pub const Font = extern struct {
    IndexAdvanceX: Vector(f32),
    FallbackAdvanceX: f32,
    FontSize: f32,
    IndexLookup: Vector(Wchar),
    Glyphs: Vector(FontGlyph),
    FallbackGlyph: ?*const FontGlyph,
    ContainerAtlas: ?*FontAtlas,
    ConfigData: ?*const FontConfig,
    ConfigDataCount: i16,
    FallbackChar: Wchar,
    EllipsisChar: Wchar,
    DotChar: Wchar,
    DirtyLookupTables: bool,
    Scale: f32,
    Ascent: f32,
    Descent: f32,
    MetricsTotalSurface: i32,
    Used4kPagesMap: [(0xFFFF+1)/4096/8]u8,

    /// AddGlyph(self: *Font, src_cfg: ?*const FontConfig, c: Wchar, x0: f32, y0: f32, x1: f32, y1: f32, u0: f32, v0: f32, u1: f32, v1: f32, advance_x: f32) void
    pub const AddGlyph = raw.ImFont_AddGlyph;

    /// AddRemapCharExt(self: *Font, dst: Wchar, src: Wchar, overwrite_dst: bool) void
    pub const AddRemapCharExt = raw.ImFont_AddRemapChar;
    pub inline fn AddRemapChar(self: *Font, dst: Wchar, src: Wchar) void {
        return @This().AddRemapCharExt(self, dst, src, true);
    }

    /// BuildLookupTable(self: *Font) void
    pub const BuildLookupTable = raw.ImFont_BuildLookupTable;

    pub inline fn CalcTextSizeAExt(self: *const Font, size: f32, max_width: f32, wrap_width: f32, text_begin: ?[*]const u8, text_end: ?[*]const u8, remaining: ?*?[*:0]const u8) Vec2 {
        var out: Vec2 = undefined;
        raw.ImFont_CalcTextSizeA(&out, self, size, max_width, wrap_width, text_begin, text_end, remaining);
        return out;
    }
    pub inline fn CalcTextSizeA(self: *const Font, size: f32, max_width: f32, wrap_width: f32, text_begin: ?[*]const u8) Vec2 {
        return @This().CalcTextSizeAExt(self, size, max_width, wrap_width, text_begin, null, null);
    }

    /// CalcWordWrapPositionA(self: *const Font, scale: f32, text: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32) ?[*]const u8
    pub const CalcWordWrapPositionA = raw.ImFont_CalcWordWrapPositionA;

    /// ClearOutputData(self: *Font) void
    pub const ClearOutputData = raw.ImFont_ClearOutputData;

    /// FindGlyph(self: *const Font, c: Wchar) ?*const FontGlyph
    pub const FindGlyph = raw.ImFont_FindGlyph;

    /// FindGlyphNoFallback(self: *const Font, c: Wchar) ?*const FontGlyph
    pub const FindGlyphNoFallback = raw.ImFont_FindGlyphNoFallback;

    /// GetCharAdvance(self: *const Font, c: Wchar) f32
    pub const GetCharAdvance = raw.ImFont_GetCharAdvance;

    /// GetDebugName(self: *const Font) ?[*:0]const u8
    pub const GetDebugName = raw.ImFont_GetDebugName;

    /// GrowIndex(self: *Font, new_size: i32) void
    pub const GrowIndex = raw.ImFont_GrowIndex;

    /// init_ImFont(self: ?*anyopaque) void
    pub const init_ImFont = raw.ImFont_ImFont;

    /// IsGlyphRangeUnused(self: *Font, c_begin: u32, c_last: u32) bool
    pub const IsGlyphRangeUnused = raw.ImFont_IsGlyphRangeUnused;

    /// IsLoaded(self: *const Font) bool
    pub const IsLoaded = raw.ImFont_IsLoaded;

    pub inline fn RenderChar(self: *const Font, draw_list: ?*DrawList, size: f32, pos: Vec2, col: u32, c: Wchar) void {
        return raw.ImFont_RenderChar(self, draw_list, size, &pos, col, c);
    }

    pub inline fn RenderTextExt(self: *const Font, draw_list: ?*DrawList, size: f32, pos: Vec2, col: u32, clip_rect: Vec4, text_begin: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32, cpu_fine_clip: bool) void {
        return raw.ImFont_RenderText(self, draw_list, size, &pos, col, &clip_rect, text_begin, text_end, wrap_width, cpu_fine_clip);
    }
    pub inline fn RenderText(self: *const Font, draw_list: ?*DrawList, size: f32, pos: Vec2, col: u32, clip_rect: Vec4, text_begin: ?[*]const u8, text_end: ?[*]const u8) void {
        return @This().RenderTextExt(self, draw_list, size, pos, col, clip_rect, text_begin, text_end, 0.0, false);
    }

    /// SetGlyphVisible(self: *Font, c: Wchar, visible: bool) void
    pub const SetGlyphVisible = raw.ImFont_SetGlyphVisible;

    /// deinit(self: *Font) void
    pub const deinit = raw.ImFont_destroy;
};

pub const FontAtlas = extern struct {
    Flags: FontAtlasFlags align(4),
    TexID: TextureID,
    TexDesiredWidth: i32,
    TexGlyphPadding: i32,
    Locked: bool,
    TexReady: bool,
    TexPixelsUseColors: bool,
    TexPixelsAlpha8: ?[*]u8,
    TexPixelsRGBA32: ?[*]u32,
    TexWidth: i32,
    TexHeight: i32,
    TexUvScale: Vec2,
    TexUvWhitePixel: Vec2,
    Fonts: Vector(?*Font),
    CustomRects: Vector(FontAtlasCustomRect),
    ConfigData: Vector(FontConfig),
    TexUvLines: [(63)+1]Vec4,
    FontBuilderIO: ?*const FontBuilderIO,
    FontBuilderFlags: u32,
    PackIdMouseCursors: i32,
    PackIdLines: i32,

    pub inline fn AddCustomRectFontGlyphExt(self: *FontAtlas, font: ?*Font, id: Wchar, width: i32, height: i32, advance_x: f32, offset: Vec2) i32 {
        return raw.ImFontAtlas_AddCustomRectFontGlyph(self, font, id, width, height, advance_x, &offset);
    }
    pub inline fn AddCustomRectFontGlyph(self: *FontAtlas, font: ?*Font, id: Wchar, width: i32, height: i32, advance_x: f32) i32 {
        return @This().AddCustomRectFontGlyphExt(self, font, id, width, height, advance_x, .{.x=0,.y=0});
    }

    /// AddCustomRectRegular(self: *FontAtlas, width: i32, height: i32) i32
    pub const AddCustomRectRegular = raw.ImFontAtlas_AddCustomRectRegular;

    /// AddFont(self: *FontAtlas, font_cfg: ?*const FontConfig) ?*Font
    pub const AddFont = raw.ImFontAtlas_AddFont;

    /// AddFontDefaultExt(self: *FontAtlas, font_cfg: ?*const FontConfig) ?*Font
    pub const AddFontDefaultExt = raw.ImFontAtlas_AddFontDefault;
    pub inline fn AddFontDefault(self: *FontAtlas) ?*Font {
        return @This().AddFontDefaultExt(self, null);
    }

    /// AddFontFromFileTTFExt(self: *FontAtlas, filename: ?[*:0]const u8, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) ?*Font
    pub const AddFontFromFileTTFExt = raw.ImFontAtlas_AddFontFromFileTTF;
    pub inline fn AddFontFromFileTTF(self: *FontAtlas, filename: ?[*:0]const u8, size_pixels: f32) ?*Font {
        return @This().AddFontFromFileTTFExt(self, filename, size_pixels, null, null);
    }

    /// AddFontFromMemoryCompressedBase85TTFExt(self: *FontAtlas, compressed_font_data_base85: ?[*]const u8, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) ?*Font
    pub const AddFontFromMemoryCompressedBase85TTFExt = raw.ImFontAtlas_AddFontFromMemoryCompressedBase85TTF;
    pub inline fn AddFontFromMemoryCompressedBase85TTF(self: *FontAtlas, compressed_font_data_base85: ?[*]const u8, size_pixels: f32) ?*Font {
        return @This().AddFontFromMemoryCompressedBase85TTFExt(self, compressed_font_data_base85, size_pixels, null, null);
    }

    /// AddFontFromMemoryCompressedTTFExt(self: *FontAtlas, compressed_font_data: ?*const anyopaque, compressed_font_size: i32, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) ?*Font
    pub const AddFontFromMemoryCompressedTTFExt = raw.ImFontAtlas_AddFontFromMemoryCompressedTTF;
    pub inline fn AddFontFromMemoryCompressedTTF(self: *FontAtlas, compressed_font_data: ?*const anyopaque, compressed_font_size: i32, size_pixels: f32) ?*Font {
        return @This().AddFontFromMemoryCompressedTTFExt(self, compressed_font_data, compressed_font_size, size_pixels, null, null);
    }

    /// AddFontFromMemoryTTFExt(self: *FontAtlas, font_data: ?*anyopaque, font_size: i32, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) ?*Font
    pub const AddFontFromMemoryTTFExt = raw.ImFontAtlas_AddFontFromMemoryTTF;
    pub inline fn AddFontFromMemoryTTF(self: *FontAtlas, font_data: ?*anyopaque, font_size: i32, size_pixels: f32) ?*Font {
        return @This().AddFontFromMemoryTTFExt(self, font_data, font_size, size_pixels, null, null);
    }

    /// Build(self: *FontAtlas) bool
    pub const Build = raw.ImFontAtlas_Build;

    /// CalcCustomRectUV(self: *const FontAtlas, rect: ?*const FontAtlasCustomRect, out_uv_min: ?*Vec2, out_uv_max: ?*Vec2) void
    pub const CalcCustomRectUV = raw.ImFontAtlas_CalcCustomRectUV;

    /// Clear(self: *FontAtlas) void
    pub const Clear = raw.ImFontAtlas_Clear;

    /// ClearFonts(self: *FontAtlas) void
    pub const ClearFonts = raw.ImFontAtlas_ClearFonts;

    /// ClearInputData(self: *FontAtlas) void
    pub const ClearInputData = raw.ImFontAtlas_ClearInputData;

    /// ClearTexData(self: *FontAtlas) void
    pub const ClearTexData = raw.ImFontAtlas_ClearTexData;

    /// GetCustomRectByIndex(self: *FontAtlas, index: i32) ?*FontAtlasCustomRect
    pub const GetCustomRectByIndex = raw.ImFontAtlas_GetCustomRectByIndex;

    /// GetGlyphRangesChineseFull(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesChineseFull = raw.ImFontAtlas_GetGlyphRangesChineseFull;

    /// GetGlyphRangesChineseSimplifiedCommon(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesChineseSimplifiedCommon = raw.ImFontAtlas_GetGlyphRangesChineseSimplifiedCommon;

    /// GetGlyphRangesCyrillic(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesCyrillic = raw.ImFontAtlas_GetGlyphRangesCyrillic;

    /// GetGlyphRangesDefault(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesDefault = raw.ImFontAtlas_GetGlyphRangesDefault;

    /// GetGlyphRangesJapanese(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesJapanese = raw.ImFontAtlas_GetGlyphRangesJapanese;

    /// GetGlyphRangesKorean(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesKorean = raw.ImFontAtlas_GetGlyphRangesKorean;

    /// GetGlyphRangesThai(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesThai = raw.ImFontAtlas_GetGlyphRangesThai;

    /// GetGlyphRangesVietnamese(self: *FontAtlas) ?*const Wchar
    pub const GetGlyphRangesVietnamese = raw.ImFontAtlas_GetGlyphRangesVietnamese;

    /// GetMouseCursorTexData(self: *FontAtlas, cursor: MouseCursor, out_offset: ?*Vec2, out_size: ?*Vec2, out_uv_border: *[2]Vec2, out_uv_fill: *[2]Vec2) bool
    pub const GetMouseCursorTexData = raw.ImFontAtlas_GetMouseCursorTexData;

    /// GetTexDataAsAlpha8Ext(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32, out_bytes_per_pixel: ?*i32) void
    pub const GetTexDataAsAlpha8Ext = raw.ImFontAtlas_GetTexDataAsAlpha8;
    pub inline fn GetTexDataAsAlpha8(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32) void {
        return @This().GetTexDataAsAlpha8Ext(self, out_pixels, out_width, out_height, null);
    }

    /// GetTexDataAsRGBA32Ext(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32, out_bytes_per_pixel: ?*i32) void
    pub const GetTexDataAsRGBA32Ext = raw.ImFontAtlas_GetTexDataAsRGBA32;
    pub inline fn GetTexDataAsRGBA32(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32) void {
        return @This().GetTexDataAsRGBA32Ext(self, out_pixels, out_width, out_height, null);
    }

    /// init_ImFontAtlas(self: ?*anyopaque) void
    pub const init_ImFontAtlas = raw.ImFontAtlas_ImFontAtlas;

    /// IsBuilt(self: *const FontAtlas) bool
    pub const IsBuilt = raw.ImFontAtlas_IsBuilt;

    /// SetTexID(self: *FontAtlas, id: TextureID) void
    pub const SetTexID = raw.ImFontAtlas_SetTexID;

    /// deinit(self: *FontAtlas) void
    pub const deinit = raw.ImFontAtlas_destroy;
};

pub const FontAtlasCustomRect = extern struct {
    Width: u16,
    Height: u16,
    X: u16,
    Y: u16,
    GlyphID: u32,
    GlyphAdvanceX: f32,
    GlyphOffset: Vec2,
    Font: ?*Font,

    /// init_ImFontAtlasCustomRect(self: ?*anyopaque) void
    pub const init_ImFontAtlasCustomRect = raw.ImFontAtlasCustomRect_ImFontAtlasCustomRect;

    /// IsPacked(self: *const FontAtlasCustomRect) bool
    pub const IsPacked = raw.ImFontAtlasCustomRect_IsPacked;

    /// deinit(self: *FontAtlasCustomRect) void
    pub const deinit = raw.ImFontAtlasCustomRect_destroy;
};

pub const FontBuilderIO = extern struct {
    FontBuilder_Build: ?fn (atlas: ?*FontAtlas) callconv(.C) bool,
};

pub const FontConfig = extern struct {
    FontData: ?*anyopaque,
    FontDataSize: i32,
    FontDataOwnedByAtlas: bool,
    FontNo: i32,
    SizePixels: f32,
    OversampleH: i32,
    OversampleV: i32,
    PixelSnapH: bool,
    GlyphExtraSpacing: Vec2,
    GlyphOffset: Vec2,
    GlyphRanges: ?[*:0]const Wchar,
    GlyphMinAdvanceX: f32,
    GlyphMaxAdvanceX: f32,
    MergeMode: bool,
    FontBuilderFlags: u32,
    RasterizerMultiply: f32,
    EllipsisChar: Wchar,
    Name: [40]u8,
    DstFont: ?*Font,

    /// init_ImFontConfig(self: ?*anyopaque) void
    pub const init_ImFontConfig = raw.ImFontConfig_ImFontConfig;

    /// deinit(self: *FontConfig) void
    pub const deinit = raw.ImFontConfig_destroy;
};

pub const FontGlyph = extern struct {
    Colored: u32,
    Visible: u32,
    Codepoint: u32,
    AdvanceX: f32,
    X0: f32,
    Y0: f32,
    X1: f32,
    Y1: f32,
    U0: f32,
    V0: f32,
    U1: f32,
    V1: f32,
};

pub const FontGlyphRangesBuilder = extern struct {
    UsedChars: Vector(u32),

    /// AddChar(self: *FontGlyphRangesBuilder, c: Wchar) void
    pub const AddChar = raw.ImFontGlyphRangesBuilder_AddChar;

    /// AddRanges(self: *FontGlyphRangesBuilder, ranges: ?[*:0]const Wchar) void
    pub const AddRanges = raw.ImFontGlyphRangesBuilder_AddRanges;

    /// AddTextExt(self: *FontGlyphRangesBuilder, text: ?[*]const u8, text_end: ?[*]const u8) void
    pub const AddTextExt = raw.ImFontGlyphRangesBuilder_AddText;
    pub inline fn AddText(self: *FontGlyphRangesBuilder, text: ?[*]const u8) void {
        return @This().AddTextExt(self, text, null);
    }

    /// BuildRanges(self: *FontGlyphRangesBuilder, out_ranges: *Vector(Wchar)) void
    pub const BuildRanges = raw.ImFontGlyphRangesBuilder_BuildRanges;

    /// Clear(self: *FontGlyphRangesBuilder) void
    pub const Clear = raw.ImFontGlyphRangesBuilder_Clear;

    /// GetBit(self: *const FontGlyphRangesBuilder, n: usize) bool
    pub const GetBit = raw.ImFontGlyphRangesBuilder_GetBit;

    /// init_ImFontGlyphRangesBuilder(self: ?*anyopaque) void
    pub const init_ImFontGlyphRangesBuilder = raw.ImFontGlyphRangesBuilder_ImFontGlyphRangesBuilder;

    /// SetBit(self: *FontGlyphRangesBuilder, n: usize) void
    pub const SetBit = raw.ImFontGlyphRangesBuilder_SetBit;

    /// deinit(self: *FontGlyphRangesBuilder) void
    pub const deinit = raw.ImFontGlyphRangesBuilder_destroy;
};

pub const ColorMod = extern struct {
    Col: Col,
    BackupValue: Vec4,
};

pub const ComboPreviewData = extern struct {
    PreviewRect: Rect,
    BackupCursorPos: Vec2,
    BackupCursorMaxPos: Vec2,
    BackupCursorPosPrevLine: Vec2,
    BackupPrevLineTextBaseOffset: f32,
    BackupLayout: LayoutType,

    /// init_ImGuiComboPreviewData(self: ?*anyopaque) void
    pub const init_ImGuiComboPreviewData = raw.ImGuiComboPreviewData_ImGuiComboPreviewData;

    /// deinit(self: *ComboPreviewData) void
    pub const deinit = raw.ImGuiComboPreviewData_destroy;
};

pub const Context = extern struct {
    Initialized: bool,
    FontAtlasOwnedByContext: bool,
    IO: IO,
    InputEventsQueue: Vector(InputEvent),
    InputEventsTrail: Vector(InputEvent),
    Style: Style,
    Font: ?*Font,
    FontSize: f32,
    FontBaseSize: f32,
    DrawListSharedData: DrawListSharedData,
    Time: f64,
    FrameCount: i32,
    FrameCountEnded: i32,
    FrameCountRendered: i32,
    WithinFrameScope: bool,
    WithinFrameScopeWithImplicitWindow: bool,
    WithinEndChild: bool,
    GcCompactAll: bool,
    TestEngineHookItems: bool,
    TestEngine: ?*anyopaque,
    Windows: Vector(?*Window),
    WindowsFocusOrder: Vector(?*Window),
    WindowsTempSortBuffer: Vector(?*Window),
    CurrentWindowStack: Vector(WindowStackData),
    WindowsById: Storage,
    WindowsActiveCount: i32,
    WindowsHoverPadding: Vec2,
    CurrentWindow: ?*Window,
    HoveredWindow: ?*Window,
    HoveredWindowUnderMovingWindow: ?*Window,
    MovingWindow: ?*Window,
    WheelingWindow: ?*Window,
    WheelingWindowRefMousePos: Vec2,
    WheelingWindowTimer: f32,
    DebugHookIdInfo: ID,
    HoveredId: ID,
    HoveredIdPreviousFrame: ID,
    HoveredIdAllowOverlap: bool,
    HoveredIdUsingMouseWheel: bool,
    HoveredIdPreviousFrameUsingMouseWheel: bool,
    HoveredIdDisabled: bool,
    HoveredIdTimer: f32,
    HoveredIdNotActiveTimer: f32,
    ActiveId: ID,
    ActiveIdIsAlive: ID,
    ActiveIdTimer: f32,
    ActiveIdIsJustActivated: bool,
    ActiveIdAllowOverlap: bool,
    ActiveIdNoClearOnFocusLoss: bool,
    ActiveIdHasBeenPressedBefore: bool,
    ActiveIdHasBeenEditedBefore: bool,
    ActiveIdHasBeenEditedThisFrame: bool,
    ActiveIdClickOffset: Vec2,
    ActiveIdWindow: ?*Window,
    ActiveIdSource: InputSource,
    ActiveIdMouseButton: i32,
    ActiveIdPreviousFrame: ID,
    ActiveIdPreviousFrameIsAlive: bool,
    ActiveIdPreviousFrameHasBeenEditedBefore: bool,
    ActiveIdPreviousFrameWindow: ?*Window,
    LastActiveId: ID,
    LastActiveIdTimer: f32,
    ActiveIdUsingMouseWheel: bool,
    ActiveIdUsingNavDirMask: u32,
    ActiveIdUsingNavInputMask: u32,
    ActiveIdUsingKeyInputMask: BitArrayForNamedKeys,
    CurrentItemFlags: ItemFlags align(4),
    NextItemData: NextItemData,
    LastItemData: LastItemData,
    NextWindowData: NextWindowData,
    ColorStack: Vector(ColorMod),
    StyleVarStack: Vector(StyleMod),
    FontStack: Vector(?*Font),
    FocusScopeStack: Vector(ID),
    ItemFlagsStack: Vector(ItemFlags) align(4),
    GroupStack: Vector(GroupData),
    OpenPopupStack: Vector(PopupData),
    BeginPopupStack: Vector(PopupData),
    BeginMenuCount: i32,
    Viewports: Vector(?*ViewportP),
    NavWindow: ?*Window,
    NavId: ID,
    NavFocusScopeId: ID,
    NavActivateId: ID,
    NavActivateDownId: ID,
    NavActivatePressedId: ID,
    NavActivateInputId: ID,
    NavActivateFlags: ActivateFlags align(4),
    NavJustMovedToId: ID,
    NavJustMovedToFocusScopeId: ID,
    NavJustMovedToKeyMods: ModFlags align(4),
    NavNextActivateId: ID,
    NavNextActivateFlags: ActivateFlags align(4),
    NavInputSource: InputSource,
    NavLayer: NavLayer,
    NavIdIsAlive: bool,
    NavMousePosDirty: bool,
    NavDisableHighlight: bool,
    NavDisableMouseHover: bool,
    NavAnyRequest: bool,
    NavInitRequest: bool,
    NavInitRequestFromMove: bool,
    NavInitResultId: ID,
    NavInitResultRectRel: Rect,
    NavMoveSubmitted: bool,
    NavMoveScoringItems: bool,
    NavMoveForwardToNextFrame: bool,
    NavMoveFlags: NavMoveFlags align(4),
    NavMoveScrollFlags: ScrollFlags align(4),
    NavMoveKeyMods: ModFlags align(4),
    NavMoveDir: Dir,
    NavMoveDirForDebug: Dir,
    NavMoveClipDir: Dir,
    NavScoringRect: Rect,
    NavScoringNoClipRect: Rect,
    NavScoringDebugCount: i32,
    NavTabbingDir: i32,
    NavTabbingCounter: i32,
    NavMoveResultLocal: NavItemData,
    NavMoveResultLocalVisible: NavItemData,
    NavMoveResultOther: NavItemData,
    NavTabbingResultFirst: NavItemData,
    NavWindowingTarget: ?*Window,
    NavWindowingTargetAnim: ?*Window,
    NavWindowingListWindow: ?*Window,
    NavWindowingTimer: f32,
    NavWindowingHighlightAlpha: f32,
    NavWindowingToggleLayer: bool,
    DimBgRatio: f32,
    MouseCursor: MouseCursor,
    DragDropActive: bool,
    DragDropWithinSource: bool,
    DragDropWithinTarget: bool,
    DragDropSourceFlags: DragDropFlags align(4),
    DragDropSourceFrameCount: i32,
    DragDropMouseButton: i32,
    DragDropPayload: Payload,
    DragDropTargetRect: Rect,
    DragDropTargetId: ID,
    DragDropAcceptFlags: DragDropFlags align(4),
    DragDropAcceptIdCurrRectSurface: f32,
    DragDropAcceptIdCurr: ID,
    DragDropAcceptIdPrev: ID,
    DragDropAcceptFrameCount: i32,
    DragDropHoldJustPressedId: ID,
    DragDropPayloadBufHeap: Vector(unsigned_char),
    DragDropPayloadBufLocal: [16]u8,
    ClipperTempDataStacked: i32,
    ClipperTempData: Vector(ListClipperData),
    CurrentTable: ?*Table,
    TablesTempDataStacked: i32,
    TablesTempData: Vector(TableTempData),
    Tables: Pool_ImGuiTable,
    TablesLastTimeActive: Vector(f32),
    DrawChannelsTempMergeBuffer: Vector(DrawChannel),
    CurrentTabBar: ?*TabBar,
    TabBars: Pool_ImGuiTabBar,
    CurrentTabBarStack: Vector(PtrOrIndex),
    ShrinkWidthBuffer: Vector(ShrinkWidthItem),
    MouseLastValidPos: Vec2,
    InputTextState: InputTextState,
    InputTextPasswordFont: Font,
    TempInputId: ID,
    ColorEditOptions: ColorEditFlags align(4),
    ColorEditLastHue: f32,
    ColorEditLastSat: f32,
    ColorEditLastColor: u32,
    ColorPickerRef: Vec4,
    ComboPreviewData: ComboPreviewData,
    SliderGrabClickOffset: f32,
    SliderCurrentAccum: f32,
    SliderCurrentAccumDirty: bool,
    DragCurrentAccumDirty: bool,
    DragCurrentAccum: f32,
    DragSpeedDefaultRatio: f32,
    ScrollbarClickDeltaToGrabCenter: f32,
    DisabledAlphaBackup: f32,
    DisabledStackSize: i16,
    TooltipOverrideCount: i16,
    TooltipSlowDelay: f32,
    ClipboardHandlerData: Vector(u8),
    MenusIdSubmittedThisFrame: Vector(ID),
    PlatformImeData: PlatformImeData,
    PlatformImeDataPrev: PlatformImeData,
    PlatformLocaleDecimalPoint: u8,
    SettingsLoaded: bool,
    SettingsDirtyTimer: f32,
    SettingsIniData: TextBuffer,
    SettingsHandlers: Vector(SettingsHandler),
    SettingsWindows: ChunkStream_ImGuiWindowSettings,
    SettingsTables: ChunkStream_ImGuiTableSettings,
    Hooks: Vector(ContextHook),
    HookIdNext: ID,
    LogEnabled: bool,
    LogType: LogType,
    LogFile: FileHandle,
    LogBuffer: TextBuffer,
    LogNextPrefix: [*c]const u8,
    LogNextSuffix: [*c]const u8,
    LogLinePosY: f32,
    LogLineFirstItem: bool,
    LogDepthRef: i32,
    LogDepthToExpand: i32,
    LogDepthToExpandDefault: i32,
    DebugLogFlags: DebugLogFlags align(4),
    DebugLogBuf: TextBuffer,
    DebugItemPickerActive: bool,
    DebugItemPickerBreakId: ID,
    DebugMetricsConfig: MetricsConfig,
    DebugStackTool: StackTool,
    FramerateSecPerFrame: [120]f32,
    FramerateSecPerFrameIdx: i32,
    FramerateSecPerFrameCount: i32,
    FramerateSecPerFrameAccum: f32,
    WantCaptureMouseNextFrame: i32,
    WantCaptureKeyboardNextFrame: i32,
    WantTextInputNextFrame: i32,
    TempBuffer: Vector(u8),

    /// init_ImGuiContext(self: ?*anyopaque, shared_font_atlas: ?*FontAtlas) void
    pub const init_ImGuiContext = raw.ImGuiContext_ImGuiContext;

    /// deinit(self: *Context) void
    pub const deinit = raw.ImGuiContext_destroy;
};

pub const ContextHook = extern struct {
    HookId: ID,
    Type: ContextHookType,
    Owner: ID,
    Callback: ContextHookCallback,
    UserData: ?*anyopaque,

    /// init_ImGuiContextHook(self: ?*anyopaque) void
    pub const init_ImGuiContextHook = raw.ImGuiContextHook_ImGuiContextHook;

    /// deinit(self: *ContextHook) void
    pub const deinit = raw.ImGuiContextHook_destroy;
};

pub const DataTypeInfo = extern struct {
    Size: usize,
    Name: ?[*:0]const u8,
    PrintFmt: [*c]const u8,
    ScanFmt: [*c]const u8,
};

pub const DataTypeTempStorage = extern struct {
    Data: [8]u8,
};

pub const GroupData = extern struct {
    WindowID: ID,
    BackupCursorPos: Vec2,
    BackupCursorMaxPos: Vec2,
    BackupIndent: Vec1,
    BackupGroupOffset: Vec1,
    BackupCurrLineSize: Vec2,
    BackupCurrLineTextBaseOffset: f32,
    BackupActiveIdIsAlive: ID,
    BackupActiveIdPreviousFrameIsAlive: bool,
    BackupHoveredIdIsAlive: bool,
    EmitItem: bool,
};

pub const IO = extern struct {
    ConfigFlags: ConfigFlags align(4),
    BackendFlags: BackendFlags align(4),
    DisplaySize: Vec2,
    DeltaTime: f32,
    IniSavingRate: f32,
    IniFilename: ?[*:0]const u8,
    LogFilename: ?[*:0]const u8,
    MouseDoubleClickTime: f32,
    MouseDoubleClickMaxDist: f32,
    MouseDragThreshold: f32,
    KeyRepeatDelay: f32,
    KeyRepeatRate: f32,
    UserData: ?*anyopaque,
    Fonts: ?*FontAtlas,
    FontGlobalScale: f32,
    FontAllowUserScaling: bool,
    FontDefault: ?*Font,
    DisplayFramebufferScale: Vec2,
    MouseDrawCursor: bool,
    ConfigMacOSXBehaviors: bool,
    ConfigInputTrickleEventQueue: bool,
    ConfigInputTextCursorBlink: bool,
    ConfigDragClickToInputText: bool,
    ConfigWindowsResizeFromEdges: bool,
    ConfigWindowsMoveFromTitleBarOnly: bool,
    ConfigMemoryCompactTimer: f32,
    BackendPlatformName: ?[*:0]const u8,
    BackendRendererName: ?[*:0]const u8,
    BackendPlatformUserData: ?*anyopaque,
    BackendRendererUserData: ?*anyopaque,
    BackendLanguageUserData: ?*anyopaque,
    GetClipboardTextFn: ?fn (user_data: ?*anyopaque) callconv(.C) ?[*:0]const u8,
    SetClipboardTextFn: ?fn (user_data: ?*anyopaque, text: ?[*:0]const u8) callconv(.C) void,
    ClipboardUserData: ?*anyopaque,
    SetPlatformImeDataFn: ?fn (viewport: ?*Viewport, data: ?*PlatformImeData) callconv(.C) void,
    _UnusedPadding: ?*anyopaque,
    WantCaptureMouse: bool,
    WantCaptureKeyboard: bool,
    WantTextInput: bool,
    WantSetMousePos: bool,
    WantSaveIniSettings: bool,
    NavActive: bool,
    NavVisible: bool,
    Framerate: f32,
    MetricsRenderVertices: i32,
    MetricsRenderIndices: i32,
    MetricsRenderWindows: i32,
    MetricsActiveWindows: i32,
    MetricsActiveAllocations: i32,
    MouseDelta: Vec2,
    MousePos: Vec2,
    MouseDown: [5]bool,
    MouseWheel: f32,
    MouseWheelH: f32,
    KeyCtrl: bool,
    KeyShift: bool,
    KeyAlt: bool,
    KeySuper: bool,
    NavInputs: [NavInput.COUNT]f32,
    KeyMods: ModFlags align(4),
    KeysData: [Key.KeysData_SIZE]KeyData,
    WantCaptureMouseUnlessPopupClose: bool,
    MousePosPrev: Vec2,
    MouseClickedPos: [5]Vec2,
    MouseClickedTime: [5]f64,
    MouseClicked: [5]bool,
    MouseDoubleClicked: [5]bool,
    MouseClickedCount: [5]u16,
    MouseClickedLastCount: [5]u16,
    MouseReleased: [5]bool,
    MouseDownOwned: [5]bool,
    MouseDownOwnedUnlessPopupClose: [5]bool,
    MouseDownDuration: [5]f32,
    MouseDownDurationPrev: [5]f32,
    MouseDragMaxDistanceSqr: [5]f32,
    NavInputsDownDuration: [NavInput.COUNT]f32,
    NavInputsDownDurationPrev: [NavInput.COUNT]f32,
    PenPressure: f32,
    AppFocusLost: bool,
    AppAcceptingEvents: bool,
    BackendUsingLegacyKeyArrays: i8,
    BackendUsingLegacyNavInputArray: bool,
    InputQueueSurrogate: Wchar16,
    InputQueueCharacters: Vector(Wchar),

    /// AddFocusEvent(self: *IO, focused: bool) void
    pub const AddFocusEvent = raw.ImGuiIO_AddFocusEvent;

    /// AddInputCharacter(self: *IO, c: u32) void
    pub const AddInputCharacter = raw.ImGuiIO_AddInputCharacter;

    /// AddInputCharacterUTF16(self: *IO, c: Wchar16) void
    pub const AddInputCharacterUTF16 = raw.ImGuiIO_AddInputCharacterUTF16;

    /// AddInputCharactersUTF8(self: *IO, str: ?[*:0]const u8) void
    pub const AddInputCharactersUTF8 = raw.ImGuiIO_AddInputCharactersUTF8;

    /// AddKeyAnalogEvent(self: *IO, key: Key, down: bool, v: f32) void
    pub const AddKeyAnalogEvent = raw.ImGuiIO_AddKeyAnalogEvent;

    /// AddKeyEvent(self: *IO, key: Key, down: bool) void
    pub const AddKeyEvent = raw.ImGuiIO_AddKeyEvent;

    /// AddMouseButtonEvent(self: *IO, button: i32, down: bool) void
    pub const AddMouseButtonEvent = raw.ImGuiIO_AddMouseButtonEvent;

    /// AddMousePosEvent(self: *IO, x: f32, y: f32) void
    pub const AddMousePosEvent = raw.ImGuiIO_AddMousePosEvent;

    /// AddMouseWheelEvent(self: *IO, wh_x: f32, wh_y: f32) void
    pub const AddMouseWheelEvent = raw.ImGuiIO_AddMouseWheelEvent;

    /// ClearInputCharacters(self: *IO) void
    pub const ClearInputCharacters = raw.ImGuiIO_ClearInputCharacters;

    /// ClearInputKeys(self: *IO) void
    pub const ClearInputKeys = raw.ImGuiIO_ClearInputKeys;

    /// init_ImGuiIO(self: ?*anyopaque) void
    pub const init_ImGuiIO = raw.ImGuiIO_ImGuiIO;

    /// SetAppAcceptingEvents(self: *IO, accepting_events: bool) void
    pub const SetAppAcceptingEvents = raw.ImGuiIO_SetAppAcceptingEvents;

    /// SetKeyEventNativeDataExt(self: *IO, key: Key, native_keycode: i32, native_scancode: i32, native_legacy_index: i32) void
    pub const SetKeyEventNativeDataExt = raw.ImGuiIO_SetKeyEventNativeData;
    pub inline fn SetKeyEventNativeData(self: *IO, key: Key, native_keycode: i32, native_scancode: i32) void {
        return @This().SetKeyEventNativeDataExt(self, key, native_keycode, native_scancode, -1);
    }

    /// deinit(self: *IO) void
    pub const deinit = raw.ImGuiIO_destroy;
};

pub const InputEvent = extern struct {
    Type: InputEventType,
    Source: InputSource,
    value: extern union { MousePos: InputEventMousePos, MouseWheel: InputEventMouseWheel, MouseButton: InputEventMouseButton, Key: InputEventKey, Text: InputEventText, AppFocused: InputEventAppFocused },
    AddedByTestEngine: bool,

    /// init_ImGuiInputEvent(self: ?*anyopaque) void
    pub const init_ImGuiInputEvent = raw.ImGuiInputEvent_ImGuiInputEvent;

    /// deinit(self: *InputEvent) void
    pub const deinit = raw.ImGuiInputEvent_destroy;
};

pub const InputEventAppFocused = extern struct {
    Focused: bool,
};

pub const InputEventKey = extern struct {
    Key: Key,
    Down: bool,
    AnalogValue: f32,
};

pub const InputEventMouseButton = extern struct {
    Button: i32,
    Down: bool,
};

pub const InputEventMousePos = extern struct {
    PosX: f32,
    PosY: f32,
};

pub const InputEventMouseWheel = extern struct {
    WheelX: f32,
    WheelY: f32,
};

pub const InputEventText = extern struct {
    Char: u32,
};

pub const InputTextCallbackData = extern struct {
    EventFlag: InputTextFlags align(4),
    Flags: InputTextFlags align(4),
    UserData: ?*anyopaque,
    EventChar: Wchar,
    EventKey: Key,
    Buf: ?[*]u8,
    BufTextLen: i32,
    BufSize: i32,
    BufDirty: bool,
    CursorPos: i32,
    SelectionStart: i32,
    SelectionEnd: i32,

    /// ClearSelection(self: *InputTextCallbackData) void
    pub const ClearSelection = raw.ImGuiInputTextCallbackData_ClearSelection;

    /// DeleteChars(self: *InputTextCallbackData, pos: i32, bytes_count: i32) void
    pub const DeleteChars = raw.ImGuiInputTextCallbackData_DeleteChars;

    /// HasSelection(self: *const InputTextCallbackData) bool
    pub const HasSelection = raw.ImGuiInputTextCallbackData_HasSelection;

    /// init_ImGuiInputTextCallbackData(self: ?*anyopaque) void
    pub const init_ImGuiInputTextCallbackData = raw.ImGuiInputTextCallbackData_ImGuiInputTextCallbackData;

    /// InsertCharsExt(self: *InputTextCallbackData, pos: i32, text: ?[*]const u8, text_end: ?[*]const u8) void
    pub const InsertCharsExt = raw.ImGuiInputTextCallbackData_InsertChars;
    pub inline fn InsertChars(self: *InputTextCallbackData, pos: i32, text: ?[*]const u8) void {
        return @This().InsertCharsExt(self, pos, text, null);
    }

    /// SelectAll(self: *InputTextCallbackData) void
    pub const SelectAll = raw.ImGuiInputTextCallbackData_SelectAll;

    /// deinit(self: *InputTextCallbackData) void
    pub const deinit = raw.ImGuiInputTextCallbackData_destroy;
};

pub const InputTextState = extern struct {
    ID: ID,
    CurLenW: i32,
    CurLenA: i32,
    TextW: Vector(Wchar),
    TextA: Vector(u8),
    InitialTextA: Vector(u8),
    TextAIsValid: bool,
    BufCapacityA: i32,
    ScrollX: f32,
    Stb: STB_TexteditState,
    CursorAnim: f32,
    CursorFollow: bool,
    SelectedAllMouseLock: bool,
    Edited: bool,
    Flags: InputTextFlags align(4),

    /// ClearFreeMemory(self: *InputTextState) void
    pub const ClearFreeMemory = raw.ImGuiInputTextState_ClearFreeMemory;

    /// ClearSelection(self: *InputTextState) void
    pub const ClearSelection = raw.ImGuiInputTextState_ClearSelection;

    /// ClearText(self: *InputTextState) void
    pub const ClearText = raw.ImGuiInputTextState_ClearText;

    /// CursorAnimReset(self: *InputTextState) void
    pub const CursorAnimReset = raw.ImGuiInputTextState_CursorAnimReset;

    /// CursorClamp(self: *InputTextState) void
    pub const CursorClamp = raw.ImGuiInputTextState_CursorClamp;

    /// GetCursorPos(self: *const InputTextState) i32
    pub const GetCursorPos = raw.ImGuiInputTextState_GetCursorPos;

    /// GetRedoAvailCount(self: *const InputTextState) i32
    pub const GetRedoAvailCount = raw.ImGuiInputTextState_GetRedoAvailCount;

    /// GetSelectionEnd(self: *const InputTextState) i32
    pub const GetSelectionEnd = raw.ImGuiInputTextState_GetSelectionEnd;

    /// GetSelectionStart(self: *const InputTextState) i32
    pub const GetSelectionStart = raw.ImGuiInputTextState_GetSelectionStart;

    /// GetUndoAvailCount(self: *const InputTextState) i32
    pub const GetUndoAvailCount = raw.ImGuiInputTextState_GetUndoAvailCount;

    /// HasSelection(self: *const InputTextState) bool
    pub const HasSelection = raw.ImGuiInputTextState_HasSelection;

    /// init_ImGuiInputTextState(self: ?*anyopaque) void
    pub const init_ImGuiInputTextState = raw.ImGuiInputTextState_ImGuiInputTextState;

    /// OnKeyPressed(self: *InputTextState, key: i32) void
    pub const OnKeyPressed = raw.ImGuiInputTextState_OnKeyPressed;

    /// SelectAll(self: *InputTextState) void
    pub const SelectAll = raw.ImGuiInputTextState_SelectAll;

    /// deinit(self: *InputTextState) void
    pub const deinit = raw.ImGuiInputTextState_destroy;
};

pub const KeyData = extern struct {
    Down: bool,
    DownDuration: f32,
    DownDurationPrev: f32,
    AnalogValue: f32,
};

pub const LastItemData = extern struct {
    ID: ID,
    InFlags: ItemFlags align(4),
    StatusFlags: ItemStatusFlags align(4),
    Rect: Rect,
    NavRect: Rect,
    DisplayRect: Rect,

    /// init_ImGuiLastItemData(self: ?*anyopaque) void
    pub const init_ImGuiLastItemData = raw.ImGuiLastItemData_ImGuiLastItemData;

    /// deinit(self: *LastItemData) void
    pub const deinit = raw.ImGuiLastItemData_destroy;
};

pub const ListClipper = extern struct {
    DisplayStart: i32,
    DisplayEnd: i32,
    ItemsCount: i32,
    ItemsHeight: f32,
    StartPosY: f32,
    TempData: ?*anyopaque,

    /// BeginExt(self: *ListClipper, items_count: i32, items_height: f32) void
    pub const BeginExt = raw.ImGuiListClipper_Begin;
    pub inline fn Begin(self: *ListClipper, items_count: i32) void {
        return @This().BeginExt(self, items_count, -1.0);
    }

    /// End(self: *ListClipper) void
    pub const End = raw.ImGuiListClipper_End;

    /// ForceDisplayRangeByIndices(self: *ListClipper, item_min: i32, item_max: i32) void
    pub const ForceDisplayRangeByIndices = raw.ImGuiListClipper_ForceDisplayRangeByIndices;

    /// init_ImGuiListClipper(self: ?*anyopaque) void
    pub const init_ImGuiListClipper = raw.ImGuiListClipper_ImGuiListClipper;

    /// Step(self: *ListClipper) bool
    pub const Step = raw.ImGuiListClipper_Step;

    /// deinit(self: *ListClipper) void
    pub const deinit = raw.ImGuiListClipper_destroy;
};

pub const ListClipperData = extern struct {
    ListClipper: ?*ListClipper,
    LossynessOffset: f32,
    StepNo: i32,
    ItemsFrozen: i32,
    Ranges: Vector(ListClipperRange),

    /// init_ImGuiListClipperData(self: ?*anyopaque) void
    pub const init_ImGuiListClipperData = raw.ImGuiListClipperData_ImGuiListClipperData;

    /// Reset(self: *ListClipperData, clipper: ?*ListClipper) void
    pub const Reset = raw.ImGuiListClipperData_Reset;

    /// deinit(self: *ListClipperData) void
    pub const deinit = raw.ImGuiListClipperData_destroy;
};

pub const ListClipperRange = extern struct {
    Min: i32,
    Max: i32,
    PosToIndexConvert: bool,
    PosToIndexOffsetMin: i8,
    PosToIndexOffsetMax: i8,

    /// FromIndices(min: i32, max: i32) ListClipperRange
    pub const FromIndices = raw.ImGuiListClipperRange_FromIndices;

    /// FromPositions(y1: f32, y2: f32, off_min: i32, off_max: i32) ListClipperRange
    pub const FromPositions = raw.ImGuiListClipperRange_FromPositions;
};

pub const MenuColumns = extern struct {
    TotalWidth: u32,
    NextTotalWidth: u32,
    Spacing: u16,
    OffsetIcon: u16,
    OffsetLabel: u16,
    OffsetShortcut: u16,
    OffsetMark: u16,
    Widths: [4]u16,

    /// CalcNextTotalWidth(self: *MenuColumns, update_offsets: bool) void
    pub const CalcNextTotalWidth = raw.ImGuiMenuColumns_CalcNextTotalWidth;

    /// DeclColumns(self: *MenuColumns, w_icon: f32, w_label: f32, w_shortcut: f32, w_mark: f32) f32
    pub const DeclColumns = raw.ImGuiMenuColumns_DeclColumns;

    /// init_ImGuiMenuColumns(self: ?*anyopaque) void
    pub const init_ImGuiMenuColumns = raw.ImGuiMenuColumns_ImGuiMenuColumns;

    /// Update(self: *MenuColumns, spacing: f32, window_reappearing: bool) void
    pub const Update = raw.ImGuiMenuColumns_Update;

    /// deinit(self: *MenuColumns) void
    pub const deinit = raw.ImGuiMenuColumns_destroy;
};

pub const MetricsConfig = extern struct {
    ShowDebugLog: bool,
    ShowStackTool: bool,
    ShowWindowsRects: bool,
    ShowWindowsBeginOrder: bool,
    ShowTablesRects: bool,
    ShowDrawCmdMesh: bool,
    ShowDrawCmdBoundingBoxes: bool,
    ShowWindowsRectsType: i32,
    ShowTablesRectsType: i32,

    /// init_ImGuiMetricsConfig(self: ?*anyopaque) void
    pub const init_ImGuiMetricsConfig = raw.ImGuiMetricsConfig_ImGuiMetricsConfig;

    /// deinit(self: *MetricsConfig) void
    pub const deinit = raw.ImGuiMetricsConfig_destroy;
};

pub const NavItemData = extern struct {
    Window: ?*Window,
    ID: ID,
    FocusScopeId: ID,
    RectRel: Rect,
    InFlags: ItemFlags align(4),
    DistBox: f32,
    DistCenter: f32,
    DistAxial: f32,

    /// Clear(self: *NavItemData) void
    pub const Clear = raw.ImGuiNavItemData_Clear;

    /// init_ImGuiNavItemData(self: ?*anyopaque) void
    pub const init_ImGuiNavItemData = raw.ImGuiNavItemData_ImGuiNavItemData;

    /// deinit(self: *NavItemData) void
    pub const deinit = raw.ImGuiNavItemData_destroy;
};

pub const NextItemData = extern struct {
    Flags: NextItemDataFlags align(4),
    Width: f32,
    FocusScopeId: ID,
    OpenCond: CondFlags align(4),
    OpenVal: bool,

    /// ClearFlags(self: *NextItemData) void
    pub const ClearFlags = raw.ImGuiNextItemData_ClearFlags;

    /// init_ImGuiNextItemData(self: ?*anyopaque) void
    pub const init_ImGuiNextItemData = raw.ImGuiNextItemData_ImGuiNextItemData;

    /// deinit(self: *NextItemData) void
    pub const deinit = raw.ImGuiNextItemData_destroy;
};

pub const NextWindowData = extern struct {
    Flags: NextWindowDataFlags align(4),
    PosCond: CondFlags align(4),
    SizeCond: CondFlags align(4),
    CollapsedCond: CondFlags align(4),
    PosVal: Vec2,
    PosPivotVal: Vec2,
    SizeVal: Vec2,
    ContentSizeVal: Vec2,
    ScrollVal: Vec2,
    CollapsedVal: bool,
    SizeConstraintRect: Rect,
    SizeCallback: SizeCallback,
    SizeCallbackUserData: ?*anyopaque,
    BgAlphaVal: f32,
    MenuBarOffsetMinVal: Vec2,

    /// ClearFlags(self: *NextWindowData) void
    pub const ClearFlags = raw.ImGuiNextWindowData_ClearFlags;

    /// init_ImGuiNextWindowData(self: ?*anyopaque) void
    pub const init_ImGuiNextWindowData = raw.ImGuiNextWindowData_ImGuiNextWindowData;

    /// deinit(self: *NextWindowData) void
    pub const deinit = raw.ImGuiNextWindowData_destroy;
};

pub const OldColumnData = extern struct {
    OffsetNorm: f32,
    OffsetNormBeforeResize: f32,
    Flags: OldColumnFlags align(4),
    ClipRect: Rect,

    /// init_ImGuiOldColumnData(self: ?*anyopaque) void
    pub const init_ImGuiOldColumnData = raw.ImGuiOldColumnData_ImGuiOldColumnData;

    /// deinit(self: *OldColumnData) void
    pub const deinit = raw.ImGuiOldColumnData_destroy;
};

pub const OldColumns = extern struct {
    ID: ID,
    Flags: OldColumnFlags align(4),
    IsFirstFrame: bool,
    IsBeingResized: bool,
    Current: i32,
    Count: i32,
    OffMinX: f32,
    OffMaxX: f32,
    LineMinY: f32,
    LineMaxY: f32,
    HostCursorPosY: f32,
    HostCursorMaxPosX: f32,
    HostInitialClipRect: Rect,
    HostBackupClipRect: Rect,
    HostBackupParentWorkRect: Rect,
    Columns: Vector(OldColumnData),
    Splitter: DrawListSplitter,

    /// init_ImGuiOldColumns(self: ?*anyopaque) void
    pub const init_ImGuiOldColumns = raw.ImGuiOldColumns_ImGuiOldColumns;

    /// deinit(self: *OldColumns) void
    pub const deinit = raw.ImGuiOldColumns_destroy;
};

pub const OnceUponAFrame = extern struct {
    RefFrame: i32,

    /// init_ImGuiOnceUponAFrame(self: ?*anyopaque) void
    pub const init_ImGuiOnceUponAFrame = raw.ImGuiOnceUponAFrame_ImGuiOnceUponAFrame;

    /// deinit(self: *OnceUponAFrame) void
    pub const deinit = raw.ImGuiOnceUponAFrame_destroy;
};

pub const Payload = extern struct {
    Data: ?*anyopaque,
    DataSize: i32,
    SourceId: ID,
    SourceParentId: ID,
    DataFrameCount: i32,
    DataType: [32+1]u8,
    Preview: bool,
    Delivery: bool,

    /// Clear(self: *Payload) void
    pub const Clear = raw.ImGuiPayload_Clear;

    /// init_ImGuiPayload(self: ?*anyopaque) void
    pub const init_ImGuiPayload = raw.ImGuiPayload_ImGuiPayload;

    /// IsDataType(self: *const Payload, kind: ?[*:0]const u8) bool
    pub const IsDataType = raw.ImGuiPayload_IsDataType;

    /// IsDelivery(self: *const Payload) bool
    pub const IsDelivery = raw.ImGuiPayload_IsDelivery;

    /// IsPreview(self: *const Payload) bool
    pub const IsPreview = raw.ImGuiPayload_IsPreview;

    /// deinit(self: *Payload) void
    pub const deinit = raw.ImGuiPayload_destroy;
};

pub const PlatformImeData = extern struct {
    WantVisible: bool,
    InputPos: Vec2,
    InputLineHeight: f32,

    /// init_ImGuiPlatformImeData(self: ?*anyopaque) void
    pub const init_ImGuiPlatformImeData = raw.ImGuiPlatformImeData_ImGuiPlatformImeData;

    /// deinit(self: *PlatformImeData) void
    pub const deinit = raw.ImGuiPlatformImeData_destroy;
};

pub const PopupData = extern struct {
    PopupId: ID,
    Window: ?*Window,
    SourceWindow: ?*Window,
    ParentNavLayer: i32,
    OpenFrameCount: i32,
    OpenParentId: ID,
    OpenPopupPos: Vec2,
    OpenMousePos: Vec2,

    /// init_ImGuiPopupData(self: ?*anyopaque) void
    pub const init_ImGuiPopupData = raw.ImGuiPopupData_ImGuiPopupData;

    /// deinit(self: *PopupData) void
    pub const deinit = raw.ImGuiPopupData_destroy;
};

pub const PtrOrIndex = extern struct {
    Ptr: ?*anyopaque,
    Index: i32,

    /// init_Ptr(self: ?*anyopaque, ptr: ?*anyopaque) void
    pub const init_Ptr = raw.ImGuiPtrOrIndex_ImGuiPtrOrIndex_Ptr;

    /// init_Int(self: ?*anyopaque, index: i32) void
    pub const init_Int = raw.ImGuiPtrOrIndex_ImGuiPtrOrIndex_Int;

    /// deinit(self: *PtrOrIndex) void
    pub const deinit = raw.ImGuiPtrOrIndex_destroy;
};

pub const SettingsHandler = extern struct {
    TypeName: ?[*:0]const u8,
    TypeHash: ID,
    ClearAllFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler) callconv(.C) void,
    ReadInitFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler) callconv(.C) void,
    ReadOpenFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler, name: ?[*:0]const u8) callconv(.C) ?*anyopaque,
    ReadLineFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler, entry: ?*anyopaque, line: [*c]const u8) callconv(.C) void,
    ApplyAllFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler) callconv(.C) void,
    WriteAllFn: ?fn (ctx: ?*Context, handler: ?*SettingsHandler, out_buf: ?*TextBuffer) callconv(.C) void,
    UserData: ?*anyopaque,

    /// init_ImGuiSettingsHandler(self: ?*anyopaque) void
    pub const init_ImGuiSettingsHandler = raw.ImGuiSettingsHandler_ImGuiSettingsHandler;

    /// deinit(self: *SettingsHandler) void
    pub const deinit = raw.ImGuiSettingsHandler_destroy;
};

pub const ShrinkWidthItem = extern struct {
    Index: i32,
    Width: f32,
    InitialWidth: f32,
};

pub const SizeCallbackData = extern struct {
    UserData: ?*anyopaque,
    Pos: Vec2,
    CurrentSize: Vec2,
    DesiredSize: Vec2,
};

pub const StackLevelInfo = extern struct {
    ID: ID,
    QueryFrameCount: i8,
    QuerySuccess: bool,
    DataType: DataType,
    Desc: [57]u8,

    /// init_ImGuiStackLevelInfo(self: ?*anyopaque) void
    pub const init_ImGuiStackLevelInfo = raw.ImGuiStackLevelInfo_ImGuiStackLevelInfo;

    /// deinit(self: *StackLevelInfo) void
    pub const deinit = raw.ImGuiStackLevelInfo_destroy;
};

pub const StackSizes = extern struct {
    SizeOfIDStack: i16,
    SizeOfColorStack: i16,
    SizeOfStyleVarStack: i16,
    SizeOfFontStack: i16,
    SizeOfFocusScopeStack: i16,
    SizeOfGroupStack: i16,
    SizeOfItemFlagsStack: i16,
    SizeOfBeginPopupStack: i16,
    SizeOfDisabledStack: i16,

    /// CompareWithCurrentState(self: *StackSizes) void
    pub const CompareWithCurrentState = raw.ImGuiStackSizes_CompareWithCurrentState;

    /// init_ImGuiStackSizes(self: ?*anyopaque) void
    pub const init_ImGuiStackSizes = raw.ImGuiStackSizes_ImGuiStackSizes;

    /// SetToCurrentState(self: *StackSizes) void
    pub const SetToCurrentState = raw.ImGuiStackSizes_SetToCurrentState;

    /// deinit(self: *StackSizes) void
    pub const deinit = raw.ImGuiStackSizes_destroy;
};

pub const StackTool = extern struct {
    LastActiveFrame: i32,
    StackLevel: i32,
    QueryId: ID,
    Results: Vector(StackLevelInfo),
    CopyToClipboardOnCtrlC: bool,
    CopyToClipboardLastTime: f32,

    /// init_ImGuiStackTool(self: ?*anyopaque) void
    pub const init_ImGuiStackTool = raw.ImGuiStackTool_ImGuiStackTool;

    /// deinit(self: *StackTool) void
    pub const deinit = raw.ImGuiStackTool_destroy;
};

pub const Storage = extern struct {
    Data: Vector(StoragePair),

    /// BuildSortByKey(self: *Storage) void
    pub const BuildSortByKey = raw.ImGuiStorage_BuildSortByKey;

    /// Clear(self: *Storage) void
    pub const Clear = raw.ImGuiStorage_Clear;

    /// GetBoolExt(self: *const Storage, key: ID, default_val: bool) bool
    pub const GetBoolExt = raw.ImGuiStorage_GetBool;
    pub inline fn GetBool(self: *const Storage, key: ID) bool {
        return @This().GetBoolExt(self, key, false);
    }

    /// GetBoolRefExt(self: *Storage, key: ID, default_val: bool) ?*bool
    pub const GetBoolRefExt = raw.ImGuiStorage_GetBoolRef;
    pub inline fn GetBoolRef(self: *Storage, key: ID) ?*bool {
        return @This().GetBoolRefExt(self, key, false);
    }

    /// GetFloatExt(self: *const Storage, key: ID, default_val: f32) f32
    pub const GetFloatExt = raw.ImGuiStorage_GetFloat;
    pub inline fn GetFloat(self: *const Storage, key: ID) f32 {
        return @This().GetFloatExt(self, key, 0.0);
    }

    /// GetFloatRefExt(self: *Storage, key: ID, default_val: f32) ?*f32
    pub const GetFloatRefExt = raw.ImGuiStorage_GetFloatRef;
    pub inline fn GetFloatRef(self: *Storage, key: ID) ?*f32 {
        return @This().GetFloatRefExt(self, key, 0.0);
    }

    /// GetIntExt(self: *const Storage, key: ID, default_val: i32) i32
    pub const GetIntExt = raw.ImGuiStorage_GetInt;
    pub inline fn GetInt(self: *const Storage, key: ID) i32 {
        return @This().GetIntExt(self, key, 0);
    }

    /// GetIntRefExt(self: *Storage, key: ID, default_val: i32) ?*i32
    pub const GetIntRefExt = raw.ImGuiStorage_GetIntRef;
    pub inline fn GetIntRef(self: *Storage, key: ID) ?*i32 {
        return @This().GetIntRefExt(self, key, 0);
    }

    /// GetVoidPtr(self: *const Storage, key: ID) ?*anyopaque
    pub const GetVoidPtr = raw.ImGuiStorage_GetVoidPtr;

    /// GetVoidPtrRefExt(self: *Storage, key: ID, default_val: ?*anyopaque) ?*?*anyopaque
    pub const GetVoidPtrRefExt = raw.ImGuiStorage_GetVoidPtrRef;
    pub inline fn GetVoidPtrRef(self: *Storage, key: ID) ?*?*anyopaque {
        return @This().GetVoidPtrRefExt(self, key, null);
    }

    /// SetAllInt(self: *Storage, val: i32) void
    pub const SetAllInt = raw.ImGuiStorage_SetAllInt;

    /// SetBool(self: *Storage, key: ID, val: bool) void
    pub const SetBool = raw.ImGuiStorage_SetBool;

    /// SetFloat(self: *Storage, key: ID, val: f32) void
    pub const SetFloat = raw.ImGuiStorage_SetFloat;

    /// SetInt(self: *Storage, key: ID, val: i32) void
    pub const SetInt = raw.ImGuiStorage_SetInt;

    /// SetVoidPtr(self: *Storage, key: ID, val: ?*anyopaque) void
    pub const SetVoidPtr = raw.ImGuiStorage_SetVoidPtr;
};

pub const StoragePair = extern struct {
    key: ID,
    value: extern union { val_i: i32, val_f: f32, val_p: ?*anyopaque },

    /// init_Int(self: ?*anyopaque, _key: ID, _val_i: i32) void
    pub const init_Int = raw.ImGuiStoragePair_ImGuiStoragePair_Int;

    /// init_Float(self: ?*anyopaque, _key: ID, _val_f: f32) void
    pub const init_Float = raw.ImGuiStoragePair_ImGuiStoragePair_Float;

    /// init_Ptr(self: ?*anyopaque, _key: ID, _val_p: ?*anyopaque) void
    pub const init_Ptr = raw.ImGuiStoragePair_ImGuiStoragePair_Ptr;

    /// deinit(self: *StoragePair) void
    pub const deinit = raw.ImGuiStoragePair_destroy;
};

pub const Style = extern struct {
    Alpha: f32,
    DisabledAlpha: f32,
    WindowPadding: Vec2,
    WindowRounding: f32,
    WindowBorderSize: f32,
    WindowMinSize: Vec2,
    WindowTitleAlign: Vec2,
    WindowMenuButtonPosition: Dir,
    ChildRounding: f32,
    ChildBorderSize: f32,
    PopupRounding: f32,
    PopupBorderSize: f32,
    FramePadding: Vec2,
    FrameRounding: f32,
    FrameBorderSize: f32,
    ItemSpacing: Vec2,
    ItemInnerSpacing: Vec2,
    CellPadding: Vec2,
    TouchExtraPadding: Vec2,
    IndentSpacing: f32,
    ColumnsMinSpacing: f32,
    ScrollbarSize: f32,
    ScrollbarRounding: f32,
    GrabMinSize: f32,
    GrabRounding: f32,
    LogSliderDeadzone: f32,
    TabRounding: f32,
    TabBorderSize: f32,
    TabMinWidthForCloseButton: f32,
    ColorButtonPosition: Dir,
    ButtonTextAlign: Vec2,
    SelectableTextAlign: Vec2,
    DisplayWindowPadding: Vec2,
    DisplaySafeAreaPadding: Vec2,
    MouseCursorScale: f32,
    AntiAliasedLines: bool,
    AntiAliasedLinesUseTex: bool,
    AntiAliasedFill: bool,
    CurveTessellationTol: f32,
    CircleTessellationMaxError: f32,
    Colors: [Col.COUNT]Vec4,

    /// init_ImGuiStyle(self: ?*anyopaque) void
    pub const init_ImGuiStyle = raw.ImGuiStyle_ImGuiStyle;

    /// ScaleAllSizes(self: *Style, scale_factor: f32) void
    pub const ScaleAllSizes = raw.ImGuiStyle_ScaleAllSizes;

    /// deinit(self: *Style) void
    pub const deinit = raw.ImGuiStyle_destroy;
};

pub const StyleMod = extern struct {
    VarIdx: StyleVar,
    value: extern union { BackupInt[2]: i32, BackupFloat[2]: f32 },

    /// init_Int(self: ?*anyopaque, idx: StyleVar, v: i32) void
    pub const init_Int = raw.ImGuiStyleMod_ImGuiStyleMod_Int;

    /// init_Float(self: ?*anyopaque, idx: StyleVar, v: f32) void
    pub const init_Float = raw.ImGuiStyleMod_ImGuiStyleMod_Float;

    pub inline fn init_Vec2(self: ?*anyopaque, idx: StyleVar, v: Vec2) void {
        return raw.ImGuiStyleMod_ImGuiStyleMod_Vec2(self, idx, &v);
    }

    /// deinit(self: *StyleMod) void
    pub const deinit = raw.ImGuiStyleMod_destroy;
};

pub const TabBar = extern struct {
    Tabs: Vector(TabItem),
    Flags: TabBarFlags align(4),
    ID: ID,
    SelectedTabId: ID,
    NextSelectedTabId: ID,
    VisibleTabId: ID,
    CurrFrameVisible: i32,
    PrevFrameVisible: i32,
    BarRect: Rect,
    CurrTabsContentsHeight: f32,
    PrevTabsContentsHeight: f32,
    WidthAllTabs: f32,
    WidthAllTabsIdeal: f32,
    ScrollingAnim: f32,
    ScrollingTarget: f32,
    ScrollingTargetDistToVisibility: f32,
    ScrollingSpeed: f32,
    ScrollingRectMinX: f32,
    ScrollingRectMaxX: f32,
    ReorderRequestTabId: ID,
    ReorderRequestOffset: i16,
    BeginCount: i8,
    WantLayout: bool,
    VisibleTabWasSubmitted: bool,
    TabsAddedNew: bool,
    TabsActiveCount: i16,
    LastTabItemIdx: i16,
    ItemSpacingY: f32,
    FramePadding: Vec2,
    BackupCursorPos: Vec2,
    TabsNames: TextBuffer,

    /// GetTabName(self: *const TabBar, tab: ?*const TabItem) ?[*:0]const u8
    pub const GetTabName = raw.ImGuiTabBar_GetTabName;

    /// GetTabOrder(self: *const TabBar, tab: ?*const TabItem) i32
    pub const GetTabOrder = raw.ImGuiTabBar_GetTabOrder;

    /// init_ImGuiTabBar(self: ?*anyopaque) void
    pub const init_ImGuiTabBar = raw.ImGuiTabBar_ImGuiTabBar;

    /// deinit(self: *TabBar) void
    pub const deinit = raw.ImGuiTabBar_destroy;
};

pub const TabItem = extern struct {
    ID: ID,
    Flags: TabItemFlags align(4),
    LastFrameVisible: i32,
    LastFrameSelected: i32,
    Offset: f32,
    Width: f32,
    ContentWidth: f32,
    RequestedWidth: f32,
    NameOffset: i32,
    BeginOrder: i16,
    IndexDuringLayout: i16,
    WantClose: bool,

    /// init_ImGuiTabItem(self: ?*anyopaque) void
    pub const init_ImGuiTabItem = raw.ImGuiTabItem_ImGuiTabItem;

    /// deinit(self: *TabItem) void
    pub const deinit = raw.ImGuiTabItem_destroy;
};

pub const Table = extern struct {
    ID: ID,
    Flags: TableFlags align(4),
    RawData: ?*anyopaque,
    TempData: ?*TableTempData,
    Columns: Span_ImGuiTableColumn,
    DisplayOrderToIndex: Span_ImGuiTableColumnIdx,
    RowCellData: Span_ImGuiTableCellData,
    EnabledMaskByDisplayOrder: u64,
    EnabledMaskByIndex: u64,
    VisibleMaskByIndex: u64,
    RequestOutputMaskByIndex: u64,
    SettingsLoadedFlags: TableFlags align(4),
    SettingsOffset: i32,
    LastFrameActive: i32,
    ColumnsCount: i32,
    CurrentRow: i32,
    CurrentColumn: i32,
    InstanceCurrent: i16,
    InstanceInteracted: i16,
    RowPosY1: f32,
    RowPosY2: f32,
    RowMinHeight: f32,
    RowTextBaseline: f32,
    RowIndentOffsetX: f32,
    RowFlags: TableRowFlags align(4),
    LastRowFlags: TableRowFlags align(4),
    RowBgColorCounter: i32,
    RowBgColor: [2]u32,
    BorderColorStrong: u32,
    BorderColorLight: u32,
    BorderX1: f32,
    BorderX2: f32,
    HostIndentX: f32,
    MinColumnWidth: f32,
    OuterPaddingX: f32,
    CellPaddingX: f32,
    CellPaddingY: f32,
    CellSpacingX1: f32,
    CellSpacingX2: f32,
    InnerWidth: f32,
    ColumnsGivenWidth: f32,
    ColumnsAutoFitWidth: f32,
    ColumnsStretchSumWeights: f32,
    ResizedColumnNextWidth: f32,
    ResizeLockMinContentsX2: f32,
    RefScale: f32,
    OuterRect: Rect,
    InnerRect: Rect,
    WorkRect: Rect,
    InnerClipRect: Rect,
    BgClipRect: Rect,
    Bg0ClipRectForDrawCmd: Rect,
    Bg2ClipRectForDrawCmd: Rect,
    HostClipRect: Rect,
    HostBackupInnerClipRect: Rect,
    OuterWindow: ?*Window,
    InnerWindow: ?*Window,
    ColumnsNames: TextBuffer,
    DrawSplitter: ?*DrawListSplitter,
    InstanceDataFirst: TableInstanceData,
    InstanceDataExtra: Vector(TableInstanceData),
    SortSpecsSingle: TableColumnSortSpecs,
    SortSpecsMulti: Vector(TableColumnSortSpecs),
    SortSpecs: TableSortSpecs,
    SortSpecsCount: TableColumnIdx,
    ColumnsEnabledCount: TableColumnIdx,
    ColumnsEnabledFixedCount: TableColumnIdx,
    DeclColumnsCount: TableColumnIdx,
    HoveredColumnBody: TableColumnIdx,
    HoveredColumnBorder: TableColumnIdx,
    AutoFitSingleColumn: TableColumnIdx,
    ResizedColumn: TableColumnIdx,
    LastResizedColumn: TableColumnIdx,
    HeldHeaderColumn: TableColumnIdx,
    ReorderColumn: TableColumnIdx,
    ReorderColumnDir: TableColumnIdx,
    LeftMostEnabledColumn: TableColumnIdx,
    RightMostEnabledColumn: TableColumnIdx,
    LeftMostStretchedColumn: TableColumnIdx,
    RightMostStretchedColumn: TableColumnIdx,
    ContextPopupColumn: TableColumnIdx,
    FreezeRowsRequest: TableColumnIdx,
    FreezeRowsCount: TableColumnIdx,
    FreezeColumnsRequest: TableColumnIdx,
    FreezeColumnsCount: TableColumnIdx,
    RowCellDataCurrent: TableColumnIdx,
    DummyDrawChannel: TableDrawChannelIdx,
    Bg2DrawChannelCurrent: TableDrawChannelIdx,
    Bg2DrawChannelUnfrozen: TableDrawChannelIdx,
    IsLayoutLocked: bool,
    IsInsideRow: bool,
    IsInitializing: bool,
    IsSortSpecsDirty: bool,
    IsUsingHeaders: bool,
    IsContextPopupOpen: bool,
    IsSettingsRequestLoad: bool,
    IsSettingsDirty: bool,
    IsDefaultDisplayOrder: bool,
    IsResetAllRequest: bool,
    IsResetDisplayOrderRequest: bool,
    IsUnfrozenRows: bool,
    IsDefaultSizingPolicy: bool,
    MemoryCompacted: bool,
    HostSkipItems: bool,

    /// init_ImGuiTable(self: ?*anyopaque) void
    pub const init_ImGuiTable = raw.ImGuiTable_ImGuiTable;

    /// deinit(self: *Table) void
    pub const deinit = raw.ImGuiTable_destroy;
};

pub const TableCellData = extern struct {
    BgColor: u32,
    Column: TableColumnIdx,
};

pub const TableColumn = extern struct {
    Flags: TableColumnFlags align(4),
    WidthGiven: f32,
    MinX: f32,
    MaxX: f32,
    WidthRequest: f32,
    WidthAuto: f32,
    StretchWeight: f32,
    InitStretchWeightOrWidth: f32,
    ClipRect: Rect,
    UserID: ID,
    WorkMinX: f32,
    WorkMaxX: f32,
    ItemWidth: f32,
    ContentMaxXFrozen: f32,
    ContentMaxXUnfrozen: f32,
    ContentMaxXHeadersUsed: f32,
    ContentMaxXHeadersIdeal: f32,
    NameOffset: i16,
    DisplayOrder: TableColumnIdx,
    IndexWithinEnabledSet: TableColumnIdx,
    PrevEnabledColumn: TableColumnIdx,
    NextEnabledColumn: TableColumnIdx,
    SortOrder: TableColumnIdx,
    DrawChannelCurrent: TableDrawChannelIdx,
    DrawChannelFrozen: TableDrawChannelIdx,
    DrawChannelUnfrozen: TableDrawChannelIdx,
    IsEnabled: bool,
    IsUserEnabled: bool,
    IsUserEnabledNextFrame: bool,
    IsVisibleX: bool,
    IsVisibleY: bool,
    IsRequestOutput: bool,
    IsSkipItems: bool,
    IsPreserveWidthAuto: bool,
    NavLayerCurrent: i8,
    AutoFitQueue: u8,
    CannotSkipItemsQueue: u8,
    SortDirection: u8,
    SortDirectionsAvailCount: u8,
    SortDirectionsAvailMask: u8,
    SortDirectionsAvailList: u8,

    /// init_ImGuiTableColumn(self: ?*anyopaque) void
    pub const init_ImGuiTableColumn = raw.ImGuiTableColumn_ImGuiTableColumn;

    /// deinit(self: *TableColumn) void
    pub const deinit = raw.ImGuiTableColumn_destroy;
};

pub const TableColumnSettings = extern struct {
    WidthOrWeight: f32,
    UserID: ID,
    Index: TableColumnIdx,
    DisplayOrder: TableColumnIdx,
    SortOrder: TableColumnIdx,
    SortDirection: u8,
    IsEnabled: u8,
    IsStretch: u8,

    /// init_ImGuiTableColumnSettings(self: ?*anyopaque) void
    pub const init_ImGuiTableColumnSettings = raw.ImGuiTableColumnSettings_ImGuiTableColumnSettings;

    /// deinit(self: *TableColumnSettings) void
    pub const deinit = raw.ImGuiTableColumnSettings_destroy;
};

pub const TableColumnSortSpecs = extern struct {
    ColumnUserID: ID,
    ColumnIndex: i16,
    SortOrder: i16,
    SortDirection: SortDirection,

    /// init_ImGuiTableColumnSortSpecs(self: ?*anyopaque) void
    pub const init_ImGuiTableColumnSortSpecs = raw.ImGuiTableColumnSortSpecs_ImGuiTableColumnSortSpecs;

    /// deinit(self: *TableColumnSortSpecs) void
    pub const deinit = raw.ImGuiTableColumnSortSpecs_destroy;
};

pub const TableInstanceData = extern struct {
    LastOuterHeight: f32,
    LastFirstRowHeight: f32,

    /// init_ImGuiTableInstanceData(self: ?*anyopaque) void
    pub const init_ImGuiTableInstanceData = raw.ImGuiTableInstanceData_ImGuiTableInstanceData;

    /// deinit(self: *TableInstanceData) void
    pub const deinit = raw.ImGuiTableInstanceData_destroy;
};

pub const TableSettings = extern struct {
    ID: ID,
    SaveFlags: TableFlags align(4),
    RefScale: f32,
    ColumnsCount: TableColumnIdx,
    ColumnsCountMax: TableColumnIdx,
    WantApply: bool,

    /// GetColumnSettings(self: *TableSettings) ?*TableColumnSettings
    pub const GetColumnSettings = raw.ImGuiTableSettings_GetColumnSettings;

    /// init_ImGuiTableSettings(self: ?*anyopaque) void
    pub const init_ImGuiTableSettings = raw.ImGuiTableSettings_ImGuiTableSettings;

    /// deinit(self: *TableSettings) void
    pub const deinit = raw.ImGuiTableSettings_destroy;
};

pub const TableSortSpecs = extern struct {
    Specs: ?[*]const TableColumnSortSpecs,
    SpecsCount: i32,
    SpecsDirty: bool,

    /// init_ImGuiTableSortSpecs(self: ?*anyopaque) void
    pub const init_ImGuiTableSortSpecs = raw.ImGuiTableSortSpecs_ImGuiTableSortSpecs;

    /// deinit(self: *TableSortSpecs) void
    pub const deinit = raw.ImGuiTableSortSpecs_destroy;
};

pub const TableTempData = extern struct {
    TableIndex: i32,
    LastTimeActive: f32,
    UserOuterSize: Vec2,
    DrawSplitter: DrawListSplitter,
    HostBackupWorkRect: Rect,
    HostBackupParentWorkRect: Rect,
    HostBackupPrevLineSize: Vec2,
    HostBackupCurrLineSize: Vec2,
    HostBackupCursorMaxPos: Vec2,
    HostBackupColumnsOffset: Vec1,
    HostBackupItemWidth: f32,
    HostBackupItemWidthStackSize: i32,

    /// init_ImGuiTableTempData(self: ?*anyopaque) void
    pub const init_ImGuiTableTempData = raw.ImGuiTableTempData_ImGuiTableTempData;

    /// deinit(self: *TableTempData) void
    pub const deinit = raw.ImGuiTableTempData_destroy;
};

pub const TextBuffer = extern struct {
    Buf: Vector(u8),

    /// init_ImGuiTextBuffer(self: ?*anyopaque) void
    pub const init_ImGuiTextBuffer = raw.ImGuiTextBuffer_ImGuiTextBuffer;

    /// appendExt(self: *TextBuffer, str: ?[*]const u8, str_end: ?[*]const u8) void
    pub const appendExt = raw.ImGuiTextBuffer_append;
    pub inline fn append(self: *TextBuffer, str: ?[*]const u8) void {
        return @This().appendExt(self, str, null);
    }

    /// appendf(self: *TextBuffer, fmt: ?[*:0]const u8, ...: ...) void
    pub const appendf = raw.ImGuiTextBuffer_appendf;

    /// begin(self: *const TextBuffer) [*]const u8
    pub const begin = raw.ImGuiTextBuffer_begin;

    /// c_str(self: *const TextBuffer) [*:0]const u8
    pub const c_str = raw.ImGuiTextBuffer_c_str;

    /// clear(self: *TextBuffer) void
    pub const clear = raw.ImGuiTextBuffer_clear;

    /// deinit(self: *TextBuffer) void
    pub const deinit = raw.ImGuiTextBuffer_destroy;

    /// empty(self: *const TextBuffer) bool
    pub const empty = raw.ImGuiTextBuffer_empty;

    /// end(self: *const TextBuffer) [*]const u8
    pub const end = raw.ImGuiTextBuffer_end;

    /// reserve(self: *TextBuffer, capacity: i32) void
    pub const reserve = raw.ImGuiTextBuffer_reserve;

    /// size(self: *const TextBuffer) i32
    pub const size = raw.ImGuiTextBuffer_size;
};

pub const TextFilter = extern struct {
    InputBuf: [256]u8,
    Filters: Vector(TextRange),
    CountGrep: i32,

    /// Build(self: *TextFilter) void
    pub const Build = raw.ImGuiTextFilter_Build;

    /// Clear(self: *TextFilter) void
    pub const Clear = raw.ImGuiTextFilter_Clear;

    /// DrawExt(self: *TextFilter, label: ?[*:0]const u8, width: f32) bool
    pub const DrawExt = raw.ImGuiTextFilter_Draw;
    pub inline fn Draw(self: *TextFilter) bool {
        return @This().DrawExt(self, "Filter(inc,-exc)", 0.0);
    }

    /// init_ImGuiTextFilterExt(self: ?*anyopaque, default_filter: ?[*:0]const u8) void
    pub const init_ImGuiTextFilterExt = raw.ImGuiTextFilter_ImGuiTextFilter;
    pub inline fn init_ImGuiTextFilter(self: ?*anyopaque) void {
        return @This().init_ImGuiTextFilterExt(self, "");
    }

    /// IsActive(self: *const TextFilter) bool
    pub const IsActive = raw.ImGuiTextFilter_IsActive;

    /// PassFilterExt(self: *const TextFilter, text: ?[*]const u8, text_end: ?[*]const u8) bool
    pub const PassFilterExt = raw.ImGuiTextFilter_PassFilter;
    pub inline fn PassFilter(self: *const TextFilter, text: ?[*]const u8) bool {
        return @This().PassFilterExt(self, text, null);
    }

    /// deinit(self: *TextFilter) void
    pub const deinit = raw.ImGuiTextFilter_destroy;
};

pub const TextRange = extern struct {
    b: ?[*]const u8,
    e: ?[*]const u8,

    /// init_Nil(self: ?*anyopaque) void
    pub const init_Nil = raw.ImGuiTextRange_ImGuiTextRange_Nil;

    /// init_Str(self: ?*anyopaque, _b: ?[*]const u8, _e: ?[*]const u8) void
    pub const init_Str = raw.ImGuiTextRange_ImGuiTextRange_Str;

    /// deinit(self: *TextRange) void
    pub const deinit = raw.ImGuiTextRange_destroy;

    /// empty(self: *const TextRange) bool
    pub const empty = raw.ImGuiTextRange_empty;

    /// split(self: *const TextRange, separator: u8, out: ?*Vector(TextRange)) void
    pub const split = raw.ImGuiTextRange_split;
};

pub const Viewport = extern struct {
    Flags: ViewportFlags align(4),
    Pos: Vec2,
    Size: Vec2,
    WorkPos: Vec2,
    WorkSize: Vec2,
    PlatformHandleRaw: ?*anyopaque,

    pub inline fn GetCenter(self: *const Viewport) Vec2 {
        var out: Vec2 = undefined;
        raw.ImGuiViewport_GetCenter(&out, self);
        return out;
    }

    pub inline fn GetWorkCenter(self: *const Viewport) Vec2 {
        var out: Vec2 = undefined;
        raw.ImGuiViewport_GetWorkCenter(&out, self);
        return out;
    }

    /// init_ImGuiViewport(self: ?*anyopaque) void
    pub const init_ImGuiViewport = raw.ImGuiViewport_ImGuiViewport;

    /// deinit(self: *Viewport) void
    pub const deinit = raw.ImGuiViewport_destroy;
};

pub const ViewportP = extern struct {
    _ImGuiViewport: Viewport,
    DrawListsLastFrame: [2]i32,
    DrawLists: [2][*c]DrawList,
    DrawDataP: DrawData,
    DrawDataBuilder: DrawDataBuilder,
    WorkOffsetMin: Vec2,
    WorkOffsetMax: Vec2,
    BuildWorkOffsetMin: Vec2,
    BuildWorkOffsetMax: Vec2,

    pub inline fn CalcWorkRectPos(self: *const ViewportP, off_min: ?*const Vec2) Vec2 {
        var out: Vec2 = undefined;
        raw.ImGuiViewportP_CalcWorkRectPos(&out, self, off_min);
        return out;
    }

    pub inline fn CalcWorkRectSize(self: *const ViewportP, off_min: ?*const Vec2, off_max: ?*const Vec2) Vec2 {
        var out: Vec2 = undefined;
        raw.ImGuiViewportP_CalcWorkRectSize(&out, self, off_min, off_max);
        return out;
    }

    pub inline fn GetBuildWorkRect(self: *const ViewportP) Rect {
        var out: Rect = undefined;
        raw.ImGuiViewportP_GetBuildWorkRect(&out, self);
        return out;
    }

    pub inline fn GetMainRect(self: *const ViewportP) Rect {
        var out: Rect = undefined;
        raw.ImGuiViewportP_GetMainRect(&out, self);
        return out;
    }

    pub inline fn GetWorkRect(self: *const ViewportP) Rect {
        var out: Rect = undefined;
        raw.ImGuiViewportP_GetWorkRect(&out, self);
        return out;
    }

    /// init_ImGuiViewportP(self: ?*anyopaque) void
    pub const init_ImGuiViewportP = raw.ImGuiViewportP_ImGuiViewportP;

    /// UpdateWorkRect(self: *ViewportP) void
    pub const UpdateWorkRect = raw.ImGuiViewportP_UpdateWorkRect;

    /// deinit(self: *ViewportP) void
    pub const deinit = raw.ImGuiViewportP_destroy;
};

pub const Window = extern struct {
    Name: ?[*:0]u8,
    ID: ID,
    Flags: WindowFlags align(4),
    Viewport: ?*ViewportP,
    Pos: Vec2,
    Size: Vec2,
    SizeFull: Vec2,
    ContentSize: Vec2,
    ContentSizeIdeal: Vec2,
    ContentSizeExplicit: Vec2,
    WindowPadding: Vec2,
    WindowRounding: f32,
    WindowBorderSize: f32,
    NameBufLen: i32,
    MoveId: ID,
    ChildId: ID,
    Scroll: Vec2,
    ScrollMax: Vec2,
    ScrollTarget: Vec2,
    ScrollTargetCenterRatio: Vec2,
    ScrollTargetEdgeSnapDist: Vec2,
    ScrollbarSizes: Vec2,
    ScrollbarX: bool,
    ScrollbarY: bool,
    Active: bool,
    WasActive: bool,
    WriteAccessed: bool,
    Collapsed: bool,
    WantCollapseToggle: bool,
    SkipItems: bool,
    Appearing: bool,
    Hidden: bool,
    IsFallbackWindow: bool,
    IsExplicitChild: bool,
    HasCloseButton: bool,
    ResizeBorderHeld: signed char,
    BeginCount: i16,
    BeginOrderWithinParent: i16,
    BeginOrderWithinContext: i16,
    FocusOrder: i16,
    PopupId: ID,
    AutoFitFramesX: i8,
    AutoFitFramesY: i8,
    AutoFitChildAxises: i8,
    AutoFitOnlyGrows: bool,
    AutoPosLastDirection: Dir,
    HiddenFramesCanSkipItems: i8,
    HiddenFramesCannotSkipItems: i8,
    HiddenFramesForRenderOnly: i8,
    DisableInputsFrames: i8,
    SetWindowPosAllowFlags: CondFlags align(4),
    SetWindowSizeAllowFlags: CondFlags align(4),
    SetWindowCollapsedAllowFlags: CondFlags align(4),
    SetWindowPosVal: Vec2,
    SetWindowPosPivot: Vec2,
    IDStack: Vector(ID),
    DC: WindowTempData,
    OuterRectClipped: Rect,
    InnerRect: Rect,
    InnerClipRect: Rect,
    WorkRect: Rect,
    ParentWorkRect: Rect,
    ClipRect: Rect,
    ContentRegionRect: Rect,
    HitTestHoleSize: Vec2ih,
    HitTestHoleOffset: Vec2ih,
    LastFrameActive: i32,
    LastTimeActive: f32,
    ItemWidthDefault: f32,
    StateStorage: Storage,
    ColumnsStorage: Vector(OldColumns),
    FontWindowScale: f32,
    SettingsOffset: i32,
    DrawList: ?*DrawList,
    DrawListInst: DrawList,
    ParentWindow: ?*Window,
    ParentWindowInBeginStack: ?*Window,
    RootWindow: ?*Window,
    RootWindowPopupTree: ?*Window,
    RootWindowForTitleBarHighlight: ?*Window,
    RootWindowForNav: ?*Window,
    NavLastChildNavWindow: ?*Window,
    NavLastIds: [NavLayer.COUNT]ID,
    NavRectRel: [NavLayer.COUNT]Rect,
    MemoryDrawListIdxCapacity: i32,
    MemoryDrawListVtxCapacity: i32,
    MemoryCompacted: bool,

    /// CalcFontSize(self: *const Window) f32
    pub const CalcFontSize = raw.ImGuiWindow_CalcFontSize;

    /// GetID_StrExt(self: *Window, str: ?[*:0]const u8, str_end: ?[*]const u8) ID
    pub const GetID_StrExt = raw.ImGuiWindow_GetID_Str;
    pub inline fn GetID_Str(self: *Window, str: ?[*:0]const u8) ID {
        return @This().GetID_StrExt(self, str, null);
    }

    /// GetID_Ptr(self: *Window, ptr: ?*const anyopaque) ID
    pub const GetID_Ptr = raw.ImGuiWindow_GetID_Ptr;

    /// GetID_Int(self: *Window, n: i32) ID
    pub const GetID_Int = raw.ImGuiWindow_GetID_Int;

    pub inline fn GetIDFromRectangle(self: *Window, r_abs: Rect) ID {
        return raw.ImGuiWindow_GetIDFromRectangle(self, &r_abs);
    }

    /// init_ImGuiWindow(self: ?*anyopaque, context: ?*Context, name: ?[*:0]const u8) void
    pub const init_ImGuiWindow = raw.ImGuiWindow_ImGuiWindow;

    /// MenuBarHeight(self: *const Window) f32
    pub const MenuBarHeight = raw.ImGuiWindow_MenuBarHeight;

    pub inline fn MenuBarRect(self: *const Window) Rect {
        var out: Rect = undefined;
        raw.ImGuiWindow_MenuBarRect(&out, self);
        return out;
    }

    pub inline fn Rect(self: *const Window) Rect {
        var out: Rect = undefined;
        raw.ImGuiWindow_Rect(&out, self);
        return out;
    }

    /// TitleBarHeight(self: *const Window) f32
    pub const TitleBarHeight = raw.ImGuiWindow_TitleBarHeight;

    pub inline fn TitleBarRect(self: *const Window) Rect {
        var out: Rect = undefined;
        raw.ImGuiWindow_TitleBarRect(&out, self);
        return out;
    }

    /// deinit(self: *Window) void
    pub const deinit = raw.ImGuiWindow_destroy;
};

pub const WindowSettings = extern struct {
    ID: ID,
    Pos: Vec2ih,
    Size: Vec2ih,
    Collapsed: bool,
    WantApply: bool,

    /// GetName(self: *WindowSettings) ?[*:0]u8
    pub const GetName = raw.ImGuiWindowSettings_GetName;

    /// init_ImGuiWindowSettings(self: ?*anyopaque) void
    pub const init_ImGuiWindowSettings = raw.ImGuiWindowSettings_ImGuiWindowSettings;

    /// deinit(self: *WindowSettings) void
    pub const deinit = raw.ImGuiWindowSettings_destroy;
};

pub const WindowStackData = extern struct {
    Window: ?*Window,
    ParentLastItemDataBackup: LastItemData,
    StackSizesOnBegin: StackSizes,
};

pub const WindowTempData = extern struct {
    CursorPos: Vec2,
    CursorPosPrevLine: Vec2,
    CursorStartPos: Vec2,
    CursorMaxPos: Vec2,
    IdealMaxPos: Vec2,
    CurrLineSize: Vec2,
    PrevLineSize: Vec2,
    CurrLineTextBaseOffset: f32,
    PrevLineTextBaseOffset: f32,
    IsSameLine: bool,
    Indent: Vec1,
    ColumnsOffset: Vec1,
    GroupOffset: Vec1,
    CursorStartPosLossyness: Vec2,
    NavLayerCurrent: NavLayer,
    NavLayersActiveMask: i16,
    NavLayersActiveMaskNext: i16,
    NavFocusScopeIdCurrent: ID,
    NavHideHighlightOneFrame: bool,
    NavHasScroll: bool,
    MenuBarAppending: bool,
    MenuBarOffset: Vec2,
    MenuColumns: MenuColumns,
    TreeDepth: i32,
    TreeJumpToParentOnPopMask: u32,
    ChildWindows: Vector(?*Window),
    StateStorage: ?*Storage,
    CurrentColumns: [*c]OldColumns,
    CurrentTableIdx: i32,
    LayoutType: LayoutType,
    ParentLayoutType: LayoutType,
    ItemWidth: f32,
    TextWrapPos: f32,
    ItemWidthStack: Vector(f32),
    TextWrapPosStack: Vector(f32),
};

pub const Rect = extern struct {
    Min: Vec2,
    Max: Vec2,

    pub inline fn Add_Vec2(self: *Rect, p: Vec2) void {
        return raw.ImRect_Add_Vec2(self, &p);
    }

    pub inline fn Add_Rect(self: *Rect, r: Rect) void {
        return raw.ImRect_Add_Rect(self, &r);
    }

    pub inline fn ClipWith(self: *Rect, r: Rect) void {
        return raw.ImRect_ClipWith(self, &r);
    }

    pub inline fn ClipWithFull(self: *Rect, r: Rect) void {
        return raw.ImRect_ClipWithFull(self, &r);
    }

    pub inline fn Contains_Vec2(self: *const Rect, p: Vec2) bool {
        return raw.ImRect_Contains_Vec2(self, &p);
    }

    pub inline fn Contains_Rect(self: *const Rect, r: Rect) bool {
        return raw.ImRect_Contains_Rect(self, &r);
    }

    /// Expand_Float(self: *Rect, amount: const f32) void
    pub const Expand_Float = raw.ImRect_Expand_Float;

    pub inline fn Expand_Vec2(self: *Rect, amount: Vec2) void {
        return raw.ImRect_Expand_Vec2(self, &amount);
    }

    /// Floor(self: *Rect) void
    pub const Floor = raw.ImRect_Floor;

    /// GetArea(self: *const Rect) f32
    pub const GetArea = raw.ImRect_GetArea;

    pub inline fn GetBL(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetBL(&out, self);
        return out;
    }

    pub inline fn GetBR(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetBR(&out, self);
        return out;
    }

    pub inline fn GetCenter(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetCenter(&out, self);
        return out;
    }

    /// GetHeight(self: *const Rect) f32
    pub const GetHeight = raw.ImRect_GetHeight;

    pub inline fn GetSize(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetSize(&out, self);
        return out;
    }

    pub inline fn GetTL(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetTL(&out, self);
        return out;
    }

    pub inline fn GetTR(self: *const Rect) Vec2 {
        var out: Vec2 = undefined;
        raw.ImRect_GetTR(&out, self);
        return out;
    }

    /// GetWidth(self: *const Rect) f32
    pub const GetWidth = raw.ImRect_GetWidth;

    /// init_Nil(self: ?*anyopaque) void
    pub const init_Nil = raw.ImRect_ImRect_Nil;

    pub inline fn init_Vec2(self: ?*anyopaque, min: Vec2, max: Vec2) void {
        return raw.ImRect_ImRect_Vec2(self, &min, &max);
    }

    pub inline fn init_Vec4(self: ?*anyopaque, v: Vec4) void {
        return raw.ImRect_ImRect_Vec4(self, &v);
    }

    /// init_Float(self: ?*anyopaque, x1: f32, y1: f32, x2: f32, y2: f32) void
    pub const init_Float = raw.ImRect_ImRect_Float;

    /// IsInverted(self: *const Rect) bool
    pub const IsInverted = raw.ImRect_IsInverted;

    pub inline fn Overlaps(self: *const Rect, r: Rect) bool {
        return raw.ImRect_Overlaps(self, &r);
    }

    pub inline fn ToVec4(self: *const Rect) Vec4 {
        var out: Vec4 = undefined;
        raw.ImRect_ToVec4(&out, self);
        return out;
    }

    pub inline fn Translate(self: *Rect, d: Vec2) void {
        return raw.ImRect_Translate(self, &d);
    }

    /// TranslateX(self: *Rect, dx: f32) void
    pub const TranslateX = raw.ImRect_TranslateX;

    /// TranslateY(self: *Rect, dy: f32) void
    pub const TranslateY = raw.ImRect_TranslateY;

    /// deinit(self: *Rect) void
    pub const deinit = raw.ImRect_destroy;
};

pub const Vec1 = extern struct {
    x: f32,

    /// init_Nil(self: ?*anyopaque) void
    pub const init_Nil = raw.ImVec1_ImVec1_Nil;

    /// init_Float(self: ?*anyopaque, _x: f32) void
    pub const init_Float = raw.ImVec1_ImVec1_Float;

    /// deinit(self: *Vec1) void
    pub const deinit = raw.ImVec1_destroy;
};

pub const Vec2ih = extern struct {
    x: i16,
    y: i16,

    /// init_Nil(self: ?*anyopaque) void
    pub const init_Nil = raw.ImVec2ih_ImVec2ih_Nil;

    /// init_short(self: ?*anyopaque, _x: i16, _y: i16) void
    pub const init_short = raw.ImVec2ih_ImVec2ih_short;

    pub inline fn init_Vec2(self: ?*anyopaque, rhs: Vec2) void {
        return raw.ImVec2ih_ImVec2ih_Vec2(self, &rhs);
    }

    /// deinit(self: *Vec2ih) void
    pub const deinit = raw.ImVec2ih_destroy;
};

pub const STB_TexteditState = extern struct {
    cursor: i32,
    select_start: i32,
    select_end: i32,
    insert_mode: u8,
    row_count_per_page: i32,
    cursor_at_end_of_line: u8,
    initialized: u8,
    has_preferred_x: u8,
    single_line: u8,
    padding1: u8,
    padding2: u8,
    padding3: u8,
    preferred_x: f32,
    undostate: StbUndoState,
};

pub const StbTexteditRow = extern struct {
    x0: f32,
    x1: f32,
    baseline_y_delta: f32,
    ymin: f32,
    ymax: f32,
    num_chars: i32,
};

pub const StbUndoRecord = extern struct {
    where: i32,
    insert_length: i32,
    delete_length: i32,
    char_storage: i32,
};

pub const StbUndoState = extern struct {
    undo_rec: [99]StbUndoRecord,
    undo_char: [999]Wchar,
    undo_point: i16,
    redo_point: i16,
    undo_char_point: i32,
    redo_char_point: i32,
};


pub inline fn AcceptDragDropPayloadExt(kind: ?[*:0]const u8, flags: DragDropFlags) ?*const Payload {
    return raw.igAcceptDragDropPayload(kind, flags.toInt());
}
pub inline fn AcceptDragDropPayload(kind: ?[*:0]const u8) ?*const Payload {
    return @This().AcceptDragDropPayloadExt(kind, .{});
}

/// ActivateItem(id: ID) void
pub const ActivateItem = raw.igActivateItem;

/// AddContextHook(context: ?*Context, hook: ?*const ContextHook) ID
pub const AddContextHook = raw.igAddContextHook;

/// AddSettingsHandler(handler: ?*const SettingsHandler) void
pub const AddSettingsHandler = raw.igAddSettingsHandler;

/// AlignTextToFramePadding() void
pub const AlignTextToFramePadding = raw.igAlignTextToFramePadding;

/// ArrowButton(str_id: ?[*:0]const u8, dir: Dir) bool
pub const ArrowButton = raw.igArrowButton;

pub inline fn ArrowButtonExExt(str_id: ?[*:0]const u8, dir: Dir, size_arg: Vec2, flags: ButtonFlags) bool {
    return raw.igArrowButtonEx(str_id, dir, &size_arg, flags.toInt());
}
pub inline fn ArrowButtonEx(str_id: ?[*:0]const u8, dir: Dir, size_arg: Vec2) bool {
    return @This().ArrowButtonExExt(str_id, dir, size_arg, .{});
}

pub inline fn BeginExt(name: ?[*:0]const u8, p_open: ?*bool, flags: WindowFlags) bool {
    return raw.igBegin(name, p_open, flags.toInt());
}
pub inline fn Begin(name: ?[*:0]const u8) bool {
    return @This().BeginExt(name, null, .{});
}

pub inline fn BeginChild_StrExt(str_id: ?[*:0]const u8, size: Vec2, border: bool, flags: WindowFlags) bool {
    return raw.igBeginChild_Str(str_id, &size, border, flags.toInt());
}
pub inline fn BeginChild_Str(str_id: ?[*:0]const u8) bool {
    return @This().BeginChild_StrExt(str_id, .{.x=0,.y=0}, false, .{});
}

pub inline fn BeginChild_IDExt(id: ID, size: Vec2, border: bool, flags: WindowFlags) bool {
    return raw.igBeginChild_ID(id, &size, border, flags.toInt());
}
pub inline fn BeginChild_ID(id: ID) bool {
    return @This().BeginChild_IDExt(id, .{.x=0,.y=0}, false, .{});
}

pub inline fn BeginChildEx(name: ?[*:0]const u8, id: ID, size_arg: Vec2, border: bool, flags: WindowFlags) bool {
    return raw.igBeginChildEx(name, id, &size_arg, border, flags.toInt());
}

pub inline fn BeginChildFrameExt(id: ID, size: Vec2, flags: WindowFlags) bool {
    return raw.igBeginChildFrame(id, &size, flags.toInt());
}
pub inline fn BeginChildFrame(id: ID, size: Vec2) bool {
    return @This().BeginChildFrameExt(id, size, .{});
}

pub inline fn BeginColumnsExt(str_id: ?[*:0]const u8, count: i32, flags: OldColumnFlags) void {
    return raw.igBeginColumns(str_id, count, flags.toInt());
}
pub inline fn BeginColumns(str_id: ?[*:0]const u8, count: i32) void {
    return @This().BeginColumnsExt(str_id, count, .{});
}

pub inline fn BeginComboExt(label: ?[*:0]const u8, preview_value: ?[*:0]const u8, flags: ComboFlags) bool {
    return raw.igBeginCombo(label, preview_value, flags.toInt());
}
pub inline fn BeginCombo(label: ?[*:0]const u8, preview_value: ?[*:0]const u8) bool {
    return @This().BeginComboExt(label, preview_value, .{});
}

pub inline fn BeginComboPopup(popup_id: ID, bb: Rect, flags: ComboFlags) bool {
    return raw.igBeginComboPopup(popup_id, &bb, flags.toInt());
}

/// BeginComboPreview() bool
pub const BeginComboPreview = raw.igBeginComboPreview;

/// BeginDisabledExt(disabled: bool) void
pub const BeginDisabledExt = raw.igBeginDisabled;
pub inline fn BeginDisabled() void {
    return @This().BeginDisabledExt(true);
}

pub inline fn BeginDragDropSourceExt(flags: DragDropFlags) bool {
    return raw.igBeginDragDropSource(flags.toInt());
}
pub inline fn BeginDragDropSource() bool {
    return @This().BeginDragDropSourceExt(.{});
}

/// BeginDragDropTarget() bool
pub const BeginDragDropTarget = raw.igBeginDragDropTarget;

pub inline fn BeginDragDropTargetCustom(bb: Rect, id: ID) bool {
    return raw.igBeginDragDropTargetCustom(&bb, id);
}

/// BeginGroup() void
pub const BeginGroup = raw.igBeginGroup;

pub inline fn BeginListBoxExt(label: ?[*:0]const u8, size: Vec2) bool {
    return raw.igBeginListBox(label, &size);
}
pub inline fn BeginListBox(label: ?[*:0]const u8) bool {
    return @This().BeginListBoxExt(label, .{.x=0,.y=0});
}

/// BeginMainMenuBar() bool
pub const BeginMainMenuBar = raw.igBeginMainMenuBar;

/// BeginMenuExt(label: ?[*:0]const u8, enabled: bool) bool
pub const BeginMenuExt = raw.igBeginMenu;
pub inline fn BeginMenu(label: ?[*:0]const u8) bool {
    return @This().BeginMenuExt(label, true);
}

/// BeginMenuBar() bool
pub const BeginMenuBar = raw.igBeginMenuBar;

/// BeginMenuExExt(label: ?[*:0]const u8, icon: [*c]const u8, enabled: bool) bool
pub const BeginMenuExExt = raw.igBeginMenuEx;
pub inline fn BeginMenuEx(label: ?[*:0]const u8, icon: [*c]const u8) bool {
    return @This().BeginMenuExExt(label, icon, true);
}

pub inline fn BeginPopupExt(str_id: ?[*:0]const u8, flags: WindowFlags) bool {
    return raw.igBeginPopup(str_id, flags.toInt());
}
pub inline fn BeginPopup(str_id: ?[*:0]const u8) bool {
    return @This().BeginPopupExt(str_id, .{});
}

pub inline fn BeginPopupContextItemExt(str_id: ?[*:0]const u8, popup_flags: PopupFlags) bool {
    return raw.igBeginPopupContextItem(str_id, popup_flags.toInt());
}
pub inline fn BeginPopupContextItem() bool {
    return @This().BeginPopupContextItemExt(null, .{ .MouseButtonRight = true });
}

pub inline fn BeginPopupContextVoidExt(str_id: ?[*:0]const u8, popup_flags: PopupFlags) bool {
    return raw.igBeginPopupContextVoid(str_id, popup_flags.toInt());
}
pub inline fn BeginPopupContextVoid() bool {
    return @This().BeginPopupContextVoidExt(null, .{ .MouseButtonRight = true });
}

pub inline fn BeginPopupContextWindowExt(str_id: ?[*:0]const u8, popup_flags: PopupFlags) bool {
    return raw.igBeginPopupContextWindow(str_id, popup_flags.toInt());
}
pub inline fn BeginPopupContextWindow() bool {
    return @This().BeginPopupContextWindowExt(null, .{ .MouseButtonRight = true });
}

pub inline fn BeginPopupEx(id: ID, extra_flags: WindowFlags) bool {
    return raw.igBeginPopupEx(id, extra_flags.toInt());
}

pub inline fn BeginPopupModalExt(name: ?[*:0]const u8, p_open: ?*bool, flags: WindowFlags) bool {
    return raw.igBeginPopupModal(name, p_open, flags.toInt());
}
pub inline fn BeginPopupModal(name: ?[*:0]const u8) bool {
    return @This().BeginPopupModalExt(name, null, .{});
}

pub inline fn BeginTabBarExt(str_id: ?[*:0]const u8, flags: TabBarFlags) bool {
    return raw.igBeginTabBar(str_id, flags.toInt());
}
pub inline fn BeginTabBar(str_id: ?[*:0]const u8) bool {
    return @This().BeginTabBarExt(str_id, .{});
}

pub inline fn BeginTabBarEx(tab_bar: ?*TabBar, bb: Rect, flags: TabBarFlags) bool {
    return raw.igBeginTabBarEx(tab_bar, &bb, flags.toInt());
}

pub inline fn BeginTabItemExt(label: ?[*:0]const u8, p_open: ?*bool, flags: TabItemFlags) bool {
    return raw.igBeginTabItem(label, p_open, flags.toInt());
}
pub inline fn BeginTabItem(label: ?[*:0]const u8) bool {
    return @This().BeginTabItemExt(label, null, .{});
}

pub inline fn BeginTableExt(str_id: ?[*:0]const u8, column: i32, flags: TableFlags, outer_size: Vec2, inner_width: f32) bool {
    return raw.igBeginTable(str_id, column, flags.toInt(), &outer_size, inner_width);
}
pub inline fn BeginTable(str_id: ?[*:0]const u8, column: i32) bool {
    return @This().BeginTableExt(str_id, column, .{}, .{.x=0.0,.y=0.0}, 0.0);
}

pub inline fn BeginTableExExt(name: ?[*:0]const u8, id: ID, columns_count: i32, flags: TableFlags, outer_size: Vec2, inner_width: f32) bool {
    return raw.igBeginTableEx(name, id, columns_count, flags.toInt(), &outer_size, inner_width);
}
pub inline fn BeginTableEx(name: ?[*:0]const u8, id: ID, columns_count: i32) bool {
    return @This().BeginTableExExt(name, id, columns_count, .{}, .{.x=0,.y=0}, 0.0);
}

/// BeginTooltip() void
pub const BeginTooltip = raw.igBeginTooltip;

pub inline fn BeginTooltipEx(tooltip_flags: TooltipFlags, extra_window_flags: WindowFlags) void {
    return raw.igBeginTooltipEx(tooltip_flags.toInt(), extra_window_flags.toInt());
}

pub inline fn BeginViewportSideBar(name: ?[*:0]const u8, viewport: ?*Viewport, dir: Dir, size: f32, window_flags: WindowFlags) bool {
    return raw.igBeginViewportSideBar(name, viewport, dir, size, window_flags.toInt());
}

/// BringWindowToDisplayBack(window: ?*Window) void
pub const BringWindowToDisplayBack = raw.igBringWindowToDisplayBack;

/// BringWindowToDisplayBehind(window: ?*Window, above_window: ?*Window) void
pub const BringWindowToDisplayBehind = raw.igBringWindowToDisplayBehind;

/// BringWindowToDisplayFront(window: ?*Window) void
pub const BringWindowToDisplayFront = raw.igBringWindowToDisplayFront;

/// BringWindowToFocusFront(window: ?*Window) void
pub const BringWindowToFocusFront = raw.igBringWindowToFocusFront;

/// Bullet() void
pub const Bullet = raw.igBullet;

/// BulletText(fmt: ?[*:0]const u8, ...: ...) void
pub const BulletText = raw.igBulletText;

pub inline fn ButtonExt(label: ?[*:0]const u8, size: Vec2) bool {
    return raw.igButton(label, &size);
}
pub inline fn Button(label: ?[*:0]const u8) bool {
    return @This().ButtonExt(label, .{.x=0,.y=0});
}

pub inline fn ButtonBehaviorExt(bb: Rect, id: ID, out_hovered: *bool, out_held: *bool, flags: ButtonFlags) bool {
    return raw.igButtonBehavior(&bb, id, out_hovered, out_held, flags.toInt());
}
pub inline fn ButtonBehavior(bb: Rect, id: ID, out_hovered: *bool, out_held: *bool) bool {
    return @This().ButtonBehaviorExt(bb, id, out_hovered, out_held, .{});
}

pub inline fn ButtonExExt(label: ?[*:0]const u8, size_arg: Vec2, flags: ButtonFlags) bool {
    return raw.igButtonEx(label, &size_arg, flags.toInt());
}
pub inline fn ButtonEx(label: ?[*:0]const u8) bool {
    return @This().ButtonExExt(label, .{.x=0,.y=0}, .{});
}

pub inline fn CalcItemSize(size: ?*const Vec2, default_w: f32, default_h: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igCalcItemSize(&out, size, default_w, default_h);
    return out;
}

/// CalcItemWidth() f32
pub const CalcItemWidth = raw.igCalcItemWidth;

pub inline fn CalcTextSizeExt(text: ?[*]const u8, text_end: ?[*]const u8, hide_text_after_double_hash: bool, wrap_width: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igCalcTextSize(&out, text, text_end, hide_text_after_double_hash, wrap_width);
    return out;
}
pub inline fn CalcTextSize(text: ?[*]const u8) Vec2 {
    return @This().CalcTextSizeExt(text, null, false, -1.0);
}

/// CalcTypematicRepeatAmount(t0: f32, t1: f32, repeat_delay: f32, repeat_rate: f32) i32
pub const CalcTypematicRepeatAmount = raw.igCalcTypematicRepeatAmount;

pub inline fn CalcWindowNextAutoFitSize(window: ?*Window) Vec2 {
    var out: Vec2 = undefined;
    raw.igCalcWindowNextAutoFitSize(&out, window);
    return out;
}

pub inline fn CalcWrapWidthForPos(pos: Vec2, wrap_pos_x: f32) f32 {
    return raw.igCalcWrapWidthForPos(&pos, wrap_pos_x);
}

/// CallContextHooks(context: ?*Context, kind: ContextHookType) void
pub const CallContextHooks = raw.igCallContextHooks;

/// Checkbox(label: ?[*:0]const u8, v: *bool) bool
pub const Checkbox = raw.igCheckbox;

/// CheckboxFlags_IntPtr(label: ?[*:0]const u8, flags: *i32, flags_value: i32) bool
pub const CheckboxFlags_IntPtr = raw.igCheckboxFlags_IntPtr;

/// CheckboxFlags_UintPtr(label: ?[*:0]const u8, flags: *u32, flags_value: u32) bool
pub const CheckboxFlags_UintPtr = raw.igCheckboxFlags_UintPtr;

/// CheckboxFlags_S64Ptr(label: ?[*:0]const u8, flags: [*c]i64, flags_value: i64) bool
pub const CheckboxFlags_S64Ptr = raw.igCheckboxFlags_S64Ptr;

/// CheckboxFlags_U64Ptr(label: ?[*:0]const u8, flags: [*c]u64, flags_value: u64) bool
pub const CheckboxFlags_U64Ptr = raw.igCheckboxFlags_U64Ptr;

/// ClearActiveID() void
pub const ClearActiveID = raw.igClearActiveID;

/// ClearDragDrop() void
pub const ClearDragDrop = raw.igClearDragDrop;

/// ClearIniSettings() void
pub const ClearIniSettings = raw.igClearIniSettings;

pub inline fn CloseButton(id: ID, pos: Vec2) bool {
    return raw.igCloseButton(id, &pos);
}

/// CloseCurrentPopup() void
pub const CloseCurrentPopup = raw.igCloseCurrentPopup;

/// ClosePopupToLevel(remaining: i32, restore_focus_to_window_under_popup: bool) void
pub const ClosePopupToLevel = raw.igClosePopupToLevel;

/// ClosePopupsExceptModals() void
pub const ClosePopupsExceptModals = raw.igClosePopupsExceptModals;

/// ClosePopupsOverWindow(ref_window: ?*Window, restore_focus_to_window_under_popup: bool) void
pub const ClosePopupsOverWindow = raw.igClosePopupsOverWindow;

pub inline fn CollapseButton(id: ID, pos: Vec2) bool {
    return raw.igCollapseButton(id, &pos);
}

pub inline fn CollapsingHeader_TreeNodeFlagsExt(label: ?[*:0]const u8, flags: TreeNodeFlags) bool {
    return raw.igCollapsingHeader_TreeNodeFlags(label, flags.toInt());
}
pub inline fn CollapsingHeader_TreeNodeFlags(label: ?[*:0]const u8) bool {
    return @This().CollapsingHeader_TreeNodeFlagsExt(label, .{});
}

pub inline fn CollapsingHeader_BoolPtrExt(label: ?[*:0]const u8, p_visible: ?*bool, flags: TreeNodeFlags) bool {
    return raw.igCollapsingHeader_BoolPtr(label, p_visible, flags.toInt());
}
pub inline fn CollapsingHeader_BoolPtr(label: ?[*:0]const u8, p_visible: ?*bool) bool {
    return @This().CollapsingHeader_BoolPtrExt(label, p_visible, .{});
}

pub inline fn ColorButtonExt(desc_id: ?[*:0]const u8, col: Vec4, flags: ColorEditFlags, size: Vec2) bool {
    return raw.igColorButton(desc_id, &col, flags.toInt(), &size);
}
pub inline fn ColorButton(desc_id: ?[*:0]const u8, col: Vec4) bool {
    return @This().ColorButtonExt(desc_id, col, .{}, .{.x=0,.y=0});
}

pub inline fn ColorConvertFloat4ToU32(in: Vec4) u32 {
    return raw.igColorConvertFloat4ToU32(&in);
}

/// ColorConvertHSVtoRGB(h: f32, s: f32, v: f32, out_r: *f32, out_g: *f32, out_b: *f32) void
pub const ColorConvertHSVtoRGB = raw.igColorConvertHSVtoRGB;

/// ColorConvertRGBtoHSV(r: f32, g: f32, b: f32, out_h: *f32, out_s: *f32, out_v: *f32) void
pub const ColorConvertRGBtoHSV = raw.igColorConvertRGBtoHSV;

pub inline fn ColorConvertU32ToFloat4(in: u32) Vec4 {
    var out: Vec4 = undefined;
    raw.igColorConvertU32ToFloat4(&out, in);
    return out;
}

pub inline fn ColorEdit3Ext(label: ?[*:0]const u8, col: *[3]f32, flags: ColorEditFlags) bool {
    return raw.igColorEdit3(label, col, flags.toInt());
}
pub inline fn ColorEdit3(label: ?[*:0]const u8, col: *[3]f32) bool {
    return @This().ColorEdit3Ext(label, col, .{});
}

pub inline fn ColorEdit4Ext(label: ?[*:0]const u8, col: *[4]f32, flags: ColorEditFlags) bool {
    return raw.igColorEdit4(label, col, flags.toInt());
}
pub inline fn ColorEdit4(label: ?[*:0]const u8, col: *[4]f32) bool {
    return @This().ColorEdit4Ext(label, col, .{});
}

pub inline fn ColorEditOptionsPopup(col: [*c]const f32, flags: ColorEditFlags) void {
    return raw.igColorEditOptionsPopup(col, flags.toInt());
}

pub inline fn ColorPicker3Ext(label: ?[*:0]const u8, col: *[3]f32, flags: ColorEditFlags) bool {
    return raw.igColorPicker3(label, col, flags.toInt());
}
pub inline fn ColorPicker3(label: ?[*:0]const u8, col: *[3]f32) bool {
    return @This().ColorPicker3Ext(label, col, .{});
}

pub inline fn ColorPicker4Ext(label: ?[*:0]const u8, col: *[4]f32, flags: ColorEditFlags, ref_col: ?*const[4]f32) bool {
    return raw.igColorPicker4(label, col, flags.toInt(), ref_col);
}
pub inline fn ColorPicker4(label: ?[*:0]const u8, col: *[4]f32) bool {
    return @This().ColorPicker4Ext(label, col, .{}, null);
}

pub inline fn ColorPickerOptionsPopup(ref_col: [*c]const f32, flags: ColorEditFlags) void {
    return raw.igColorPickerOptionsPopup(ref_col, flags.toInt());
}

pub inline fn ColorTooltip(text: ?[*]const u8, col: [*c]const f32, flags: ColorEditFlags) void {
    return raw.igColorTooltip(text, col, flags.toInt());
}

/// ColumnsExt(count: i32, id: ?[*:0]const u8, border: bool) void
pub const ColumnsExt = raw.igColumns;
pub inline fn Columns() void {
    return @This().ColumnsExt(1, null, true);
}

/// Combo_Str_arrExt(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32, popup_max_height_in_items: i32) bool
pub const Combo_Str_arrExt = raw.igCombo_Str_arr;
pub inline fn Combo_Str_arr(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32) bool {
    return @This().Combo_Str_arrExt(label, current_item, items, items_count, -1);
}

/// Combo_StrExt(label: ?[*:0]const u8, current_item: ?*i32, items_separated_by_zeros: ?[*]const u8, popup_max_height_in_items: i32) bool
pub const Combo_StrExt = raw.igCombo_Str;
pub inline fn Combo_Str(label: ?[*:0]const u8, current_item: ?*i32, items_separated_by_zeros: ?[*]const u8) bool {
    return @This().Combo_StrExt(label, current_item, items_separated_by_zeros, -1);
}

/// Combo_FnBoolPtrExt(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32, popup_max_height_in_items: i32) bool
pub const Combo_FnBoolPtrExt = raw.igCombo_FnBoolPtr;
pub inline fn Combo_FnBoolPtr(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32) bool {
    return @This().Combo_FnBoolPtrExt(label, current_item, items_getter, data, items_count, -1);
}

/// CreateContextExt(shared_font_atlas: ?*FontAtlas) ?*Context
pub const CreateContextExt = raw.igCreateContext;
pub inline fn CreateContext() ?*Context {
    return @This().CreateContextExt(null);
}

/// CreateNewWindowSettings(name: ?[*:0]const u8) ?*WindowSettings
pub const CreateNewWindowSettings = raw.igCreateNewWindowSettings;

/// DataTypeApplyFromText(buf: ?[*]const u8, data_type: DataType, p_data: ?*anyopaque, format: ?[*:0]const u8) bool
pub const DataTypeApplyFromText = raw.igDataTypeApplyFromText;

/// DataTypeApplyOp(data_type: DataType, op: i32, output: ?*anyopaque, arg_1: ?*const anyopaque, arg_2: ?*const anyopaque) void
pub const DataTypeApplyOp = raw.igDataTypeApplyOp;

/// DataTypeClamp(data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque) bool
pub const DataTypeClamp = raw.igDataTypeClamp;

/// DataTypeCompare(data_type: DataType, arg_1: ?*const anyopaque, arg_2: ?*const anyopaque) i32
pub const DataTypeCompare = raw.igDataTypeCompare;

/// DataTypeFormatString(buf: ?[*]u8, buf_size: i32, data_type: DataType, p_data: ?*const anyopaque, format: ?[*:0]const u8) i32
pub const DataTypeFormatString = raw.igDataTypeFormatString;

/// DataTypeGetInfo(data_type: DataType) ?*const DataTypeInfo
pub const DataTypeGetInfo = raw.igDataTypeGetInfo;

/// DebugCheckVersionAndDataLayout(version_str: ?[*:0]const u8, sz_io: usize, sz_style: usize, sz_vec2: usize, sz_vec4: usize, sz_drawvert: usize, sz_drawidx: usize) bool
pub const DebugCheckVersionAndDataLayout = raw.igDebugCheckVersionAndDataLayout;

/// DebugDrawItemRectExt(col: u32) void
pub const DebugDrawItemRectExt = raw.igDebugDrawItemRect;
pub inline fn DebugDrawItemRect() void {
    return @This().DebugDrawItemRectExt(4278190335);
}

/// DebugHookIdInfo(id: ID, data_type: DataType, data_id: ?*const anyopaque, data_id_end: ?*const anyopaque) void
pub const DebugHookIdInfo = raw.igDebugHookIdInfo;

/// DebugLog(fmt: ?[*:0]const u8, ...: ...) void
pub const DebugLog = raw.igDebugLog;

/// DebugNodeColumns(columns: [*c]OldColumns) void
pub const DebugNodeColumns = raw.igDebugNodeColumns;

/// DebugNodeDrawCmdShowMeshAndBoundingBox(out_draw_list: ?*DrawList, draw_list: ?*const DrawList, draw_cmd: ?*const DrawCmd, show_mesh: bool, show_aabb: bool) void
pub const DebugNodeDrawCmdShowMeshAndBoundingBox = raw.igDebugNodeDrawCmdShowMeshAndBoundingBox;

/// DebugNodeDrawList(window: ?*Window, draw_list: ?*const DrawList, label: ?[*:0]const u8) void
pub const DebugNodeDrawList = raw.igDebugNodeDrawList;

/// DebugNodeFont(font: ?*Font) void
pub const DebugNodeFont = raw.igDebugNodeFont;

/// DebugNodeFontGlyph(font: ?*Font, glyph: ?*const FontGlyph) void
pub const DebugNodeFontGlyph = raw.igDebugNodeFontGlyph;

/// DebugNodeInputTextState(state: ?*InputTextState) void
pub const DebugNodeInputTextState = raw.igDebugNodeInputTextState;

/// DebugNodeStorage(storage: ?*Storage, label: ?[*:0]const u8) void
pub const DebugNodeStorage = raw.igDebugNodeStorage;

/// DebugNodeTabBar(tab_bar: ?*TabBar, label: ?[*:0]const u8) void
pub const DebugNodeTabBar = raw.igDebugNodeTabBar;

/// DebugNodeTable(table: ?*Table) void
pub const DebugNodeTable = raw.igDebugNodeTable;

/// DebugNodeTableSettings(settings: [*c]TableSettings) void
pub const DebugNodeTableSettings = raw.igDebugNodeTableSettings;

/// DebugNodeViewport(viewport: ?*ViewportP) void
pub const DebugNodeViewport = raw.igDebugNodeViewport;

/// DebugNodeWindow(window: ?*Window, label: ?[*:0]const u8) void
pub const DebugNodeWindow = raw.igDebugNodeWindow;

/// DebugNodeWindowSettings(settings: [*c]WindowSettings) void
pub const DebugNodeWindowSettings = raw.igDebugNodeWindowSettings;

/// DebugNodeWindowsList(windows: [*c]Vector(?*Window), label: ?[*:0]const u8) void
pub const DebugNodeWindowsList = raw.igDebugNodeWindowsList;

/// DebugNodeWindowsListByBeginStackParent(windows: [*c][*c]Window, windows_size: i32, parent_in_begin_stack: ?*Window) void
pub const DebugNodeWindowsListByBeginStackParent = raw.igDebugNodeWindowsListByBeginStackParent;

pub inline fn DebugRenderViewportThumbnail(draw_list: ?*DrawList, viewport: ?*ViewportP, bb: Rect) void {
    return raw.igDebugRenderViewportThumbnail(draw_list, viewport, &bb);
}

/// DebugStartItemPicker() void
pub const DebugStartItemPicker = raw.igDebugStartItemPicker;

/// DebugTextEncoding(text: ?[*]const u8) void
pub const DebugTextEncoding = raw.igDebugTextEncoding;

/// DestroyContextExt(ctx: ?*Context) void
pub const DestroyContextExt = raw.igDestroyContext;
pub inline fn DestroyContext() void {
    return @This().DestroyContextExt(null);
}

pub inline fn DragBehavior(id: ID, data_type: DataType, p_v: ?*anyopaque, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragBehavior(id, data_type, p_v, v_speed, p_min, p_max, format, flags.toInt());
}

pub inline fn DragFloatExt(label: ?[*:0]const u8, v: *f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragFloat(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragFloat(label: ?[*:0]const u8, v: *f32) bool {
    return @This().DragFloatExt(label, v, 1.0, 0.0, 0.0, "%.3f", .{});
}

pub inline fn DragFloat2Ext(label: ?[*:0]const u8, v: *[2]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragFloat2(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragFloat2(label: ?[*:0]const u8, v: *[2]f32) bool {
    return @This().DragFloat2Ext(label, v, 1.0, 0.0, 0.0, "%.3f", .{});
}

pub inline fn DragFloat3Ext(label: ?[*:0]const u8, v: *[3]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragFloat3(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragFloat3(label: ?[*:0]const u8, v: *[3]f32) bool {
    return @This().DragFloat3Ext(label, v, 1.0, 0.0, 0.0, "%.3f", .{});
}

pub inline fn DragFloat4Ext(label: ?[*:0]const u8, v: *[4]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragFloat4(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragFloat4(label: ?[*:0]const u8, v: *[4]f32) bool {
    return @This().DragFloat4Ext(label, v, 1.0, 0.0, 0.0, "%.3f", .{});
}

pub inline fn DragFloatRange2Ext(label: ?[*:0]const u8, v_current_min: *f32, v_current_max: *f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, format_max: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragFloatRange2(label, v_current_min, v_current_max, v_speed, v_min, v_max, format, format_max, flags.toInt());
}
pub inline fn DragFloatRange2(label: ?[*:0]const u8, v_current_min: *f32, v_current_max: *f32) bool {
    return @This().DragFloatRange2Ext(label, v_current_min, v_current_max, 1.0, 0.0, 0.0, "%.3f", null, .{});
}

pub inline fn DragIntExt(label: ?[*:0]const u8, v: *i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragInt(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragInt(label: ?[*:0]const u8, v: *i32) bool {
    return @This().DragIntExt(label, v, 1.0, 0, 0, "%d", .{});
}

pub inline fn DragInt2Ext(label: ?[*:0]const u8, v: *[2]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragInt2(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragInt2(label: ?[*:0]const u8, v: *[2]i32) bool {
    return @This().DragInt2Ext(label, v, 1.0, 0, 0, "%d", .{});
}

pub inline fn DragInt3Ext(label: ?[*:0]const u8, v: *[3]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragInt3(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragInt3(label: ?[*:0]const u8, v: *[3]i32) bool {
    return @This().DragInt3Ext(label, v, 1.0, 0, 0, "%d", .{});
}

pub inline fn DragInt4Ext(label: ?[*:0]const u8, v: *[4]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragInt4(label, v, v_speed, v_min, v_max, format, flags.toInt());
}
pub inline fn DragInt4(label: ?[*:0]const u8, v: *[4]i32) bool {
    return @This().DragInt4Ext(label, v, 1.0, 0, 0, "%d", .{});
}

pub inline fn DragIntRange2Ext(label: ?[*:0]const u8, v_current_min: *i32, v_current_max: *i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, format_max: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragIntRange2(label, v_current_min, v_current_max, v_speed, v_min, v_max, format, format_max, flags.toInt());
}
pub inline fn DragIntRange2(label: ?[*:0]const u8, v_current_min: *i32, v_current_max: *i32) bool {
    return @This().DragIntRange2Ext(label, v_current_min, v_current_max, 1.0, 0, 0, "%d", null, .{});
}

pub inline fn DragScalarExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragScalar(label, data_type, p_data, v_speed, p_min, p_max, format, flags.toInt());
}
pub inline fn DragScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque) bool {
    return @This().DragScalarExt(label, data_type, p_data, 1.0, null, null, null, .{});
}

pub inline fn DragScalarNExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igDragScalarN(label, data_type, p_data, components, v_speed, p_min, p_max, format, flags.toInt());
}
pub inline fn DragScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32) bool {
    return @This().DragScalarNExt(label, data_type, p_data, components, 1.0, null, null, null, .{});
}

pub inline fn Dummy(size: Vec2) void {
    return raw.igDummy(&size);
}

/// End() void
pub const End = raw.igEnd;

/// EndChild() void
pub const EndChild = raw.igEndChild;

/// EndChildFrame() void
pub const EndChildFrame = raw.igEndChildFrame;

/// EndColumns() void
pub const EndColumns = raw.igEndColumns;

/// EndCombo() void
pub const EndCombo = raw.igEndCombo;

/// EndComboPreview() void
pub const EndComboPreview = raw.igEndComboPreview;

/// EndDisabled() void
pub const EndDisabled = raw.igEndDisabled;

/// EndDragDropSource() void
pub const EndDragDropSource = raw.igEndDragDropSource;

/// EndDragDropTarget() void
pub const EndDragDropTarget = raw.igEndDragDropTarget;

/// EndFrame() void
pub const EndFrame = raw.igEndFrame;

/// EndGroup() void
pub const EndGroup = raw.igEndGroup;

/// EndListBox() void
pub const EndListBox = raw.igEndListBox;

/// EndMainMenuBar() void
pub const EndMainMenuBar = raw.igEndMainMenuBar;

/// EndMenu() void
pub const EndMenu = raw.igEndMenu;

/// EndMenuBar() void
pub const EndMenuBar = raw.igEndMenuBar;

/// EndPopup() void
pub const EndPopup = raw.igEndPopup;

/// EndTabBar() void
pub const EndTabBar = raw.igEndTabBar;

/// EndTabItem() void
pub const EndTabItem = raw.igEndTabItem;

/// EndTable() void
pub const EndTable = raw.igEndTable;

/// EndTooltip() void
pub const EndTooltip = raw.igEndTooltip;

/// ErrorCheckEndFrameRecoverExt(log_callback: ErrorLogCallback, user_data: ?*anyopaque) void
pub const ErrorCheckEndFrameRecoverExt = raw.igErrorCheckEndFrameRecover;
pub inline fn ErrorCheckEndFrameRecover(log_callback: ErrorLogCallback) void {
    return @This().ErrorCheckEndFrameRecoverExt(log_callback, null);
}

/// ErrorCheckEndWindowRecoverExt(log_callback: ErrorLogCallback, user_data: ?*anyopaque) void
pub const ErrorCheckEndWindowRecoverExt = raw.igErrorCheckEndWindowRecover;
pub inline fn ErrorCheckEndWindowRecover(log_callback: ErrorLogCallback) void {
    return @This().ErrorCheckEndWindowRecoverExt(log_callback, null);
}

pub inline fn FindBestWindowPosForPopup(window: ?*Window) Vec2 {
    var out: Vec2 = undefined;
    raw.igFindBestWindowPosForPopup(&out, window);
    return out;
}

pub inline fn FindBestWindowPosForPopupEx(ref_pos: [*c]const Vec2, size: ?*const Vec2, last_dir: ?*Dir, r_outer: ?*const Rect, r_avoid: ?*const Rect, policy: PopupPositionPolicy) Vec2 {
    var out: Vec2 = undefined;
    raw.igFindBestWindowPosForPopupEx(&out, ref_pos, size, last_dir, r_outer, r_avoid, policy);
    return out;
}

/// FindBottomMostVisibleWindowWithinBeginStack(window: ?*Window) ?*Window
pub const FindBottomMostVisibleWindowWithinBeginStack = raw.igFindBottomMostVisibleWindowWithinBeginStack;

/// FindOrCreateColumns(window: ?*Window, id: ID) ?*OldColumns
pub const FindOrCreateColumns = raw.igFindOrCreateColumns;

/// FindOrCreateWindowSettings(name: ?[*:0]const u8) ?*WindowSettings
pub const FindOrCreateWindowSettings = raw.igFindOrCreateWindowSettings;

/// FindRenderedTextEndExt(text: ?[*]const u8, text_end: ?[*]const u8) [*c]const u8
pub const FindRenderedTextEndExt = raw.igFindRenderedTextEnd;
pub inline fn FindRenderedTextEnd(text: ?[*]const u8) [*c]const u8 {
    return @This().FindRenderedTextEndExt(text, null);
}

/// FindSettingsHandler(type_name: ?[*:0]const u8) ?*SettingsHandler
pub const FindSettingsHandler = raw.igFindSettingsHandler;

/// FindWindowByID(id: ID) ?*Window
pub const FindWindowByID = raw.igFindWindowByID;

/// FindWindowByName(name: ?[*:0]const u8) ?*Window
pub const FindWindowByName = raw.igFindWindowByName;

/// FindWindowDisplayIndex(window: ?*Window) i32
pub const FindWindowDisplayIndex = raw.igFindWindowDisplayIndex;

/// FindWindowSettings(id: ID) ?*WindowSettings
pub const FindWindowSettings = raw.igFindWindowSettings;

/// FocusTopMostWindowUnderOne(under_this_window: ?*Window, ignore_window: ?*Window) void
pub const FocusTopMostWindowUnderOne = raw.igFocusTopMostWindowUnderOne;

/// FocusWindow(window: ?*Window) void
pub const FocusWindow = raw.igFocusWindow;

/// GcAwakeTransientWindowBuffers(window: ?*Window) void
pub const GcAwakeTransientWindowBuffers = raw.igGcAwakeTransientWindowBuffers;

/// GcCompactTransientMiscBuffers() void
pub const GcCompactTransientMiscBuffers = raw.igGcCompactTransientMiscBuffers;

/// GcCompactTransientWindowBuffers(window: ?*Window) void
pub const GcCompactTransientWindowBuffers = raw.igGcCompactTransientWindowBuffers;

/// GetActiveID() ID
pub const GetActiveID = raw.igGetActiveID;

/// GetAllocatorFunctions(p_alloc_func: ?*MemAllocFunc, p_free_func: ?*MemFreeFunc, p_user_data: ?*?*anyopaque) void
pub const GetAllocatorFunctions = raw.igGetAllocatorFunctions;

/// GetBackgroundDrawList_Nil() ?*DrawList
pub const GetBackgroundDrawList_Nil = raw.igGetBackgroundDrawList_Nil;

/// GetBackgroundDrawList_ViewportPtr(viewport: ?*Viewport) ?*DrawList
pub const GetBackgroundDrawList_ViewportPtr = raw.igGetBackgroundDrawList_ViewportPtr;

/// GetClipboardText() ?[*:0]const u8
pub const GetClipboardText = raw.igGetClipboardText;

/// GetColorU32_ColExt(idx: Col, alpha_mul: f32) u32
pub const GetColorU32_ColExt = raw.igGetColorU32_Col;
pub inline fn GetColorU32_Col(idx: Col) u32 {
    return @This().GetColorU32_ColExt(idx, 1.0);
}

pub inline fn GetColorU32_Vec4(col: Vec4) u32 {
    return raw.igGetColorU32_Vec4(&col);
}

/// GetColorU32_U32(col: u32) u32
pub const GetColorU32_U32 = raw.igGetColorU32_U32;

/// GetColumnIndex() i32
pub const GetColumnIndex = raw.igGetColumnIndex;

/// GetColumnNormFromOffset(columns: [*c]const OldColumns, offset: f32) f32
pub const GetColumnNormFromOffset = raw.igGetColumnNormFromOffset;

/// GetColumnOffsetExt(column_index: i32) f32
pub const GetColumnOffsetExt = raw.igGetColumnOffset;
pub inline fn GetColumnOffset() f32 {
    return @This().GetColumnOffsetExt(-1);
}

/// GetColumnOffsetFromNorm(columns: [*c]const OldColumns, offset_norm: f32) f32
pub const GetColumnOffsetFromNorm = raw.igGetColumnOffsetFromNorm;

/// GetColumnWidthExt(column_index: i32) f32
pub const GetColumnWidthExt = raw.igGetColumnWidth;
pub inline fn GetColumnWidth() f32 {
    return @This().GetColumnWidthExt(-1);
}

/// GetColumnsCount() i32
pub const GetColumnsCount = raw.igGetColumnsCount;

/// GetColumnsID(str_id: ?[*:0]const u8, count: i32) ID
pub const GetColumnsID = raw.igGetColumnsID;

pub inline fn GetContentRegionAvail() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetContentRegionAvail(&out);
    return out;
}

pub inline fn GetContentRegionMax() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetContentRegionMax(&out);
    return out;
}

pub inline fn GetContentRegionMaxAbs() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetContentRegionMaxAbs(&out);
    return out;
}

/// GetCurrentContext() ?*Context
pub const GetCurrentContext = raw.igGetCurrentContext;

/// GetCurrentTable() ?*Table
pub const GetCurrentTable = raw.igGetCurrentTable;

/// GetCurrentWindow() ?*Window
pub const GetCurrentWindow = raw.igGetCurrentWindow;

/// GetCurrentWindowRead() ?*Window
pub const GetCurrentWindowRead = raw.igGetCurrentWindowRead;

pub inline fn GetCursorPos() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetCursorPos(&out);
    return out;
}

/// GetCursorPosX() f32
pub const GetCursorPosX = raw.igGetCursorPosX;

/// GetCursorPosY() f32
pub const GetCursorPosY = raw.igGetCursorPosY;

pub inline fn GetCursorScreenPos() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetCursorScreenPos(&out);
    return out;
}

pub inline fn GetCursorStartPos() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetCursorStartPos(&out);
    return out;
}

/// GetDefaultFont() ?*Font
pub const GetDefaultFont = raw.igGetDefaultFont;

/// GetDragDropPayload() ?*const Payload
pub const GetDragDropPayload = raw.igGetDragDropPayload;

/// GetDrawData() *DrawData
pub const GetDrawData = raw.igGetDrawData;

/// GetDrawListSharedData() ?*DrawListSharedData
pub const GetDrawListSharedData = raw.igGetDrawListSharedData;

/// GetFocusID() ID
pub const GetFocusID = raw.igGetFocusID;

/// GetFocusScope() ID
pub const GetFocusScope = raw.igGetFocusScope;

/// GetFocusedFocusScope() ID
pub const GetFocusedFocusScope = raw.igGetFocusedFocusScope;

/// GetFont() ?*Font
pub const GetFont = raw.igGetFont;

/// GetFontSize() f32
pub const GetFontSize = raw.igGetFontSize;

pub inline fn GetFontTexUvWhitePixel() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetFontTexUvWhitePixel(&out);
    return out;
}

/// GetForegroundDrawList_Nil() ?*DrawList
pub const GetForegroundDrawList_Nil = raw.igGetForegroundDrawList_Nil;

/// GetForegroundDrawList_WindowPtr(window: ?*Window) ?*DrawList
pub const GetForegroundDrawList_WindowPtr = raw.igGetForegroundDrawList_WindowPtr;

/// GetForegroundDrawList_ViewportPtr(viewport: ?*Viewport) ?*DrawList
pub const GetForegroundDrawList_ViewportPtr = raw.igGetForegroundDrawList_ViewportPtr;

/// GetFrameCount() i32
pub const GetFrameCount = raw.igGetFrameCount;

/// GetFrameHeight() f32
pub const GetFrameHeight = raw.igGetFrameHeight;

/// GetFrameHeightWithSpacing() f32
pub const GetFrameHeightWithSpacing = raw.igGetFrameHeightWithSpacing;

/// GetHoveredID() ID
pub const GetHoveredID = raw.igGetHoveredID;

/// GetID_Str(str_id: ?[*:0]const u8) ID
pub const GetID_Str = raw.igGetID_Str;

/// GetID_StrStr(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8) ID
pub const GetID_StrStr = raw.igGetID_StrStr;

/// GetID_Ptr(ptr_id: ?*const anyopaque) ID
pub const GetID_Ptr = raw.igGetID_Ptr;

/// GetIDWithSeed(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8, seed: ID) ID
pub const GetIDWithSeed = raw.igGetIDWithSeed;

/// GetIO() *IO
pub const GetIO = raw.igGetIO;

/// GetInputTextState(id: ID) ?*InputTextState
pub const GetInputTextState = raw.igGetInputTextState;

pub inline fn GetItemFlags() ItemFlags {
    const _retflags = raw.igGetItemFlags();
    return ItemFlags.fromInt(_retflags);
}

/// GetItemID() ID
pub const GetItemID = raw.igGetItemID;

pub inline fn GetItemRectMax() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetItemRectMax(&out);
    return out;
}

pub inline fn GetItemRectMin() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetItemRectMin(&out);
    return out;
}

pub inline fn GetItemRectSize() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetItemRectSize(&out);
    return out;
}

pub inline fn GetItemStatusFlags() ItemStatusFlags {
    const _retflags = raw.igGetItemStatusFlags();
    return ItemStatusFlags.fromInt(_retflags);
}

/// GetKeyData(key: Key) ?*KeyData
pub const GetKeyData = raw.igGetKeyData;

/// GetKeyIndex(key: Key) i32
pub const GetKeyIndex = raw.igGetKeyIndex;

/// GetKeyName(key: Key) ?[*:0]const u8
pub const GetKeyName = raw.igGetKeyName;

/// GetKeyPressedAmount(key: Key, repeat_delay: f32, rate: f32) i32
pub const GetKeyPressedAmount = raw.igGetKeyPressedAmount;

/// GetMainViewport() ?*Viewport
pub const GetMainViewport = raw.igGetMainViewport;

pub inline fn GetMergedModFlags() ModFlags {
    const _retflags = raw.igGetMergedModFlags();
    return ModFlags.fromInt(_retflags);
}

/// GetMouseClickedCount(button: MouseButton) i32
pub const GetMouseClickedCount = raw.igGetMouseClickedCount;

/// GetMouseCursor() MouseCursor
pub const GetMouseCursor = raw.igGetMouseCursor;

pub inline fn GetMouseDragDeltaExt(button: MouseButton, lock_threshold: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igGetMouseDragDelta(&out, button, lock_threshold);
    return out;
}
pub inline fn GetMouseDragDelta() Vec2 {
    return @This().GetMouseDragDeltaExt(.Left, -1.0);
}

pub inline fn GetMousePos() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetMousePos(&out);
    return out;
}

pub inline fn GetMousePosOnOpeningCurrentPopup() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetMousePosOnOpeningCurrentPopup(&out);
    return out;
}

/// GetNavInputAmount(n: NavInput, mode: NavReadMode) f32
pub const GetNavInputAmount = raw.igGetNavInputAmount;

pub inline fn GetNavInputAmount2dExt(dir_sources: NavDirSourceFlags, mode: NavReadMode, slow_factor: f32, fast_factor: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igGetNavInputAmount2d(&out, dir_sources.toInt(), mode, slow_factor, fast_factor);
    return out;
}
pub inline fn GetNavInputAmount2d(dir_sources: NavDirSourceFlags, mode: NavReadMode) Vec2 {
    return @This().GetNavInputAmount2dExt(dir_sources, mode, 0.0, 0.0);
}

/// GetNavInputName(n: NavInput) ?[*:0]const u8
pub const GetNavInputName = raw.igGetNavInputName;

pub inline fn GetPopupAllowedExtentRect(window: ?*Window) Rect {
    var out: Rect = undefined;
    raw.igGetPopupAllowedExtentRect(&out, window);
    return out;
}

/// GetScrollMaxX() f32
pub const GetScrollMaxX = raw.igGetScrollMaxX;

/// GetScrollMaxY() f32
pub const GetScrollMaxY = raw.igGetScrollMaxY;

/// GetScrollX() f32
pub const GetScrollX = raw.igGetScrollX;

/// GetScrollY() f32
pub const GetScrollY = raw.igGetScrollY;

/// GetStateStorage() ?*Storage
pub const GetStateStorage = raw.igGetStateStorage;

/// GetStyle() ?*Style
pub const GetStyle = raw.igGetStyle;

/// GetStyleColorName(idx: Col) ?[*:0]const u8
pub const GetStyleColorName = raw.igGetStyleColorName;

/// GetStyleColorVec4(idx: Col) ?*const Vec4
pub const GetStyleColorVec4 = raw.igGetStyleColorVec4;

/// GetTextLineHeight() f32
pub const GetTextLineHeight = raw.igGetTextLineHeight;

/// GetTextLineHeightWithSpacing() f32
pub const GetTextLineHeightWithSpacing = raw.igGetTextLineHeightWithSpacing;

/// GetTime() f64
pub const GetTime = raw.igGetTime;

/// GetTopMostAndVisiblePopupModal() ?*Window
pub const GetTopMostAndVisiblePopupModal = raw.igGetTopMostAndVisiblePopupModal;

/// GetTopMostPopupModal() ?*Window
pub const GetTopMostPopupModal = raw.igGetTopMostPopupModal;

/// GetTreeNodeToLabelSpacing() f32
pub const GetTreeNodeToLabelSpacing = raw.igGetTreeNodeToLabelSpacing;

/// GetVersion() ?[*:0]const u8
pub const GetVersion = raw.igGetVersion;

pub inline fn GetWindowContentRegionMax() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetWindowContentRegionMax(&out);
    return out;
}

pub inline fn GetWindowContentRegionMin() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetWindowContentRegionMin(&out);
    return out;
}

/// GetWindowDrawList() ?*DrawList
pub const GetWindowDrawList = raw.igGetWindowDrawList;

/// GetWindowHeight() f32
pub const GetWindowHeight = raw.igGetWindowHeight;

pub inline fn GetWindowPos() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetWindowPos(&out);
    return out;
}

/// GetWindowResizeBorderID(window: ?*Window, dir: Dir) ID
pub const GetWindowResizeBorderID = raw.igGetWindowResizeBorderID;

/// GetWindowResizeCornerID(window: ?*Window, n: i32) ID
pub const GetWindowResizeCornerID = raw.igGetWindowResizeCornerID;

/// GetWindowScrollbarID(window: ?*Window, axis: Axis) ID
pub const GetWindowScrollbarID = raw.igGetWindowScrollbarID;

pub inline fn GetWindowScrollbarRect(window: ?*Window, axis: Axis) Rect {
    var out: Rect = undefined;
    raw.igGetWindowScrollbarRect(&out, window, axis);
    return out;
}

pub inline fn GetWindowSize() Vec2 {
    var out: Vec2 = undefined;
    raw.igGetWindowSize(&out);
    return out;
}

/// GetWindowWidth() f32
pub const GetWindowWidth = raw.igGetWindowWidth;

/// ImAbs_Int(x: i32) i32
pub const ImAbs_Int = raw.igImAbs_Int;

/// ImAbs_Float(x: f32) f32
pub const ImAbs_Float = raw.igImAbs_Float;

/// ImAbs_double(x: f64) f64
pub const ImAbs_double = raw.igImAbs_double;

/// ImAlphaBlendColors(col_a: u32, col_b: u32) u32
pub const ImAlphaBlendColors = raw.igImAlphaBlendColors;

pub inline fn ImBezierCubicCalc(p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, t: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImBezierCubicCalc(&out, p1, p2, p3, p4, t);
    return out;
}

pub inline fn ImBezierCubicClosestPoint(p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, p: ?*const Vec2, num_segments: i32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImBezierCubicClosestPoint(&out, p1, p2, p3, p4, p, num_segments);
    return out;
}

pub inline fn ImBezierCubicClosestPointCasteljau(p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, p: ?*const Vec2, tess_tol: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImBezierCubicClosestPointCasteljau(&out, p1, p2, p3, p4, p, tess_tol);
    return out;
}

pub inline fn ImBezierQuadraticCalc(p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, t: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImBezierQuadraticCalc(&out, p1, p2, p3, t);
    return out;
}

/// ImBitArrayClearBit(arr: ?*u32, n: i32) void
pub const ImBitArrayClearBit = raw.igImBitArrayClearBit;

/// ImBitArraySetBit(arr: ?*u32, n: i32) void
pub const ImBitArraySetBit = raw.igImBitArraySetBit;

/// ImBitArraySetBitRange(arr: ?*u32, n: i32, n2: i32) void
pub const ImBitArraySetBitRange = raw.igImBitArraySetBitRange;

/// ImBitArrayTestBit(arr: ?*const u32, n: i32) bool
pub const ImBitArrayTestBit = raw.igImBitArrayTestBit;

/// ImCharIsBlankA(c: u8) bool
pub const ImCharIsBlankA = raw.igImCharIsBlankA;

/// ImCharIsBlankW(c: u32) bool
pub const ImCharIsBlankW = raw.igImCharIsBlankW;

pub inline fn ImClamp(v: ?*const Vec2, mn: ?*const Vec2, mx: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImClamp(&out, v, mn, mx);
    return out;
}

pub inline fn ImDot(a: Vec2, b: Vec2) f32 {
    return raw.igImDot(&a, &b);
}

/// ImFileClose(file: FileHandle) bool
pub const ImFileClose = raw.igImFileClose;

/// ImFileGetSize(file: FileHandle) u64
pub const ImFileGetSize = raw.igImFileGetSize;

/// ImFileLoadToMemoryExt(filename: ?[*:0]const u8, mode: [*c]const u8, out_file_size: *usize, padding_bytes: i32) ?*anyopaque
pub const ImFileLoadToMemoryExt = raw.igImFileLoadToMemory;
pub inline fn ImFileLoadToMemory(filename: ?[*:0]const u8, mode: [*c]const u8) ?*anyopaque {
    return @This().ImFileLoadToMemoryExt(filename, mode, NULL, 0);
}

/// ImFileOpen(filename: ?[*:0]const u8, mode: [*c]const u8) FileHandle
pub const ImFileOpen = raw.igImFileOpen;

/// ImFileRead(data: ?*anyopaque, size: u64, count: u64, file: FileHandle) u64
pub const ImFileRead = raw.igImFileRead;

/// ImFileWrite(data: ?*const anyopaque, size: u64, count: u64, file: FileHandle) u64
pub const ImFileWrite = raw.igImFileWrite;

/// ImFloor_Float(f: f32) f32
pub const ImFloor_Float = raw.igImFloor_Float;

pub inline fn ImFloor_Vec2(v: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImFloor_Vec2(&out, v);
    return out;
}

/// ImFloorSigned_Float(f: f32) f32
pub const ImFloorSigned_Float = raw.igImFloorSigned_Float;

pub inline fn ImFloorSigned_Vec2(v: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImFloorSigned_Vec2(&out, v);
    return out;
}

/// ImFontAtlasBuildFinish(atlas: ?*FontAtlas) void
pub const ImFontAtlasBuildFinish = raw.igImFontAtlasBuildFinish;

/// ImFontAtlasBuildInit(atlas: ?*FontAtlas) void
pub const ImFontAtlasBuildInit = raw.igImFontAtlasBuildInit;

/// ImFontAtlasBuildMultiplyCalcLookupTable(out_table: *[256]u8, in_multiply_factor: f32) void
pub const ImFontAtlasBuildMultiplyCalcLookupTable = raw.igImFontAtlasBuildMultiplyCalcLookupTable;

/// ImFontAtlasBuildMultiplyRectAlpha8(table: *const[256]u8, pixels: [*c]u8, x: i32, y: i32, w: i32, h: i32, stride: i32) void
pub const ImFontAtlasBuildMultiplyRectAlpha8 = raw.igImFontAtlasBuildMultiplyRectAlpha8;

/// ImFontAtlasBuildPackCustomRects(atlas: ?*FontAtlas, stbrp_context_opaque: ?*anyopaque) void
pub const ImFontAtlasBuildPackCustomRects = raw.igImFontAtlasBuildPackCustomRects;

/// ImFontAtlasBuildRender32bppRectFromString(atlas: ?*FontAtlas, x: i32, y: i32, w: i32, h: i32, in_str: ?[*:0]const u8, in_marker_char: u8, in_marker_pixel_value: u32) void
pub const ImFontAtlasBuildRender32bppRectFromString = raw.igImFontAtlasBuildRender32bppRectFromString;

/// ImFontAtlasBuildRender8bppRectFromString(atlas: ?*FontAtlas, x: i32, y: i32, w: i32, h: i32, in_str: ?[*:0]const u8, in_marker_char: u8, in_marker_pixel_value: u8) void
pub const ImFontAtlasBuildRender8bppRectFromString = raw.igImFontAtlasBuildRender8bppRectFromString;

/// ImFontAtlasBuildSetupFont(atlas: ?*FontAtlas, font: ?*Font, font_config: ?*FontConfig, ascent: f32, descent: f32) void
pub const ImFontAtlasBuildSetupFont = raw.igImFontAtlasBuildSetupFont;

/// ImFontAtlasGetBuilderForStbTruetype() ?*const FontBuilderIO
pub const ImFontAtlasGetBuilderForStbTruetype = raw.igImFontAtlasGetBuilderForStbTruetype;

/// ImFormatString(buf: ?[*]u8, buf_size: usize, fmt: ?[*:0]const u8, ...: ...) i32
pub const ImFormatString = raw.igImFormatString;

/// ImFormatStringToTempBuffer(out_buf: [*c][*c]const u8, out_buf_end: [*c][*c]const u8, fmt: ?[*:0]const u8, ...: ...) void
pub const ImFormatStringToTempBuffer = raw.igImFormatStringToTempBuffer;

/// ImGetDirQuadrantFromDelta(dx: f32, dy: f32) Dir
pub const ImGetDirQuadrantFromDelta = raw.igImGetDirQuadrantFromDelta;

/// ImHashDataExt(data: ?*const anyopaque, data_size: usize, seed: u32) ID
pub const ImHashDataExt = raw.igImHashData;
pub inline fn ImHashData(data: ?*const anyopaque, data_size: usize) ID {
    return @This().ImHashDataExt(data, data_size, 0);
}

/// ImHashStrExt(data: ?[*]const u8, data_size: usize, seed: u32) ID
pub const ImHashStrExt = raw.igImHashStr;
pub inline fn ImHashStr(data: ?[*]const u8) ID {
    return @This().ImHashStrExt(data, 0, 0);
}

pub inline fn ImInvLength(lhs: Vec2, fail_value: f32) f32 {
    return raw.igImInvLength(&lhs, fail_value);
}

/// ImIsFloatAboveGuaranteedIntegerPrecision(f: f32) bool
pub const ImIsFloatAboveGuaranteedIntegerPrecision = raw.igImIsFloatAboveGuaranteedIntegerPrecision;

/// ImIsPowerOfTwo_Int(v: i32) bool
pub const ImIsPowerOfTwo_Int = raw.igImIsPowerOfTwo_Int;

/// ImIsPowerOfTwo_U64(v: u64) bool
pub const ImIsPowerOfTwo_U64 = raw.igImIsPowerOfTwo_U64;

pub inline fn ImLengthSqr_Vec2(lhs: Vec2) f32 {
    return raw.igImLengthSqr_Vec2(&lhs);
}

pub inline fn ImLengthSqr_Vec4(lhs: Vec4) f32 {
    return raw.igImLengthSqr_Vec4(&lhs);
}

pub inline fn ImLerp_Vec2Float(a: ?*const Vec2, b: ?*const Vec2, t: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImLerp_Vec2Float(&out, a, b, t);
    return out;
}

pub inline fn ImLerp_Vec2Vec2(a: ?*const Vec2, b: ?*const Vec2, t: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImLerp_Vec2Vec2(&out, a, b, t);
    return out;
}

pub inline fn ImLerp_Vec4(a: ?*const Vec4, b: ?*const Vec4, t: f32) Vec4 {
    var out: Vec4 = undefined;
    raw.igImLerp_Vec4(&out, a, b, t);
    return out;
}

pub inline fn ImLineClosestPoint(a: ?*const Vec2, b: ?*const Vec2, p: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImLineClosestPoint(&out, a, b, p);
    return out;
}

/// ImLinearSweep(current: f32, target: f32, speed: f32) f32
pub const ImLinearSweep = raw.igImLinearSweep;

/// ImLog_Float(x: f32) f32
pub const ImLog_Float = raw.igImLog_Float;

/// ImLog_double(x: f64) f64
pub const ImLog_double = raw.igImLog_double;

pub inline fn ImMax(lhs: [*c]const Vec2, rhs: [*c]const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImMax(&out, lhs, rhs);
    return out;
}

pub inline fn ImMin(lhs: [*c]const Vec2, rhs: [*c]const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImMin(&out, lhs, rhs);
    return out;
}

/// ImModPositive(a: i32, b: i32) i32
pub const ImModPositive = raw.igImModPositive;

pub inline fn ImMul(lhs: [*c]const Vec2, rhs: [*c]const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImMul(&out, lhs, rhs);
    return out;
}

/// ImParseFormatFindEnd(format: ?[*:0]const u8) [*c]const u8
pub const ImParseFormatFindEnd = raw.igImParseFormatFindEnd;

/// ImParseFormatFindStart(format: ?[*:0]const u8) [*c]const u8
pub const ImParseFormatFindStart = raw.igImParseFormatFindStart;

/// ImParseFormatPrecision(format: ?[*:0]const u8, default_value: i32) i32
pub const ImParseFormatPrecision = raw.igImParseFormatPrecision;

/// ImParseFormatSanitizeForPrinting(fmt_in: [*c]const u8, fmt_out: [*c]u8, fmt_out_size: usize) void
pub const ImParseFormatSanitizeForPrinting = raw.igImParseFormatSanitizeForPrinting;

/// ImParseFormatSanitizeForScanning(fmt_in: [*c]const u8, fmt_out: [*c]u8, fmt_out_size: usize) [*c]const u8
pub const ImParseFormatSanitizeForScanning = raw.igImParseFormatSanitizeForScanning;

/// ImParseFormatTrimDecorations(format: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize) [*c]const u8
pub const ImParseFormatTrimDecorations = raw.igImParseFormatTrimDecorations;

/// ImPow_Float(x: f32, y: f32) f32
pub const ImPow_Float = raw.igImPow_Float;

/// ImPow_double(x: f64, y: f64) f64
pub const ImPow_double = raw.igImPow_double;

/// ImQsort(base: ?*anyopaque, count: usize, size_of_element: usize, compare_func: ?fn (const*: void, const*: void) callconv(.C) i32) void
pub const ImQsort = raw.igImQsort;

pub inline fn ImRotate(v: ?*const Vec2, cos_a: f32, sin_a: f32) Vec2 {
    var out: Vec2 = undefined;
    raw.igImRotate(&out, v, cos_a, sin_a);
    return out;
}

/// ImRsqrt_Float(x: f32) f32
pub const ImRsqrt_Float = raw.igImRsqrt_Float;

/// ImRsqrt_double(x: f64) f64
pub const ImRsqrt_double = raw.igImRsqrt_double;

/// ImSaturate(f: f32) f32
pub const ImSaturate = raw.igImSaturate;

/// ImSign_Float(x: f32) f32
pub const ImSign_Float = raw.igImSign_Float;

/// ImSign_double(x: f64) f64
pub const ImSign_double = raw.igImSign_double;

/// ImStrSkipBlank(str: ?[*:0]const u8) [*c]const u8
pub const ImStrSkipBlank = raw.igImStrSkipBlank;

/// ImStrTrimBlanks(str: ?[*:0]u8) void
pub const ImStrTrimBlanks = raw.igImStrTrimBlanks;

/// ImStrbolW(buf_mid_line: ?*const Wchar, buf_begin: ?*const Wchar) ?*const Wchar
pub const ImStrbolW = raw.igImStrbolW;

/// ImStrchrRange(str_begin: ?[*]const u8, str_end: ?[*]const u8, c: u8) [*c]const u8
pub const ImStrchrRange = raw.igImStrchrRange;

/// ImStrdup(str: ?[*:0]const u8) [*c]u8
pub const ImStrdup = raw.igImStrdup;

/// ImStrdupcpy(dst: [*c]u8, p_dst_size: ?*usize, str: ?[*:0]const u8) [*c]u8
pub const ImStrdupcpy = raw.igImStrdupcpy;

/// ImStreolRange(str: ?[*:0]const u8, str_end: ?[*]const u8) [*c]const u8
pub const ImStreolRange = raw.igImStreolRange;

/// ImStricmp(str1: [*c]const u8, str2: [*c]const u8) i32
pub const ImStricmp = raw.igImStricmp;

/// ImStristr(haystack: [*c]const u8, haystack_end: ?[*]const u8, needle: [*c]const u8, needle_end: ?[*]const u8) [*c]const u8
pub const ImStristr = raw.igImStristr;

/// ImStrlenW(str: ?*const Wchar) i32
pub const ImStrlenW = raw.igImStrlenW;

/// ImStrncpy(dst: [*c]u8, src: [*c]const u8, count: usize) void
pub const ImStrncpy = raw.igImStrncpy;

/// ImStrnicmp(str1: [*c]const u8, str2: [*c]const u8, count: usize) i32
pub const ImStrnicmp = raw.igImStrnicmp;

/// ImTextCharFromUtf8(out_char: *u32, in_text: [*c]const u8, in_text_end: ?[*]const u8) i32
pub const ImTextCharFromUtf8 = raw.igImTextCharFromUtf8;

/// ImTextCharToUtf8(out_buf: *[5]u8, c: u32) [*c]const u8
pub const ImTextCharToUtf8 = raw.igImTextCharToUtf8;

/// ImTextCountCharsFromUtf8(in_text: [*c]const u8, in_text_end: ?[*]const u8) i32
pub const ImTextCountCharsFromUtf8 = raw.igImTextCountCharsFromUtf8;

/// ImTextCountUtf8BytesFromChar(in_text: [*c]const u8, in_text_end: ?[*]const u8) i32
pub const ImTextCountUtf8BytesFromChar = raw.igImTextCountUtf8BytesFromChar;

/// ImTextCountUtf8BytesFromStr(in_text: ?*const Wchar, in_text_end: ?*const Wchar) i32
pub const ImTextCountUtf8BytesFromStr = raw.igImTextCountUtf8BytesFromStr;

/// ImTextStrFromUtf8Ext(out_buf: ?*Wchar, out_buf_size: i32, in_text: [*c]const u8, in_text_end: ?[*]const u8, in_remaining: [*c][*c]const u8) i32
pub const ImTextStrFromUtf8Ext = raw.igImTextStrFromUtf8;
pub inline fn ImTextStrFromUtf8(out_buf: ?*Wchar, out_buf_size: i32, in_text: [*c]const u8, in_text_end: ?[*]const u8) i32 {
    return @This().ImTextStrFromUtf8Ext(out_buf, out_buf_size, in_text, in_text_end, null);
}

/// ImTextStrToUtf8(out_buf: *u8, out_buf_size: i32, in_text: ?*const Wchar, in_text_end: ?*const Wchar) i32
pub const ImTextStrToUtf8 = raw.igImTextStrToUtf8;

pub inline fn ImTriangleArea(a: Vec2, b: Vec2, c: Vec2) f32 {
    return raw.igImTriangleArea(&a, &b, &c);
}

pub inline fn ImTriangleBarycentricCoords(a: Vec2, b: Vec2, c: Vec2, p: Vec2, out_u: *f32, out_v: *f32, out_w: *f32) void {
    return raw.igImTriangleBarycentricCoords(&a, &b, &c, &p, out_u, out_v, out_w);
}

pub inline fn ImTriangleClosestPoint(a: ?*const Vec2, b: ?*const Vec2, c: ?*const Vec2, p: ?*const Vec2) Vec2 {
    var out: Vec2 = undefined;
    raw.igImTriangleClosestPoint(&out, a, b, c, p);
    return out;
}

pub inline fn ImTriangleContainsPoint(a: Vec2, b: Vec2, c: Vec2, p: Vec2) bool {
    return raw.igImTriangleContainsPoint(&a, &b, &c, &p);
}

/// ImUpperPowerOfTwo(v: i32) i32
pub const ImUpperPowerOfTwo = raw.igImUpperPowerOfTwo;

pub inline fn ImageExt(user_texture_id: TextureID, size: Vec2, uv0: Vec2, uv1: Vec2, tint_col: Vec4, border_col: Vec4) void {
    return raw.igImage(user_texture_id, &size, &uv0, &uv1, &tint_col, &border_col);
}
pub inline fn Image(user_texture_id: TextureID, size: Vec2) void {
    return @This().ImageExt(user_texture_id, size, .{.x=0,.y=0}, .{.x=1,.y=1}, .{.x=1,.y=1,.z=1,.w=1}, .{.x=0,.y=0,.z=0,.w=0});
}

pub inline fn ImageButtonExt(user_texture_id: TextureID, size: Vec2, uv0: Vec2, uv1: Vec2, frame_padding: i32, bg_col: Vec4, tint_col: Vec4) bool {
    return raw.igImageButton(user_texture_id, &size, &uv0, &uv1, frame_padding, &bg_col, &tint_col);
}
pub inline fn ImageButton(user_texture_id: TextureID, size: Vec2) bool {
    return @This().ImageButtonExt(user_texture_id, size, .{.x=0,.y=0}, .{.x=1,.y=1}, -1, .{.x=0,.y=0,.z=0,.w=0}, .{.x=1,.y=1,.z=1,.w=1});
}

pub inline fn ImageButtonEx(id: ID, texture_id: TextureID, size: Vec2, uv0: Vec2, uv1: Vec2, padding: Vec2, bg_col: Vec4, tint_col: Vec4) bool {
    return raw.igImageButtonEx(id, texture_id, &size, &uv0, &uv1, &padding, &bg_col, &tint_col);
}

/// IndentExt(indent_w: f32) void
pub const IndentExt = raw.igIndent;
pub inline fn Indent() void {
    return @This().IndentExt(0.0);
}

/// Initialize() void
pub const Initialize = raw.igInitialize;

pub inline fn InputDoubleExt(label: ?[*:0]const u8, v: *f64, step: f64, step_fast: f64, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputDouble(label, v, step, step_fast, format, flags.toInt());
}
pub inline fn InputDouble(label: ?[*:0]const u8, v: *f64) bool {
    return @This().InputDoubleExt(label, v, 0.0, 0.0, "%.6f", .{});
}

pub inline fn InputFloatExt(label: ?[*:0]const u8, v: *f32, step: f32, step_fast: f32, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputFloat(label, v, step, step_fast, format, flags.toInt());
}
pub inline fn InputFloat(label: ?[*:0]const u8, v: *f32) bool {
    return @This().InputFloatExt(label, v, 0.0, 0.0, "%.3f", .{});
}

pub inline fn InputFloat2Ext(label: ?[*:0]const u8, v: *[2]f32, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputFloat2(label, v, format, flags.toInt());
}
pub inline fn InputFloat2(label: ?[*:0]const u8, v: *[2]f32) bool {
    return @This().InputFloat2Ext(label, v, "%.3f", .{});
}

pub inline fn InputFloat3Ext(label: ?[*:0]const u8, v: *[3]f32, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputFloat3(label, v, format, flags.toInt());
}
pub inline fn InputFloat3(label: ?[*:0]const u8, v: *[3]f32) bool {
    return @This().InputFloat3Ext(label, v, "%.3f", .{});
}

pub inline fn InputFloat4Ext(label: ?[*:0]const u8, v: *[4]f32, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputFloat4(label, v, format, flags.toInt());
}
pub inline fn InputFloat4(label: ?[*:0]const u8, v: *[4]f32) bool {
    return @This().InputFloat4Ext(label, v, "%.3f", .{});
}

pub inline fn InputIntExt(label: ?[*:0]const u8, v: *i32, step: i32, step_fast: i32, flags: InputTextFlags) bool {
    return raw.igInputInt(label, v, step, step_fast, flags.toInt());
}
pub inline fn InputInt(label: ?[*:0]const u8, v: *i32) bool {
    return @This().InputIntExt(label, v, 1, 100, .{});
}

pub inline fn InputInt2Ext(label: ?[*:0]const u8, v: *[2]i32, flags: InputTextFlags) bool {
    return raw.igInputInt2(label, v, flags.toInt());
}
pub inline fn InputInt2(label: ?[*:0]const u8, v: *[2]i32) bool {
    return @This().InputInt2Ext(label, v, .{});
}

pub inline fn InputInt3Ext(label: ?[*:0]const u8, v: *[3]i32, flags: InputTextFlags) bool {
    return raw.igInputInt3(label, v, flags.toInt());
}
pub inline fn InputInt3(label: ?[*:0]const u8, v: *[3]i32) bool {
    return @This().InputInt3Ext(label, v, .{});
}

pub inline fn InputInt4Ext(label: ?[*:0]const u8, v: *[4]i32, flags: InputTextFlags) bool {
    return raw.igInputInt4(label, v, flags.toInt());
}
pub inline fn InputInt4(label: ?[*:0]const u8, v: *[4]i32) bool {
    return @This().InputInt4Ext(label, v, .{});
}

pub inline fn InputScalarExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, p_step: ?*const anyopaque, p_step_fast: ?*const anyopaque, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputScalar(label, data_type, p_data, p_step, p_step_fast, format, flags.toInt());
}
pub inline fn InputScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque) bool {
    return @This().InputScalarExt(label, data_type, p_data, null, null, null, .{});
}

pub inline fn InputScalarNExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, p_step: ?*const anyopaque, p_step_fast: ?*const anyopaque, format: ?[*:0]const u8, flags: InputTextFlags) bool {
    return raw.igInputScalarN(label, data_type, p_data, components, p_step, p_step_fast, format, flags.toInt());
}
pub inline fn InputScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32) bool {
    return @This().InputScalarNExt(label, data_type, p_data, components, null, null, null, .{});
}

pub inline fn InputTextExt(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, flags: InputTextFlags, callback: InputTextCallback, user_data: ?*anyopaque) bool {
    return raw.igInputText(label, buf, buf_size, flags.toInt(), callback, user_data);
}
pub inline fn InputText(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize) bool {
    return @This().InputTextExt(label, buf, buf_size, .{}, null, null);
}

pub inline fn InputTextExExt(label: ?[*:0]const u8, hint: [*c]const u8, buf: ?[*]u8, buf_size: i32, size_arg: Vec2, flags: InputTextFlags, callback: InputTextCallback, user_data: ?*anyopaque) bool {
    return raw.igInputTextEx(label, hint, buf, buf_size, &size_arg, flags.toInt(), callback, user_data);
}
pub inline fn InputTextEx(label: ?[*:0]const u8, hint: [*c]const u8, buf: ?[*]u8, buf_size: i32, size_arg: Vec2, flags: InputTextFlags) bool {
    return @This().InputTextExExt(label, hint, buf, buf_size, size_arg, flags, null, null);
}

pub inline fn InputTextMultilineExt(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, size: Vec2, flags: InputTextFlags, callback: InputTextCallback, user_data: ?*anyopaque) bool {
    return raw.igInputTextMultiline(label, buf, buf_size, &size, flags.toInt(), callback, user_data);
}
pub inline fn InputTextMultiline(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize) bool {
    return @This().InputTextMultilineExt(label, buf, buf_size, .{.x=0,.y=0}, .{}, null, null);
}

pub inline fn InputTextWithHintExt(label: ?[*:0]const u8, hint: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, flags: InputTextFlags, callback: InputTextCallback, user_data: ?*anyopaque) bool {
    return raw.igInputTextWithHint(label, hint, buf, buf_size, flags.toInt(), callback, user_data);
}
pub inline fn InputTextWithHint(label: ?[*:0]const u8, hint: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize) bool {
    return @This().InputTextWithHintExt(label, hint, buf, buf_size, .{}, null, null);
}

pub inline fn InvisibleButtonExt(str_id: ?[*:0]const u8, size: Vec2, flags: ButtonFlags) bool {
    return raw.igInvisibleButton(str_id, &size, flags.toInt());
}
pub inline fn InvisibleButton(str_id: ?[*:0]const u8, size: Vec2) bool {
    return @This().InvisibleButtonExt(str_id, size, .{});
}

/// IsActiveIdUsingKey(key: Key) bool
pub const IsActiveIdUsingKey = raw.igIsActiveIdUsingKey;

/// IsActiveIdUsingNavDir(dir: Dir) bool
pub const IsActiveIdUsingNavDir = raw.igIsActiveIdUsingNavDir;

/// IsActiveIdUsingNavInput(input: NavInput) bool
pub const IsActiveIdUsingNavInput = raw.igIsActiveIdUsingNavInput;

/// IsAnyItemActive() bool
pub const IsAnyItemActive = raw.igIsAnyItemActive;

/// IsAnyItemFocused() bool
pub const IsAnyItemFocused = raw.igIsAnyItemFocused;

/// IsAnyItemHovered() bool
pub const IsAnyItemHovered = raw.igIsAnyItemHovered;

/// IsAnyMouseDown() bool
pub const IsAnyMouseDown = raw.igIsAnyMouseDown;

pub inline fn IsClippedEx(bb: Rect, id: ID) bool {
    return raw.igIsClippedEx(&bb, id);
}

/// IsDragDropActive() bool
pub const IsDragDropActive = raw.igIsDragDropActive;

/// IsDragDropPayloadBeingAccepted() bool
pub const IsDragDropPayloadBeingAccepted = raw.igIsDragDropPayloadBeingAccepted;

/// IsGamepadKey(key: Key) bool
pub const IsGamepadKey = raw.igIsGamepadKey;

/// IsItemActivated() bool
pub const IsItemActivated = raw.igIsItemActivated;

/// IsItemActive() bool
pub const IsItemActive = raw.igIsItemActive;

/// IsItemClickedExt(mouse_button: MouseButton) bool
pub const IsItemClickedExt = raw.igIsItemClicked;
pub inline fn IsItemClicked() bool {
    return @This().IsItemClickedExt(.Left);
}

/// IsItemDeactivated() bool
pub const IsItemDeactivated = raw.igIsItemDeactivated;

/// IsItemDeactivatedAfterEdit() bool
pub const IsItemDeactivatedAfterEdit = raw.igIsItemDeactivatedAfterEdit;

/// IsItemEdited() bool
pub const IsItemEdited = raw.igIsItemEdited;

/// IsItemFocused() bool
pub const IsItemFocused = raw.igIsItemFocused;

pub inline fn IsItemHoveredExt(flags: HoveredFlags) bool {
    return raw.igIsItemHovered(flags.toInt());
}
pub inline fn IsItemHovered() bool {
    return @This().IsItemHoveredExt(.{});
}

/// IsItemToggledOpen() bool
pub const IsItemToggledOpen = raw.igIsItemToggledOpen;

/// IsItemToggledSelection() bool
pub const IsItemToggledSelection = raw.igIsItemToggledSelection;

/// IsItemVisible() bool
pub const IsItemVisible = raw.igIsItemVisible;

/// IsKeyDown(key: Key) bool
pub const IsKeyDown = raw.igIsKeyDown;

/// IsKeyPressedExt(key: Key, repeat: bool) bool
pub const IsKeyPressedExt = raw.igIsKeyPressed;
pub inline fn IsKeyPressed(key: Key) bool {
    return @This().IsKeyPressedExt(key, true);
}

/// IsKeyReleased(key: Key) bool
pub const IsKeyReleased = raw.igIsKeyReleased;

/// IsLegacyKey(key: Key) bool
pub const IsLegacyKey = raw.igIsLegacyKey;

/// IsMouseClickedExt(button: MouseButton, repeat: bool) bool
pub const IsMouseClickedExt = raw.igIsMouseClicked;
pub inline fn IsMouseClicked(button: MouseButton) bool {
    return @This().IsMouseClickedExt(button, false);
}

/// IsMouseDoubleClicked(button: MouseButton) bool
pub const IsMouseDoubleClicked = raw.igIsMouseDoubleClicked;

/// IsMouseDown(button: MouseButton) bool
pub const IsMouseDown = raw.igIsMouseDown;

/// IsMouseDragPastThresholdExt(button: MouseButton, lock_threshold: f32) bool
pub const IsMouseDragPastThresholdExt = raw.igIsMouseDragPastThreshold;
pub inline fn IsMouseDragPastThreshold(button: MouseButton) bool {
    return @This().IsMouseDragPastThresholdExt(button, -1.0);
}

/// IsMouseDraggingExt(button: MouseButton, lock_threshold: f32) bool
pub const IsMouseDraggingExt = raw.igIsMouseDragging;
pub inline fn IsMouseDragging(button: MouseButton) bool {
    return @This().IsMouseDraggingExt(button, -1.0);
}

pub inline fn IsMouseHoveringRectExt(r_min: Vec2, r_max: Vec2, clip: bool) bool {
    return raw.igIsMouseHoveringRect(&r_min, &r_max, clip);
}
pub inline fn IsMouseHoveringRect(r_min: Vec2, r_max: Vec2) bool {
    return @This().IsMouseHoveringRectExt(r_min, r_max, true);
}

/// IsMousePosValidExt(mouse_pos: ?*const Vec2) bool
pub const IsMousePosValidExt = raw.igIsMousePosValid;
pub inline fn IsMousePosValid() bool {
    return @This().IsMousePosValidExt(null);
}

/// IsMouseReleased(button: MouseButton) bool
pub const IsMouseReleased = raw.igIsMouseReleased;

/// IsNamedKey(key: Key) bool
pub const IsNamedKey = raw.igIsNamedKey;

/// IsNavInputDown(n: NavInput) bool
pub const IsNavInputDown = raw.igIsNavInputDown;

/// IsNavInputTest(n: NavInput, rm: NavReadMode) bool
pub const IsNavInputTest = raw.igIsNavInputTest;

pub inline fn IsPopupOpen_StrExt(str_id: ?[*:0]const u8, flags: PopupFlags) bool {
    return raw.igIsPopupOpen_Str(str_id, flags.toInt());
}
pub inline fn IsPopupOpen_Str(str_id: ?[*:0]const u8) bool {
    return @This().IsPopupOpen_StrExt(str_id, .{});
}

pub inline fn IsPopupOpen_ID(id: ID, popup_flags: PopupFlags) bool {
    return raw.igIsPopupOpen_ID(id, popup_flags.toInt());
}

pub inline fn IsRectVisible_Nil(size: Vec2) bool {
    return raw.igIsRectVisible_Nil(&size);
}

pub inline fn IsRectVisible_Vec2(rect_min: Vec2, rect_max: Vec2) bool {
    return raw.igIsRectVisible_Vec2(&rect_min, &rect_max);
}

/// IsWindowAbove(potential_above: ?*Window, potential_below: ?*Window) bool
pub const IsWindowAbove = raw.igIsWindowAbove;

/// IsWindowAppearing() bool
pub const IsWindowAppearing = raw.igIsWindowAppearing;

/// IsWindowChildOf(window: ?*Window, potential_parent: ?*Window, popup_hierarchy: bool) bool
pub const IsWindowChildOf = raw.igIsWindowChildOf;

/// IsWindowCollapsed() bool
pub const IsWindowCollapsed = raw.igIsWindowCollapsed;

pub inline fn IsWindowFocusedExt(flags: FocusedFlags) bool {
    return raw.igIsWindowFocused(flags.toInt());
}
pub inline fn IsWindowFocused() bool {
    return @This().IsWindowFocusedExt(.{});
}

pub inline fn IsWindowHoveredExt(flags: HoveredFlags) bool {
    return raw.igIsWindowHovered(flags.toInt());
}
pub inline fn IsWindowHovered() bool {
    return @This().IsWindowHoveredExt(.{});
}

/// IsWindowNavFocusable(window: ?*Window) bool
pub const IsWindowNavFocusable = raw.igIsWindowNavFocusable;

/// IsWindowWithinBeginStackOf(window: ?*Window, potential_parent: ?*Window) bool
pub const IsWindowWithinBeginStackOf = raw.igIsWindowWithinBeginStackOf;

pub inline fn ItemAddExt(bb: Rect, id: ID, nav_bb: ?*const Rect, extra_flags: ItemFlags) bool {
    return raw.igItemAdd(&bb, id, nav_bb, extra_flags.toInt());
}
pub inline fn ItemAdd(bb: Rect, id: ID) bool {
    return @This().ItemAddExt(bb, id, null, .{});
}

pub inline fn ItemHoverable(bb: Rect, id: ID) bool {
    return raw.igItemHoverable(&bb, id);
}

pub inline fn ItemSize_Vec2Ext(size: Vec2, text_baseline_y: f32) void {
    return raw.igItemSize_Vec2(&size, text_baseline_y);
}
pub inline fn ItemSize_Vec2(size: Vec2) void {
    return @This().ItemSize_Vec2Ext(size, -1.0);
}

pub inline fn ItemSize_RectExt(bb: Rect, text_baseline_y: f32) void {
    return raw.igItemSize_Rect(&bb, text_baseline_y);
}
pub inline fn ItemSize_Rect(bb: Rect) void {
    return @This().ItemSize_RectExt(bb, -1.0);
}

/// KeepAliveID(id: ID) void
pub const KeepAliveID = raw.igKeepAliveID;

/// LabelText(label: ?[*:0]const u8, fmt: ?[*:0]const u8, ...: ...) void
pub const LabelText = raw.igLabelText;

/// ListBox_Str_arrExt(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32, height_in_items: i32) bool
pub const ListBox_Str_arrExt = raw.igListBox_Str_arr;
pub inline fn ListBox_Str_arr(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32) bool {
    return @This().ListBox_Str_arrExt(label, current_item, items, items_count, -1);
}

/// ListBox_FnBoolPtrExt(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32, height_in_items: i32) bool
pub const ListBox_FnBoolPtrExt = raw.igListBox_FnBoolPtr;
pub inline fn ListBox_FnBoolPtr(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32) bool {
    return @This().ListBox_FnBoolPtrExt(label, current_item, items_getter, data, items_count, -1);
}

/// LoadIniSettingsFromDisk(ini_filename: ?[*:0]const u8) void
pub const LoadIniSettingsFromDisk = raw.igLoadIniSettingsFromDisk;

/// LoadIniSettingsFromMemoryExt(ini_data: ?[*]const u8, ini_size: usize) void
pub const LoadIniSettingsFromMemoryExt = raw.igLoadIniSettingsFromMemory;
pub inline fn LoadIniSettingsFromMemory(ini_data: ?[*]const u8) void {
    return @This().LoadIniSettingsFromMemoryExt(ini_data, 0);
}

/// LogBegin(kind: LogType, auto_open_depth: i32) void
pub const LogBegin = raw.igLogBegin;

/// LogButtons() void
pub const LogButtons = raw.igLogButtons;

/// LogFinish() void
pub const LogFinish = raw.igLogFinish;

/// LogRenderedTextExt(ref_pos: [*c]const Vec2, text: ?[*]const u8, text_end: ?[*]const u8) void
pub const LogRenderedTextExt = raw.igLogRenderedText;
pub inline fn LogRenderedText(ref_pos: [*c]const Vec2, text: ?[*]const u8) void {
    return @This().LogRenderedTextExt(ref_pos, text, null);
}

/// LogSetNextTextDecoration(prefix: ?[*:0]const u8, suffix: [*c]const u8) void
pub const LogSetNextTextDecoration = raw.igLogSetNextTextDecoration;

/// LogText(fmt: ?[*:0]const u8, ...: ...) void
pub const LogText = raw.igLogText;

/// LogToBufferExt(auto_open_depth: i32) void
pub const LogToBufferExt = raw.igLogToBuffer;
pub inline fn LogToBuffer() void {
    return @This().LogToBufferExt(-1);
}

/// LogToClipboardExt(auto_open_depth: i32) void
pub const LogToClipboardExt = raw.igLogToClipboard;
pub inline fn LogToClipboard() void {
    return @This().LogToClipboardExt(-1);
}

/// LogToFileExt(auto_open_depth: i32, filename: ?[*:0]const u8) void
pub const LogToFileExt = raw.igLogToFile;
pub inline fn LogToFile() void {
    return @This().LogToFileExt(-1, null);
}

/// LogToTTYExt(auto_open_depth: i32) void
pub const LogToTTYExt = raw.igLogToTTY;
pub inline fn LogToTTY() void {
    return @This().LogToTTYExt(-1);
}

/// MarkIniSettingsDirty_Nil() void
pub const MarkIniSettingsDirty_Nil = raw.igMarkIniSettingsDirty_Nil;

/// MarkIniSettingsDirty_WindowPtr(window: ?*Window) void
pub const MarkIniSettingsDirty_WindowPtr = raw.igMarkIniSettingsDirty_WindowPtr;

/// MarkItemEdited(id: ID) void
pub const MarkItemEdited = raw.igMarkItemEdited;

/// MemAlloc(size: usize) ?*anyopaque
pub const MemAlloc = raw.igMemAlloc;

/// MemFree(ptr: ?*anyopaque) void
pub const MemFree = raw.igMemFree;

/// MenuItem_BoolExt(label: ?[*:0]const u8, shortcut: ?[*:0]const u8, selected: bool, enabled: bool) bool
pub const MenuItem_BoolExt = raw.igMenuItem_Bool;
pub inline fn MenuItem_Bool(label: ?[*:0]const u8) bool {
    return @This().MenuItem_BoolExt(label, null, false, true);
}

/// MenuItem_BoolPtrExt(label: ?[*:0]const u8, shortcut: ?[*:0]const u8, p_selected: ?*bool, enabled: bool) bool
pub const MenuItem_BoolPtrExt = raw.igMenuItem_BoolPtr;
pub inline fn MenuItem_BoolPtr(label: ?[*:0]const u8, shortcut: ?[*:0]const u8, p_selected: ?*bool) bool {
    return @This().MenuItem_BoolPtrExt(label, shortcut, p_selected, true);
}

/// MenuItemExExt(label: ?[*:0]const u8, icon: [*c]const u8, shortcut: ?[*:0]const u8, selected: bool, enabled: bool) bool
pub const MenuItemExExt = raw.igMenuItemEx;
pub inline fn MenuItemEx(label: ?[*:0]const u8, icon: [*c]const u8) bool {
    return @This().MenuItemExExt(label, icon, null, false, true);
}

/// NavInitRequestApplyResult() void
pub const NavInitRequestApplyResult = raw.igNavInitRequestApplyResult;

/// NavInitWindow(window: ?*Window, force_reinit: bool) void
pub const NavInitWindow = raw.igNavInitWindow;

/// NavMoveRequestApplyResult() void
pub const NavMoveRequestApplyResult = raw.igNavMoveRequestApplyResult;

/// NavMoveRequestButNoResultYet() bool
pub const NavMoveRequestButNoResultYet = raw.igNavMoveRequestButNoResultYet;

/// NavMoveRequestCancel() void
pub const NavMoveRequestCancel = raw.igNavMoveRequestCancel;

pub inline fn NavMoveRequestForward(move_dir: Dir, clip_dir: Dir, move_flags: NavMoveFlags, scroll_flags: ScrollFlags) void {
    return raw.igNavMoveRequestForward(move_dir, clip_dir, move_flags.toInt(), scroll_flags.toInt());
}

/// NavMoveRequestResolveWithLastItem(result: ?*NavItemData) void
pub const NavMoveRequestResolveWithLastItem = raw.igNavMoveRequestResolveWithLastItem;

pub inline fn NavMoveRequestSubmit(move_dir: Dir, clip_dir: Dir, move_flags: NavMoveFlags, scroll_flags: ScrollFlags) void {
    return raw.igNavMoveRequestSubmit(move_dir, clip_dir, move_flags.toInt(), scroll_flags.toInt());
}

pub inline fn NavMoveRequestTryWrapping(window: ?*Window, move_flags: NavMoveFlags) void {
    return raw.igNavMoveRequestTryWrapping(window, move_flags.toInt());
}

/// NewFrame() void
pub const NewFrame = raw.igNewFrame;

/// NewLine() void
pub const NewLine = raw.igNewLine;

/// NextColumn() void
pub const NextColumn = raw.igNextColumn;

pub inline fn OpenPopup_StrExt(str_id: ?[*:0]const u8, popup_flags: PopupFlags) void {
    return raw.igOpenPopup_Str(str_id, popup_flags.toInt());
}
pub inline fn OpenPopup_Str(str_id: ?[*:0]const u8) void {
    return @This().OpenPopup_StrExt(str_id, .{});
}

pub inline fn OpenPopup_IDExt(id: ID, popup_flags: PopupFlags) void {
    return raw.igOpenPopup_ID(id, popup_flags.toInt());
}
pub inline fn OpenPopup_ID(id: ID) void {
    return @This().OpenPopup_IDExt(id, .{});
}

pub inline fn OpenPopupExExt(id: ID, popup_flags: PopupFlags) void {
    return raw.igOpenPopupEx(id, popup_flags.toInt());
}
pub inline fn OpenPopupEx(id: ID) void {
    return @This().OpenPopupExExt(id, ImGuiPopupFlags_None);
}

pub inline fn OpenPopupOnItemClickExt(str_id: ?[*:0]const u8, popup_flags: PopupFlags) void {
    return raw.igOpenPopupOnItemClick(str_id, popup_flags.toInt());
}
pub inline fn OpenPopupOnItemClick() void {
    return @This().OpenPopupOnItemClickExt(null, .{ .MouseButtonRight = true });
}

pub inline fn PlotEx(plot_type: PlotType, label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, frame_size: Vec2) i32 {
    return raw.igPlotEx(plot_type, label, values_getter, data, values_count, values_offset, overlay_text, scale_min, scale_max, &frame_size);
}

pub inline fn PlotHistogram_FloatPtrExt(label: ?[*:0]const u8, values: *const f32, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: Vec2, stride: i32) void {
    return raw.igPlotHistogram_FloatPtr(label, values, values_count, values_offset, overlay_text, scale_min, scale_max, &graph_size, stride);
}
pub inline fn PlotHistogram_FloatPtr(label: ?[*:0]const u8, values: *const f32, values_count: i32) void {
    return @This().PlotHistogram_FloatPtrExt(label, values, values_count, 0, null, FLT_MAX, FLT_MAX, .{.x=0,.y=0}, @sizeOf(f32));
}

pub inline fn PlotHistogram_FnFloatPtrExt(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: Vec2) void {
    return raw.igPlotHistogram_FnFloatPtr(label, values_getter, data, values_count, values_offset, overlay_text, scale_min, scale_max, &graph_size);
}
pub inline fn PlotHistogram_FnFloatPtr(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32) void {
    return @This().PlotHistogram_FnFloatPtrExt(label, values_getter, data, values_count, 0, null, FLT_MAX, FLT_MAX, .{.x=0,.y=0});
}

pub inline fn PlotLines_FloatPtrExt(label: ?[*:0]const u8, values: *const f32, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: Vec2, stride: i32) void {
    return raw.igPlotLines_FloatPtr(label, values, values_count, values_offset, overlay_text, scale_min, scale_max, &graph_size, stride);
}
pub inline fn PlotLines_FloatPtr(label: ?[*:0]const u8, values: *const f32, values_count: i32) void {
    return @This().PlotLines_FloatPtrExt(label, values, values_count, 0, null, FLT_MAX, FLT_MAX, .{.x=0,.y=0}, @sizeOf(f32));
}

pub inline fn PlotLines_FnFloatPtrExt(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: Vec2) void {
    return raw.igPlotLines_FnFloatPtr(label, values_getter, data, values_count, values_offset, overlay_text, scale_min, scale_max, &graph_size);
}
pub inline fn PlotLines_FnFloatPtr(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32) void {
    return @This().PlotLines_FnFloatPtrExt(label, values_getter, data, values_count, 0, null, FLT_MAX, FLT_MAX, .{.x=0,.y=0});
}

/// PopAllowKeyboardFocus() void
pub const PopAllowKeyboardFocus = raw.igPopAllowKeyboardFocus;

/// PopButtonRepeat() void
pub const PopButtonRepeat = raw.igPopButtonRepeat;

/// PopClipRect() void
pub const PopClipRect = raw.igPopClipRect;

/// PopColumnsBackground() void
pub const PopColumnsBackground = raw.igPopColumnsBackground;

/// PopFocusScope() void
pub const PopFocusScope = raw.igPopFocusScope;

/// PopFont() void
pub const PopFont = raw.igPopFont;

/// PopID() void
pub const PopID = raw.igPopID;

/// PopItemFlag() void
pub const PopItemFlag = raw.igPopItemFlag;

/// PopItemWidth() void
pub const PopItemWidth = raw.igPopItemWidth;

/// PopStyleColorExt(count: i32) void
pub const PopStyleColorExt = raw.igPopStyleColor;
pub inline fn PopStyleColor() void {
    return @This().PopStyleColorExt(1);
}

/// PopStyleVarExt(count: i32) void
pub const PopStyleVarExt = raw.igPopStyleVar;
pub inline fn PopStyleVar() void {
    return @This().PopStyleVarExt(1);
}

/// PopTextWrapPos() void
pub const PopTextWrapPos = raw.igPopTextWrapPos;

pub inline fn ProgressBarExt(fraction: f32, size_arg: Vec2, overlay: ?[*:0]const u8) void {
    return raw.igProgressBar(fraction, &size_arg, overlay);
}
pub inline fn ProgressBar(fraction: f32) void {
    return @This().ProgressBarExt(fraction, .{.x=-FLT_MIN,.y=0}, null);
}

/// PushAllowKeyboardFocus(allow_keyboard_focus: bool) void
pub const PushAllowKeyboardFocus = raw.igPushAllowKeyboardFocus;

/// PushButtonRepeat(repeat: bool) void
pub const PushButtonRepeat = raw.igPushButtonRepeat;

pub inline fn PushClipRect(clip_rect_min: Vec2, clip_rect_max: Vec2, intersect_with_current_clip_rect: bool) void {
    return raw.igPushClipRect(&clip_rect_min, &clip_rect_max, intersect_with_current_clip_rect);
}

/// PushColumnClipRect(column_index: i32) void
pub const PushColumnClipRect = raw.igPushColumnClipRect;

/// PushColumnsBackground() void
pub const PushColumnsBackground = raw.igPushColumnsBackground;

/// PushFocusScope(id: ID) void
pub const PushFocusScope = raw.igPushFocusScope;

/// PushFont(font: ?*Font) void
pub const PushFont = raw.igPushFont;

/// PushID_Str(str_id: ?[*:0]const u8) void
pub const PushID_Str = raw.igPushID_Str;

/// PushID_StrStr(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8) void
pub const PushID_StrStr = raw.igPushID_StrStr;

/// PushID_Ptr(ptr_id: ?*const anyopaque) void
pub const PushID_Ptr = raw.igPushID_Ptr;

/// PushID_Int(int_id: i32) void
pub const PushID_Int = raw.igPushID_Int;

pub inline fn PushItemFlag(option: ItemFlags, enabled: bool) void {
    return raw.igPushItemFlag(option.toInt(), enabled);
}

/// PushItemWidth(item_width: f32) void
pub const PushItemWidth = raw.igPushItemWidth;

/// PushMultiItemsWidths(components: i32, width_full: f32) void
pub const PushMultiItemsWidths = raw.igPushMultiItemsWidths;

/// PushOverrideID(id: ID) void
pub const PushOverrideID = raw.igPushOverrideID;

/// PushStyleColor_U32(idx: Col, col: u32) void
pub const PushStyleColor_U32 = raw.igPushStyleColor_U32;

pub inline fn PushStyleColor_Vec4(idx: Col, col: Vec4) void {
    return raw.igPushStyleColor_Vec4(idx, &col);
}

/// PushStyleVar_Float(idx: StyleVar, val: f32) void
pub const PushStyleVar_Float = raw.igPushStyleVar_Float;

pub inline fn PushStyleVar_Vec2(idx: StyleVar, val: Vec2) void {
    return raw.igPushStyleVar_Vec2(idx, &val);
}

/// PushTextWrapPosExt(wrap_local_pos_x: f32) void
pub const PushTextWrapPosExt = raw.igPushTextWrapPos;
pub inline fn PushTextWrapPos() void {
    return @This().PushTextWrapPosExt(0.0);
}

/// RadioButton_Bool(label: ?[*:0]const u8, active: bool) bool
pub const RadioButton_Bool = raw.igRadioButton_Bool;

/// RadioButton_IntPtr(label: ?[*:0]const u8, v: *i32, v_button: i32) bool
pub const RadioButton_IntPtr = raw.igRadioButton_IntPtr;

/// RemoveContextHook(context: ?*Context, hook_to_remove: ID) void
pub const RemoveContextHook = raw.igRemoveContextHook;

/// RemoveSettingsHandler(type_name: ?[*:0]const u8) void
pub const RemoveSettingsHandler = raw.igRemoveSettingsHandler;

/// Render() void
pub const Render = raw.igRender;

pub inline fn RenderArrowExt(draw_list: ?*DrawList, pos: Vec2, col: u32, dir: Dir, scale: f32) void {
    return raw.igRenderArrow(draw_list, &pos, col, dir, scale);
}
pub inline fn RenderArrow(draw_list: ?*DrawList, pos: Vec2, col: u32, dir: Dir) void {
    return @This().RenderArrowExt(draw_list, pos, col, dir, 1.0);
}

pub inline fn RenderArrowPointingAt(draw_list: ?*DrawList, pos: Vec2, half_sz: Vec2, direction: Dir, col: u32) void {
    return raw.igRenderArrowPointingAt(draw_list, &pos, &half_sz, direction, col);
}

pub inline fn RenderBullet(draw_list: ?*DrawList, pos: Vec2, col: u32) void {
    return raw.igRenderBullet(draw_list, &pos, col);
}

pub inline fn RenderCheckMark(draw_list: ?*DrawList, pos: Vec2, col: u32, sz: f32) void {
    return raw.igRenderCheckMark(draw_list, &pos, col, sz);
}

pub inline fn RenderColorRectWithAlphaCheckerboardExt(draw_list: ?*DrawList, p_min: Vec2, p_max: Vec2, fill_col: u32, grid_step: f32, grid_off: Vec2, rounding: f32, flags: DrawFlags) void {
    return raw.igRenderColorRectWithAlphaCheckerboard(draw_list, &p_min, &p_max, fill_col, grid_step, &grid_off, rounding, flags.toInt());
}
pub inline fn RenderColorRectWithAlphaCheckerboard(draw_list: ?*DrawList, p_min: Vec2, p_max: Vec2, fill_col: u32, grid_step: f32, grid_off: Vec2) void {
    return @This().RenderColorRectWithAlphaCheckerboardExt(draw_list, p_min, p_max, fill_col, grid_step, grid_off, 0.0, .{});
}

pub inline fn RenderFrameExt(p_min: Vec2, p_max: Vec2, fill_col: u32, border: bool, rounding: f32) void {
    return raw.igRenderFrame(&p_min, &p_max, fill_col, border, rounding);
}
pub inline fn RenderFrame(p_min: Vec2, p_max: Vec2, fill_col: u32) void {
    return @This().RenderFrameExt(p_min, p_max, fill_col, true, 0.0);
}

pub inline fn RenderFrameBorderExt(p_min: Vec2, p_max: Vec2, rounding: f32) void {
    return raw.igRenderFrameBorder(&p_min, &p_max, rounding);
}
pub inline fn RenderFrameBorder(p_min: Vec2, p_max: Vec2) void {
    return @This().RenderFrameBorderExt(p_min, p_max, 0.0);
}

pub inline fn RenderMouseCursor(pos: Vec2, scale: f32, mouse_cursor: MouseCursor, col_fill: u32, col_border: u32, col_shadow: u32) void {
    return raw.igRenderMouseCursor(&pos, scale, mouse_cursor, col_fill, col_border, col_shadow);
}

pub inline fn RenderNavHighlightExt(bb: Rect, id: ID, flags: NavHighlightFlags) void {
    return raw.igRenderNavHighlight(&bb, id, flags.toInt());
}
pub inline fn RenderNavHighlight(bb: Rect, id: ID) void {
    return @This().RenderNavHighlightExt(bb, id, ImGuiNavHighlightFlags_TypeDefault);
}

pub inline fn RenderRectFilledRangeH(draw_list: ?*DrawList, rect: Rect, col: u32, x_start_norm: f32, x_end_norm: f32, rounding: f32) void {
    return raw.igRenderRectFilledRangeH(draw_list, &rect, col, x_start_norm, x_end_norm, rounding);
}

pub inline fn RenderRectFilledWithHole(draw_list: ?*DrawList, outer: Rect, inner: Rect, col: u32, rounding: f32) void {
    return raw.igRenderRectFilledWithHole(draw_list, &outer, &inner, col, rounding);
}

pub inline fn RenderTextExt(pos: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, hide_text_after_hash: bool) void {
    return raw.igRenderText(&pos, text, text_end, hide_text_after_hash);
}
pub inline fn RenderText(pos: Vec2, text: ?[*]const u8) void {
    return @This().RenderTextExt(pos, text, null, true);
}

pub inline fn RenderTextClippedExt(pos_min: Vec2, pos_max: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2, align: Vec2, clip_rect: ?*const Rect) void {
    return raw.igRenderTextClipped(&pos_min, &pos_max, text, text_end, text_size_if_known, &align, clip_rect);
}
pub inline fn RenderTextClipped(pos_min: Vec2, pos_max: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2) void {
    return @This().RenderTextClippedExt(pos_min, pos_max, text, text_end, text_size_if_known, .{.x=0,.y=0}, null);
}

pub inline fn RenderTextClippedExExt(draw_list: ?*DrawList, pos_min: Vec2, pos_max: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2, align: Vec2, clip_rect: ?*const Rect) void {
    return raw.igRenderTextClippedEx(draw_list, &pos_min, &pos_max, text, text_end, text_size_if_known, &align, clip_rect);
}
pub inline fn RenderTextClippedEx(draw_list: ?*DrawList, pos_min: Vec2, pos_max: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2) void {
    return @This().RenderTextClippedExExt(draw_list, pos_min, pos_max, text, text_end, text_size_if_known, .{.x=0,.y=0}, null);
}

pub inline fn RenderTextEllipsis(draw_list: ?*DrawList, pos_min: Vec2, pos_max: Vec2, clip_max_x: f32, ellipsis_max_x: f32, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2) void {
    return raw.igRenderTextEllipsis(draw_list, &pos_min, &pos_max, clip_max_x, ellipsis_max_x, text, text_end, text_size_if_known);
}

pub inline fn RenderTextWrapped(pos: Vec2, text: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32) void {
    return raw.igRenderTextWrapped(&pos, text, text_end, wrap_width);
}

/// ResetMouseDragDeltaExt(button: MouseButton) void
pub const ResetMouseDragDeltaExt = raw.igResetMouseDragDelta;
pub inline fn ResetMouseDragDelta() void {
    return @This().ResetMouseDragDeltaExt(.Left);
}

/// SameLineExt(offset_from_start_x: f32, spacing: f32) void
pub const SameLineExt = raw.igSameLine;
pub inline fn SameLine() void {
    return @This().SameLineExt(0.0, -1.0);
}

/// SaveIniSettingsToDisk(ini_filename: ?[*:0]const u8) void
pub const SaveIniSettingsToDisk = raw.igSaveIniSettingsToDisk;

/// SaveIniSettingsToMemoryExt(out_ini_size: ?*usize) ?[*:0]const u8
pub const SaveIniSettingsToMemoryExt = raw.igSaveIniSettingsToMemory;
pub inline fn SaveIniSettingsToMemory() ?[*:0]const u8 {
    return @This().SaveIniSettingsToMemoryExt(null);
}

pub inline fn ScrollToBringRectIntoView(window: ?*Window, rect: Rect) void {
    return raw.igScrollToBringRectIntoView(window, &rect);
}

pub inline fn ScrollToItemExt(flags: ScrollFlags) void {
    return raw.igScrollToItem(flags.toInt());
}
pub inline fn ScrollToItem() void {
    return @This().ScrollToItemExt(.{});
}

pub inline fn ScrollToRectExt(window: ?*Window, rect: Rect, flags: ScrollFlags) void {
    return raw.igScrollToRect(window, &rect, flags.toInt());
}
pub inline fn ScrollToRect(window: ?*Window, rect: Rect) void {
    return @This().ScrollToRectExt(window, rect, .{});
}

pub inline fn ScrollToRectExExt(window: ?*Window, rect: ?*const Rect, flags: ScrollFlags) Vec2 {
    var out: Vec2 = undefined;
    raw.igScrollToRectEx(&out, window, rect, flags.toInt());
    return out;
}
pub inline fn ScrollToRectEx(window: ?*Window, rect: ?*const Rect) Vec2 {
    return @This().ScrollToRectExExt(window, rect, .{});
}

/// Scrollbar(axis: Axis) void
pub const Scrollbar = raw.igScrollbar;

pub inline fn ScrollbarEx(bb: Rect, id: ID, axis: Axis, p_scroll_v: ?*i64, avail_v: i64, contents_v: i64, flags: DrawFlags) bool {
    return raw.igScrollbarEx(&bb, id, axis, p_scroll_v, avail_v, contents_v, flags.toInt());
}

pub inline fn Selectable_BoolExt(label: ?[*:0]const u8, selected: bool, flags: SelectableFlags, size: Vec2) bool {
    return raw.igSelectable_Bool(label, selected, flags.toInt(), &size);
}
pub inline fn Selectable_Bool(label: ?[*:0]const u8) bool {
    return @This().Selectable_BoolExt(label, false, .{}, .{.x=0,.y=0});
}

pub inline fn Selectable_BoolPtrExt(label: ?[*:0]const u8, p_selected: ?*bool, flags: SelectableFlags, size: Vec2) bool {
    return raw.igSelectable_BoolPtr(label, p_selected, flags.toInt(), &size);
}
pub inline fn Selectable_BoolPtr(label: ?[*:0]const u8, p_selected: ?*bool) bool {
    return @This().Selectable_BoolPtrExt(label, p_selected, .{}, .{.x=0,.y=0});
}

/// Separator() void
pub const Separator = raw.igSeparator;

pub inline fn SeparatorEx(flags: SeparatorFlags) void {
    return raw.igSeparatorEx(flags.toInt());
}

/// SetActiveID(id: ID, window: ?*Window) void
pub const SetActiveID = raw.igSetActiveID;

/// SetActiveIdUsingKey(key: Key) void
pub const SetActiveIdUsingKey = raw.igSetActiveIdUsingKey;

/// SetActiveIdUsingNavAndKeys() void
pub const SetActiveIdUsingNavAndKeys = raw.igSetActiveIdUsingNavAndKeys;

/// SetAllocatorFunctionsExt(alloc_func: MemAllocFunc, free_func: MemFreeFunc, user_data: ?*anyopaque) void
pub const SetAllocatorFunctionsExt = raw.igSetAllocatorFunctions;
pub inline fn SetAllocatorFunctions(alloc_func: MemAllocFunc, free_func: MemFreeFunc) void {
    return @This().SetAllocatorFunctionsExt(alloc_func, free_func, null);
}

/// SetClipboardText(text: ?[*:0]const u8) void
pub const SetClipboardText = raw.igSetClipboardText;

pub inline fn SetColorEditOptions(flags: ColorEditFlags) void {
    return raw.igSetColorEditOptions(flags.toInt());
}

/// SetColumnOffset(column_index: i32, offset_x: f32) void
pub const SetColumnOffset = raw.igSetColumnOffset;

/// SetColumnWidth(column_index: i32, width: f32) void
pub const SetColumnWidth = raw.igSetColumnWidth;

/// SetCurrentContext(ctx: ?*Context) void
pub const SetCurrentContext = raw.igSetCurrentContext;

/// SetCurrentFont(font: ?*Font) void
pub const SetCurrentFont = raw.igSetCurrentFont;

pub inline fn SetCursorPos(local_pos: Vec2) void {
    return raw.igSetCursorPos(&local_pos);
}

/// SetCursorPosX(local_x: f32) void
pub const SetCursorPosX = raw.igSetCursorPosX;

/// SetCursorPosY(local_y: f32) void
pub const SetCursorPosY = raw.igSetCursorPosY;

pub inline fn SetCursorScreenPos(pos: Vec2) void {
    return raw.igSetCursorScreenPos(&pos);
}

pub inline fn SetDragDropPayloadExt(kind: ?[*:0]const u8, data: ?*const anyopaque, sz: usize, cond: CondFlags) bool {
    return raw.igSetDragDropPayload(kind, data, sz, cond.toInt());
}
pub inline fn SetDragDropPayload(kind: ?[*:0]const u8, data: ?*const anyopaque, sz: usize) bool {
    return @This().SetDragDropPayloadExt(kind, data, sz, .{});
}

/// SetFocusID(id: ID, window: ?*Window) void
pub const SetFocusID = raw.igSetFocusID;

/// SetHoveredID(id: ID) void
pub const SetHoveredID = raw.igSetHoveredID;

/// SetItemAllowOverlap() void
pub const SetItemAllowOverlap = raw.igSetItemAllowOverlap;

/// SetItemDefaultFocus() void
pub const SetItemDefaultFocus = raw.igSetItemDefaultFocus;

/// SetItemUsingMouseWheel() void
pub const SetItemUsingMouseWheel = raw.igSetItemUsingMouseWheel;

/// SetKeyboardFocusHereExt(offset: i32) void
pub const SetKeyboardFocusHereExt = raw.igSetKeyboardFocusHere;
pub inline fn SetKeyboardFocusHere() void {
    return @This().SetKeyboardFocusHereExt(0);
}

pub inline fn SetLastItemData(item_id: ID, in_flags: ItemFlags, status_flags: ItemStatusFlags, item_rect: Rect) void {
    return raw.igSetLastItemData(item_id, in_flags.toInt(), status_flags.toInt(), &item_rect);
}

/// SetMouseCursor(cursor_type: MouseCursor) void
pub const SetMouseCursor = raw.igSetMouseCursor;

pub inline fn SetNavID(id: ID, nav_layer: NavLayer, focus_scope_id: ID, rect_rel: Rect) void {
    return raw.igSetNavID(id, nav_layer, focus_scope_id, &rect_rel);
}

/// SetNavWindow(window: ?*Window) void
pub const SetNavWindow = raw.igSetNavWindow;

/// SetNextFrameWantCaptureKeyboard(want_capture_keyboard: bool) void
pub const SetNextFrameWantCaptureKeyboard = raw.igSetNextFrameWantCaptureKeyboard;

/// SetNextFrameWantCaptureMouse(want_capture_mouse: bool) void
pub const SetNextFrameWantCaptureMouse = raw.igSetNextFrameWantCaptureMouse;

pub inline fn SetNextItemOpenExt(is_open: bool, cond: CondFlags) void {
    return raw.igSetNextItemOpen(is_open, cond.toInt());
}
pub inline fn SetNextItemOpen(is_open: bool) void {
    return @This().SetNextItemOpenExt(is_open, .{});
}

/// SetNextItemWidth(item_width: f32) void
pub const SetNextItemWidth = raw.igSetNextItemWidth;

/// SetNextWindowBgAlpha(alpha: f32) void
pub const SetNextWindowBgAlpha = raw.igSetNextWindowBgAlpha;

pub inline fn SetNextWindowCollapsedExt(collapsed: bool, cond: CondFlags) void {
    return raw.igSetNextWindowCollapsed(collapsed, cond.toInt());
}
pub inline fn SetNextWindowCollapsed(collapsed: bool) void {
    return @This().SetNextWindowCollapsedExt(collapsed, .{});
}

pub inline fn SetNextWindowContentSize(size: Vec2) void {
    return raw.igSetNextWindowContentSize(&size);
}

/// SetNextWindowFocus() void
pub const SetNextWindowFocus = raw.igSetNextWindowFocus;

pub inline fn SetNextWindowPosExt(pos: Vec2, cond: CondFlags, pivot: Vec2) void {
    return raw.igSetNextWindowPos(&pos, cond.toInt(), &pivot);
}
pub inline fn SetNextWindowPos(pos: Vec2) void {
    return @This().SetNextWindowPosExt(pos, .{}, .{.x=0,.y=0});
}

pub inline fn SetNextWindowScroll(scroll: Vec2) void {
    return raw.igSetNextWindowScroll(&scroll);
}

pub inline fn SetNextWindowSizeExt(size: Vec2, cond: CondFlags) void {
    return raw.igSetNextWindowSize(&size, cond.toInt());
}
pub inline fn SetNextWindowSize(size: Vec2) void {
    return @This().SetNextWindowSizeExt(size, .{});
}

pub inline fn SetNextWindowSizeConstraintsExt(size_min: Vec2, size_max: Vec2, custom_callback: SizeCallback, custom_callback_data: ?*anyopaque) void {
    return raw.igSetNextWindowSizeConstraints(&size_min, &size_max, custom_callback, custom_callback_data);
}
pub inline fn SetNextWindowSizeConstraints(size_min: Vec2, size_max: Vec2) void {
    return @This().SetNextWindowSizeConstraintsExt(size_min, size_max, null, null);
}

/// SetScrollFromPosX_FloatExt(local_x: f32, center_x_ratio: f32) void
pub const SetScrollFromPosX_FloatExt = raw.igSetScrollFromPosX_Float;
pub inline fn SetScrollFromPosX_Float(local_x: f32) void {
    return @This().SetScrollFromPosX_FloatExt(local_x, 0.5);
}

/// SetScrollFromPosX_WindowPtr(window: ?*Window, local_x: f32, center_x_ratio: f32) void
pub const SetScrollFromPosX_WindowPtr = raw.igSetScrollFromPosX_WindowPtr;

/// SetScrollFromPosY_FloatExt(local_y: f32, center_y_ratio: f32) void
pub const SetScrollFromPosY_FloatExt = raw.igSetScrollFromPosY_Float;
pub inline fn SetScrollFromPosY_Float(local_y: f32) void {
    return @This().SetScrollFromPosY_FloatExt(local_y, 0.5);
}

/// SetScrollFromPosY_WindowPtr(window: ?*Window, local_y: f32, center_y_ratio: f32) void
pub const SetScrollFromPosY_WindowPtr = raw.igSetScrollFromPosY_WindowPtr;

/// SetScrollHereXExt(center_x_ratio: f32) void
pub const SetScrollHereXExt = raw.igSetScrollHereX;
pub inline fn SetScrollHereX() void {
    return @This().SetScrollHereXExt(0.5);
}

/// SetScrollHereYExt(center_y_ratio: f32) void
pub const SetScrollHereYExt = raw.igSetScrollHereY;
pub inline fn SetScrollHereY() void {
    return @This().SetScrollHereYExt(0.5);
}

/// SetScrollX_Float(scroll_x: f32) void
pub const SetScrollX_Float = raw.igSetScrollX_Float;

/// SetScrollX_WindowPtr(window: ?*Window, scroll_x: f32) void
pub const SetScrollX_WindowPtr = raw.igSetScrollX_WindowPtr;

/// SetScrollY_Float(scroll_y: f32) void
pub const SetScrollY_Float = raw.igSetScrollY_Float;

/// SetScrollY_WindowPtr(window: ?*Window, scroll_y: f32) void
pub const SetScrollY_WindowPtr = raw.igSetScrollY_WindowPtr;

/// SetStateStorage(storage: ?*Storage) void
pub const SetStateStorage = raw.igSetStateStorage;

/// SetTabItemClosed(tab_or_docked_window_label: ?[*:0]const u8) void
pub const SetTabItemClosed = raw.igSetTabItemClosed;

/// SetTooltip(fmt: ?[*:0]const u8, ...: ...) void
pub const SetTooltip = raw.igSetTooltip;

pub inline fn SetWindowClipRectBeforeSetChannel(window: ?*Window, clip_rect: Rect) void {
    return raw.igSetWindowClipRectBeforeSetChannel(window, &clip_rect);
}

pub inline fn SetWindowCollapsed_BoolExt(collapsed: bool, cond: CondFlags) void {
    return raw.igSetWindowCollapsed_Bool(collapsed, cond.toInt());
}
pub inline fn SetWindowCollapsed_Bool(collapsed: bool) void {
    return @This().SetWindowCollapsed_BoolExt(collapsed, .{});
}

pub inline fn SetWindowCollapsed_StrExt(name: ?[*:0]const u8, collapsed: bool, cond: CondFlags) void {
    return raw.igSetWindowCollapsed_Str(name, collapsed, cond.toInt());
}
pub inline fn SetWindowCollapsed_Str(name: ?[*:0]const u8, collapsed: bool) void {
    return @This().SetWindowCollapsed_StrExt(name, collapsed, .{});
}

pub inline fn SetWindowCollapsed_WindowPtrExt(window: ?*Window, collapsed: bool, cond: CondFlags) void {
    return raw.igSetWindowCollapsed_WindowPtr(window, collapsed, cond.toInt());
}
pub inline fn SetWindowCollapsed_WindowPtr(window: ?*Window, collapsed: bool) void {
    return @This().SetWindowCollapsed_WindowPtrExt(window, collapsed, .{});
}

/// SetWindowFocus_Nil() void
pub const SetWindowFocus_Nil = raw.igSetWindowFocus_Nil;

/// SetWindowFocus_Str(name: ?[*:0]const u8) void
pub const SetWindowFocus_Str = raw.igSetWindowFocus_Str;

/// SetWindowFontScale(scale: f32) void
pub const SetWindowFontScale = raw.igSetWindowFontScale;

pub inline fn SetWindowHitTestHole(window: ?*Window, pos: Vec2, size: Vec2) void {
    return raw.igSetWindowHitTestHole(window, &pos, &size);
}

pub inline fn SetWindowPos_Vec2Ext(pos: Vec2, cond: CondFlags) void {
    return raw.igSetWindowPos_Vec2(&pos, cond.toInt());
}
pub inline fn SetWindowPos_Vec2(pos: Vec2) void {
    return @This().SetWindowPos_Vec2Ext(pos, .{});
}

pub inline fn SetWindowPos_StrExt(name: ?[*:0]const u8, pos: Vec2, cond: CondFlags) void {
    return raw.igSetWindowPos_Str(name, &pos, cond.toInt());
}
pub inline fn SetWindowPos_Str(name: ?[*:0]const u8, pos: Vec2) void {
    return @This().SetWindowPos_StrExt(name, pos, .{});
}

pub inline fn SetWindowPos_WindowPtrExt(window: ?*Window, pos: Vec2, cond: CondFlags) void {
    return raw.igSetWindowPos_WindowPtr(window, &pos, cond.toInt());
}
pub inline fn SetWindowPos_WindowPtr(window: ?*Window, pos: Vec2) void {
    return @This().SetWindowPos_WindowPtrExt(window, pos, .{});
}

pub inline fn SetWindowSize_Vec2Ext(size: Vec2, cond: CondFlags) void {
    return raw.igSetWindowSize_Vec2(&size, cond.toInt());
}
pub inline fn SetWindowSize_Vec2(size: Vec2) void {
    return @This().SetWindowSize_Vec2Ext(size, .{});
}

pub inline fn SetWindowSize_StrExt(name: ?[*:0]const u8, size: Vec2, cond: CondFlags) void {
    return raw.igSetWindowSize_Str(name, &size, cond.toInt());
}
pub inline fn SetWindowSize_Str(name: ?[*:0]const u8, size: Vec2) void {
    return @This().SetWindowSize_StrExt(name, size, .{});
}

pub inline fn SetWindowSize_WindowPtrExt(window: ?*Window, size: Vec2, cond: CondFlags) void {
    return raw.igSetWindowSize_WindowPtr(window, &size, cond.toInt());
}
pub inline fn SetWindowSize_WindowPtr(window: ?*Window, size: Vec2) void {
    return @This().SetWindowSize_WindowPtrExt(window, size, .{});
}

/// SetWindowViewport(window: ?*Window, viewport: ?*ViewportP) void
pub const SetWindowViewport = raw.igSetWindowViewport;

pub inline fn ShadeVertsLinearColorGradientKeepAlpha(draw_list: ?*DrawList, vert_start_idx: i32, vert_end_idx: i32, gradient_p0: Vec2, gradient_p1: Vec2, col0: u32, col1: u32) void {
    return raw.igShadeVertsLinearColorGradientKeepAlpha(draw_list, vert_start_idx, vert_end_idx, &gradient_p0, &gradient_p1, col0, col1);
}

pub inline fn ShadeVertsLinearUV(draw_list: ?*DrawList, vert_start_idx: i32, vert_end_idx: i32, a: Vec2, b: Vec2, uv_a: Vec2, uv_b: Vec2, clamp: bool) void {
    return raw.igShadeVertsLinearUV(draw_list, vert_start_idx, vert_end_idx, &a, &b, &uv_a, &uv_b, clamp);
}

/// ShowAboutWindowExt(p_open: ?*bool) void
pub const ShowAboutWindowExt = raw.igShowAboutWindow;
pub inline fn ShowAboutWindow() void {
    return @This().ShowAboutWindowExt(null);
}

/// ShowDebugLogWindowExt(p_open: ?*bool) void
pub const ShowDebugLogWindowExt = raw.igShowDebugLogWindow;
pub inline fn ShowDebugLogWindow() void {
    return @This().ShowDebugLogWindowExt(null);
}

/// ShowDemoWindowExt(p_open: ?*bool) void
pub const ShowDemoWindowExt = raw.igShowDemoWindow;
pub inline fn ShowDemoWindow() void {
    return @This().ShowDemoWindowExt(null);
}

/// ShowFontAtlas(atlas: ?*FontAtlas) void
pub const ShowFontAtlas = raw.igShowFontAtlas;

/// ShowFontSelector(label: ?[*:0]const u8) void
pub const ShowFontSelector = raw.igShowFontSelector;

/// ShowMetricsWindowExt(p_open: ?*bool) void
pub const ShowMetricsWindowExt = raw.igShowMetricsWindow;
pub inline fn ShowMetricsWindow() void {
    return @This().ShowMetricsWindowExt(null);
}

/// ShowStackToolWindowExt(p_open: ?*bool) void
pub const ShowStackToolWindowExt = raw.igShowStackToolWindow;
pub inline fn ShowStackToolWindow() void {
    return @This().ShowStackToolWindowExt(null);
}

/// ShowStyleEditorExt(ref: ?*Style) void
pub const ShowStyleEditorExt = raw.igShowStyleEditor;
pub inline fn ShowStyleEditor() void {
    return @This().ShowStyleEditorExt(null);
}

/// ShowStyleSelector(label: ?[*:0]const u8) bool
pub const ShowStyleSelector = raw.igShowStyleSelector;

/// ShowUserGuide() void
pub const ShowUserGuide = raw.igShowUserGuide;

/// ShrinkWidths(items: [*c]ShrinkWidthItem, count: i32, width_excess: f32) void
pub const ShrinkWidths = raw.igShrinkWidths;

/// Shutdown() void
pub const Shutdown = raw.igShutdown;

pub inline fn SliderAngleExt(label: ?[*:0]const u8, v_rad: *f32, v_degrees_min: f32, v_degrees_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderAngle(label, v_rad, v_degrees_min, v_degrees_max, format, flags.toInt());
}
pub inline fn SliderAngle(label: ?[*:0]const u8, v_rad: *f32) bool {
    return @This().SliderAngleExt(label, v_rad, -360.0, 360.0, "%.0f deg", .{});
}

pub inline fn SliderBehavior(bb: Rect, id: ID, data_type: DataType, p_v: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags, out_grab_bb: ?*Rect) bool {
    return raw.igSliderBehavior(&bb, id, data_type, p_v, p_min, p_max, format, flags.toInt(), out_grab_bb);
}

pub inline fn SliderFloatExt(label: ?[*:0]const u8, v: *f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderFloat(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderFloat(label: ?[*:0]const u8, v: *f32, v_min: f32, v_max: f32) bool {
    return @This().SliderFloatExt(label, v, v_min, v_max, "%.3f", .{});
}

pub inline fn SliderFloat2Ext(label: ?[*:0]const u8, v: *[2]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderFloat2(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderFloat2(label: ?[*:0]const u8, v: *[2]f32, v_min: f32, v_max: f32) bool {
    return @This().SliderFloat2Ext(label, v, v_min, v_max, "%.3f", .{});
}

pub inline fn SliderFloat3Ext(label: ?[*:0]const u8, v: *[3]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderFloat3(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderFloat3(label: ?[*:0]const u8, v: *[3]f32, v_min: f32, v_max: f32) bool {
    return @This().SliderFloat3Ext(label, v, v_min, v_max, "%.3f", .{});
}

pub inline fn SliderFloat4Ext(label: ?[*:0]const u8, v: *[4]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderFloat4(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderFloat4(label: ?[*:0]const u8, v: *[4]f32, v_min: f32, v_max: f32) bool {
    return @This().SliderFloat4Ext(label, v, v_min, v_max, "%.3f", .{});
}

pub inline fn SliderIntExt(label: ?[*:0]const u8, v: *i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderInt(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderInt(label: ?[*:0]const u8, v: *i32, v_min: i32, v_max: i32) bool {
    return @This().SliderIntExt(label, v, v_min, v_max, "%d", .{});
}

pub inline fn SliderInt2Ext(label: ?[*:0]const u8, v: *[2]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderInt2(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderInt2(label: ?[*:0]const u8, v: *[2]i32, v_min: i32, v_max: i32) bool {
    return @This().SliderInt2Ext(label, v, v_min, v_max, "%d", .{});
}

pub inline fn SliderInt3Ext(label: ?[*:0]const u8, v: *[3]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderInt3(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderInt3(label: ?[*:0]const u8, v: *[3]i32, v_min: i32, v_max: i32) bool {
    return @This().SliderInt3Ext(label, v, v_min, v_max, "%d", .{});
}

pub inline fn SliderInt4Ext(label: ?[*:0]const u8, v: *[4]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderInt4(label, v, v_min, v_max, format, flags.toInt());
}
pub inline fn SliderInt4(label: ?[*:0]const u8, v: *[4]i32, v_min: i32, v_max: i32) bool {
    return @This().SliderInt4Ext(label, v, v_min, v_max, "%d", .{});
}

pub inline fn SliderScalarExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderScalar(label, data_type, p_data, p_min, p_max, format, flags.toInt());
}
pub inline fn SliderScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque) bool {
    return @This().SliderScalarExt(label, data_type, p_data, p_min, p_max, null, .{});
}

pub inline fn SliderScalarNExt(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igSliderScalarN(label, data_type, p_data, components, p_min, p_max, format, flags.toInt());
}
pub inline fn SliderScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, p_min: ?*const anyopaque, p_max: ?*const anyopaque) bool {
    return @This().SliderScalarNExt(label, data_type, p_data, components, p_min, p_max, null, .{});
}

/// SmallButton(label: ?[*:0]const u8) bool
pub const SmallButton = raw.igSmallButton;

/// Spacing() void
pub const Spacing = raw.igSpacing;

pub inline fn SplitterBehaviorExt(bb: Rect, id: ID, axis: Axis, size1: [*c]f32, size2: [*c]f32, min_size1: f32, min_size2: f32, hover_extend: f32, hover_visibility_delay: f32) bool {
    return raw.igSplitterBehavior(&bb, id, axis, size1, size2, min_size1, min_size2, hover_extend, hover_visibility_delay);
}
pub inline fn SplitterBehavior(bb: Rect, id: ID, axis: Axis, size1: [*c]f32, size2: [*c]f32, min_size1: f32, min_size2: f32) bool {
    return @This().SplitterBehaviorExt(bb, id, axis, size1, size2, min_size1, min_size2, 0.0, 0.0);
}

/// StartMouseMovingWindow(window: ?*Window) void
pub const StartMouseMovingWindow = raw.igStartMouseMovingWindow;

/// StyleColorsClassicExt(dst: ?*Style) void
pub const StyleColorsClassicExt = raw.igStyleColorsClassic;
pub inline fn StyleColorsClassic() void {
    return @This().StyleColorsClassicExt(null);
}

/// StyleColorsDarkExt(dst: ?*Style) void
pub const StyleColorsDarkExt = raw.igStyleColorsDark;
pub inline fn StyleColorsDark() void {
    return @This().StyleColorsDarkExt(null);
}

/// StyleColorsLightExt(dst: ?*Style) void
pub const StyleColorsLightExt = raw.igStyleColorsLight;
pub inline fn StyleColorsLight() void {
    return @This().StyleColorsLightExt(null);
}

/// TabBarCloseTab(tab_bar: ?*TabBar, tab: ?*TabItem) void
pub const TabBarCloseTab = raw.igTabBarCloseTab;

/// TabBarFindTabByID(tab_bar: ?*TabBar, tab_id: ID) ?*TabItem
pub const TabBarFindTabByID = raw.igTabBarFindTabByID;

/// TabBarProcessReorder(tab_bar: ?*TabBar) bool
pub const TabBarProcessReorder = raw.igTabBarProcessReorder;

/// TabBarQueueReorder(tab_bar: ?*TabBar, tab: ?*const TabItem, offset: i32) void
pub const TabBarQueueReorder = raw.igTabBarQueueReorder;

pub inline fn TabBarQueueReorderFromMousePos(tab_bar: ?*TabBar, tab: ?*const TabItem, mouse_pos: Vec2) void {
    return raw.igTabBarQueueReorderFromMousePos(tab_bar, tab, &mouse_pos);
}

/// TabBarRemoveTab(tab_bar: ?*TabBar, tab_id: ID) void
pub const TabBarRemoveTab = raw.igTabBarRemoveTab;

pub inline fn TabItemBackground(draw_list: ?*DrawList, bb: Rect, flags: TabItemFlags, col: u32) void {
    return raw.igTabItemBackground(draw_list, &bb, flags.toInt(), col);
}

pub inline fn TabItemButtonExt(label: ?[*:0]const u8, flags: TabItemFlags) bool {
    return raw.igTabItemButton(label, flags.toInt());
}
pub inline fn TabItemButton(label: ?[*:0]const u8) bool {
    return @This().TabItemButtonExt(label, .{});
}

pub inline fn TabItemCalcSize(label: ?[*:0]const u8, has_close_button: bool) Vec2 {
    var out: Vec2 = undefined;
    raw.igTabItemCalcSize(&out, label, has_close_button);
    return out;
}

pub inline fn TabItemEx(tab_bar: ?*TabBar, label: ?[*:0]const u8, p_open: ?*bool, flags: TabItemFlags) bool {
    return raw.igTabItemEx(tab_bar, label, p_open, flags.toInt());
}

pub inline fn TabItemLabelAndCloseButton(draw_list: ?*DrawList, bb: Rect, flags: TabItemFlags, frame_padding: Vec2, label: ?[*:0]const u8, tab_id: ID, close_button_id: ID, is_contents_visible: bool, out_just_closed: *bool, out_text_clipped: *bool) void {
    return raw.igTabItemLabelAndCloseButton(draw_list, &bb, flags.toInt(), &frame_padding, label, tab_id, close_button_id, is_contents_visible, out_just_closed, out_text_clipped);
}

/// TableBeginApplyRequests(table: ?*Table) void
pub const TableBeginApplyRequests = raw.igTableBeginApplyRequests;

/// TableBeginCell(table: ?*Table, column_n: i32) void
pub const TableBeginCell = raw.igTableBeginCell;

/// TableBeginInitMemory(table: ?*Table, columns_count: i32) void
pub const TableBeginInitMemory = raw.igTableBeginInitMemory;

/// TableBeginRow(table: ?*Table) void
pub const TableBeginRow = raw.igTableBeginRow;

/// TableDrawBorders(table: ?*Table) void
pub const TableDrawBorders = raw.igTableDrawBorders;

/// TableDrawContextMenu(table: ?*Table) void
pub const TableDrawContextMenu = raw.igTableDrawContextMenu;

/// TableEndCell(table: ?*Table) void
pub const TableEndCell = raw.igTableEndCell;

/// TableEndRow(table: ?*Table) void
pub const TableEndRow = raw.igTableEndRow;

/// TableFindByID(id: ID) ?*Table
pub const TableFindByID = raw.igTableFindByID;

/// TableFixColumnSortDirection(table: ?*Table, column: ?*TableColumn) void
pub const TableFixColumnSortDirection = raw.igTableFixColumnSortDirection;

/// TableGcCompactSettings() void
pub const TableGcCompactSettings = raw.igTableGcCompactSettings;

/// TableGcCompactTransientBuffers_TablePtr(table: ?*Table) void
pub const TableGcCompactTransientBuffers_TablePtr = raw.igTableGcCompactTransientBuffers_TablePtr;

/// TableGcCompactTransientBuffers_TableTempDataPtr(table: ?*TableTempData) void
pub const TableGcCompactTransientBuffers_TableTempDataPtr = raw.igTableGcCompactTransientBuffers_TableTempDataPtr;

/// TableGetBoundSettings(table: ?*Table) ?*TableSettings
pub const TableGetBoundSettings = raw.igTableGetBoundSettings;

pub inline fn TableGetCellBgRect(table: ?*const Table, column_n: i32) Rect {
    var out: Rect = undefined;
    raw.igTableGetCellBgRect(&out, table, column_n);
    return out;
}

/// TableGetColumnCount() i32
pub const TableGetColumnCount = raw.igTableGetColumnCount;

pub inline fn TableGetColumnFlagsExt(column_n: i32) TableColumnFlags {
    const _retflags = raw.igTableGetColumnFlags(column_n);
    return TableColumnFlags.fromInt(_retflags);
}
pub inline fn TableGetColumnFlags() TableColumnFlags {
    return @This().TableGetColumnFlagsExt(-1);
}

/// TableGetColumnIndex() i32
pub const TableGetColumnIndex = raw.igTableGetColumnIndex;

/// TableGetColumnName_IntExt(column_n: i32) [*c]const u8
pub const TableGetColumnName_IntExt = raw.igTableGetColumnName_Int;
pub inline fn TableGetColumnName_Int() [*c]const u8 {
    return @This().TableGetColumnName_IntExt(-1);
}

/// TableGetColumnName_TablePtr(table: ?*const Table, column_n: i32) [*c]const u8
pub const TableGetColumnName_TablePtr = raw.igTableGetColumnName_TablePtr;

/// TableGetColumnNextSortDirection(column: ?*TableColumn) SortDirection
pub const TableGetColumnNextSortDirection = raw.igTableGetColumnNextSortDirection;

/// TableGetColumnResizeIDExt(table: ?*const Table, column_n: i32, instance_no: i32) ID
pub const TableGetColumnResizeIDExt = raw.igTableGetColumnResizeID;
pub inline fn TableGetColumnResizeID(table: ?*const Table, column_n: i32) ID {
    return @This().TableGetColumnResizeIDExt(table, column_n, 0);
}

/// TableGetColumnWidthAuto(table: ?*Table, column: ?*TableColumn) f32
pub const TableGetColumnWidthAuto = raw.igTableGetColumnWidthAuto;

/// TableGetHeaderRowHeight() f32
pub const TableGetHeaderRowHeight = raw.igTableGetHeaderRowHeight;

/// TableGetHoveredColumn() i32
pub const TableGetHoveredColumn = raw.igTableGetHoveredColumn;

/// TableGetInstanceData(table: ?*Table, instance_no: i32) ?*TableInstanceData
pub const TableGetInstanceData = raw.igTableGetInstanceData;

/// TableGetMaxColumnWidth(table: ?*const Table, column_n: i32) f32
pub const TableGetMaxColumnWidth = raw.igTableGetMaxColumnWidth;

/// TableGetRowIndex() i32
pub const TableGetRowIndex = raw.igTableGetRowIndex;

/// TableGetSortSpecs() ?*TableSortSpecs
pub const TableGetSortSpecs = raw.igTableGetSortSpecs;

/// TableHeader(label: ?[*:0]const u8) void
pub const TableHeader = raw.igTableHeader;

/// TableHeadersRow() void
pub const TableHeadersRow = raw.igTableHeadersRow;

/// TableLoadSettings(table: ?*Table) void
pub const TableLoadSettings = raw.igTableLoadSettings;

/// TableMergeDrawChannels(table: ?*Table) void
pub const TableMergeDrawChannels = raw.igTableMergeDrawChannels;

/// TableNextColumn() bool
pub const TableNextColumn = raw.igTableNextColumn;

pub inline fn TableNextRowExt(row_flags: TableRowFlags, min_row_height: f32) void {
    return raw.igTableNextRow(row_flags.toInt(), min_row_height);
}
pub inline fn TableNextRow() void {
    return @This().TableNextRowExt(.{}, 0.0);
}

/// TableOpenContextMenuExt(column_n: i32) void
pub const TableOpenContextMenuExt = raw.igTableOpenContextMenu;
pub inline fn TableOpenContextMenu() void {
    return @This().TableOpenContextMenuExt(-1);
}

/// TablePopBackgroundChannel() void
pub const TablePopBackgroundChannel = raw.igTablePopBackgroundChannel;

/// TablePushBackgroundChannel() void
pub const TablePushBackgroundChannel = raw.igTablePushBackgroundChannel;

/// TableRemove(table: ?*Table) void
pub const TableRemove = raw.igTableRemove;

/// TableResetSettings(table: ?*Table) void
pub const TableResetSettings = raw.igTableResetSettings;

/// TableSaveSettings(table: ?*Table) void
pub const TableSaveSettings = raw.igTableSaveSettings;

/// TableSetBgColorExt(target: TableBgTarget, color: u32, column_n: i32) void
pub const TableSetBgColorExt = raw.igTableSetBgColor;
pub inline fn TableSetBgColor(target: TableBgTarget, color: u32) void {
    return @This().TableSetBgColorExt(target, color, -1);
}

/// TableSetColumnEnabled(column_n: i32, v: bool) void
pub const TableSetColumnEnabled = raw.igTableSetColumnEnabled;

/// TableSetColumnIndex(column_n: i32) bool
pub const TableSetColumnIndex = raw.igTableSetColumnIndex;

/// TableSetColumnSortDirection(column_n: i32, sort_direction: SortDirection, append_to_sort_specs: bool) void
pub const TableSetColumnSortDirection = raw.igTableSetColumnSortDirection;

/// TableSetColumnWidth(column_n: i32, width: f32) void
pub const TableSetColumnWidth = raw.igTableSetColumnWidth;

/// TableSetColumnWidthAutoAll(table: ?*Table) void
pub const TableSetColumnWidthAutoAll = raw.igTableSetColumnWidthAutoAll;

/// TableSetColumnWidthAutoSingle(table: ?*Table, column_n: i32) void
pub const TableSetColumnWidthAutoSingle = raw.igTableSetColumnWidthAutoSingle;

/// TableSettingsAddSettingsHandler() void
pub const TableSettingsAddSettingsHandler = raw.igTableSettingsAddSettingsHandler;

/// TableSettingsCreate(id: ID, columns_count: i32) ?*TableSettings
pub const TableSettingsCreate = raw.igTableSettingsCreate;

/// TableSettingsFindByID(id: ID) ?*TableSettings
pub const TableSettingsFindByID = raw.igTableSettingsFindByID;

pub inline fn TableSetupColumnExt(label: ?[*:0]const u8, flags: TableColumnFlags, init_width_or_weight: f32, user_id: ID) void {
    return raw.igTableSetupColumn(label, flags.toInt(), init_width_or_weight, user_id);
}
pub inline fn TableSetupColumn(label: ?[*:0]const u8) void {
    return @This().TableSetupColumnExt(label, .{}, 0.0, 0);
}

/// TableSetupDrawChannels(table: ?*Table) void
pub const TableSetupDrawChannels = raw.igTableSetupDrawChannels;

/// TableSetupScrollFreeze(cols: i32, rows: i32) void
pub const TableSetupScrollFreeze = raw.igTableSetupScrollFreeze;

/// TableSortSpecsBuild(table: ?*Table) void
pub const TableSortSpecsBuild = raw.igTableSortSpecsBuild;

/// TableSortSpecsSanitize(table: ?*Table) void
pub const TableSortSpecsSanitize = raw.igTableSortSpecsSanitize;

/// TableUpdateBorders(table: ?*Table) void
pub const TableUpdateBorders = raw.igTableUpdateBorders;

/// TableUpdateColumnsWeightFromWidth(table: ?*Table) void
pub const TableUpdateColumnsWeightFromWidth = raw.igTableUpdateColumnsWeightFromWidth;

/// TableUpdateLayout(table: ?*Table) void
pub const TableUpdateLayout = raw.igTableUpdateLayout;

/// TempInputIsActive(id: ID) bool
pub const TempInputIsActive = raw.igTempInputIsActive;

pub inline fn TempInputScalarExt(bb: Rect, id: ID, label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, format: ?[*:0]const u8, p_clamp_min: ?*const anyopaque, p_clamp_max: ?*const anyopaque) bool {
    return raw.igTempInputScalar(&bb, id, label, data_type, p_data, format, p_clamp_min, p_clamp_max);
}
pub inline fn TempInputScalar(bb: Rect, id: ID, label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, format: ?[*:0]const u8) bool {
    return @This().TempInputScalarExt(bb, id, label, data_type, p_data, format, null, null);
}

pub inline fn TempInputText(bb: Rect, id: ID, label: ?[*:0]const u8, buf: ?[*]u8, buf_size: i32, flags: InputTextFlags) bool {
    return raw.igTempInputText(&bb, id, label, buf, buf_size, flags.toInt());
}

/// Text(fmt: ?[*:0]const u8, ...: ...) void
pub const Text = raw.igText;

/// TextColored(col: Vec4, fmt: ?[*:0]const u8, ...: ...) void
pub const TextColored = raw.igTextColored;

/// TextDisabled(fmt: ?[*:0]const u8, ...: ...) void
pub const TextDisabled = raw.igTextDisabled;

pub inline fn TextExExt(text: ?[*]const u8, text_end: ?[*]const u8, flags: TextFlags) void {
    return raw.igTextEx(text, text_end, flags.toInt());
}
pub inline fn TextEx(text: ?[*]const u8) void {
    return @This().TextExExt(text, null, .{});
}

/// TextUnformattedExt(text: ?[*]const u8, text_end: ?[*]const u8) void
pub const TextUnformattedExt = raw.igTextUnformatted;
pub inline fn TextUnformatted(text: ?[*]const u8) void {
    return @This().TextUnformattedExt(text, null);
}

/// TextWrapped(fmt: ?[*:0]const u8, ...: ...) void
pub const TextWrapped = raw.igTextWrapped;

/// TreeNode_Str(label: ?[*:0]const u8) bool
pub const TreeNode_Str = raw.igTreeNode_Str;

/// TreeNode_StrStr(str_id: ?[*:0]const u8, fmt: ?[*:0]const u8, ...: ...) bool
pub const TreeNode_StrStr = raw.igTreeNode_StrStr;

/// TreeNode_Ptr(ptr_id: ?*const anyopaque, fmt: ?[*:0]const u8, ...: ...) bool
pub const TreeNode_Ptr = raw.igTreeNode_Ptr;

pub inline fn TreeNodeBehaviorExt(id: ID, flags: TreeNodeFlags, label: ?[*:0]const u8, label_end: ?[*]const u8) bool {
    return raw.igTreeNodeBehavior(id, flags.toInt(), label, label_end);
}
pub inline fn TreeNodeBehavior(id: ID, flags: TreeNodeFlags, label: ?[*:0]const u8) bool {
    return @This().TreeNodeBehaviorExt(id, flags, label, null);
}

pub inline fn TreeNodeBehaviorIsOpenExt(id: ID, flags: TreeNodeFlags) bool {
    return raw.igTreeNodeBehaviorIsOpen(id, flags.toInt());
}
pub inline fn TreeNodeBehaviorIsOpen(id: ID) bool {
    return @This().TreeNodeBehaviorIsOpenExt(id, .{});
}

pub inline fn TreeNodeEx_StrExt(label: ?[*:0]const u8, flags: TreeNodeFlags) bool {
    return raw.igTreeNodeEx_Str(label, flags.toInt());
}
pub inline fn TreeNodeEx_Str(label: ?[*:0]const u8) bool {
    return @This().TreeNodeEx_StrExt(label, .{});
}

/// TreeNodeEx_StrStr(str_id: ?[*:0]const u8, flags: TreeNodeFlags, fmt: ?[*:0]const u8, ...: ...) bool
pub const TreeNodeEx_StrStr = raw.igTreeNodeEx_StrStr;

/// TreeNodeEx_Ptr(ptr_id: ?*const anyopaque, flags: TreeNodeFlags, fmt: ?[*:0]const u8, ...: ...) bool
pub const TreeNodeEx_Ptr = raw.igTreeNodeEx_Ptr;

/// TreePop() void
pub const TreePop = raw.igTreePop;

/// TreePush_Str(str_id: ?[*:0]const u8) void
pub const TreePush_Str = raw.igTreePush_Str;

/// TreePush_PtrExt(ptr_id: ?*const anyopaque) void
pub const TreePush_PtrExt = raw.igTreePush_Ptr;
pub inline fn TreePush_Ptr() void {
    return @This().TreePush_PtrExt(null);
}

/// TreePushOverrideID(id: ID) void
pub const TreePushOverrideID = raw.igTreePushOverrideID;

/// UnindentExt(indent_w: f32) void
pub const UnindentExt = raw.igUnindent;
pub inline fn Unindent() void {
    return @This().UnindentExt(0.0);
}

/// UpdateHoveredWindowAndCaptureFlags() void
pub const UpdateHoveredWindowAndCaptureFlags = raw.igUpdateHoveredWindowAndCaptureFlags;

/// UpdateInputEvents(trickle_fast_inputs: bool) void
pub const UpdateInputEvents = raw.igUpdateInputEvents;

/// UpdateMouseMovingWindowEndFrame() void
pub const UpdateMouseMovingWindowEndFrame = raw.igUpdateMouseMovingWindowEndFrame;

/// UpdateMouseMovingWindowNewFrame() void
pub const UpdateMouseMovingWindowNewFrame = raw.igUpdateMouseMovingWindowNewFrame;

pub inline fn UpdateWindowParentAndRootLinks(window: ?*Window, flags: WindowFlags, parent_window: ?*Window) void {
    return raw.igUpdateWindowParentAndRootLinks(window, flags.toInt(), parent_window);
}

pub inline fn VSliderFloatExt(label: ?[*:0]const u8, size: Vec2, v: *f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igVSliderFloat(label, &size, v, v_min, v_max, format, flags.toInt());
}
pub inline fn VSliderFloat(label: ?[*:0]const u8, size: Vec2, v: *f32, v_min: f32, v_max: f32) bool {
    return @This().VSliderFloatExt(label, size, v, v_min, v_max, "%.3f", .{});
}

pub inline fn VSliderIntExt(label: ?[*:0]const u8, size: Vec2, v: *i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igVSliderInt(label, &size, v, v_min, v_max, format, flags.toInt());
}
pub inline fn VSliderInt(label: ?[*:0]const u8, size: Vec2, v: *i32, v_min: i32, v_max: i32) bool {
    return @This().VSliderIntExt(label, size, v, v_min, v_max, "%d", .{});
}

pub inline fn VSliderScalarExt(label: ?[*:0]const u8, size: Vec2, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlags) bool {
    return raw.igVSliderScalar(label, &size, data_type, p_data, p_min, p_max, format, flags.toInt());
}
pub inline fn VSliderScalar(label: ?[*:0]const u8, size: Vec2, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque) bool {
    return @This().VSliderScalarExt(label, size, data_type, p_data, p_min, p_max, null, .{});
}

/// Value_Bool(prefix: ?[*:0]const u8, b: bool) void
pub const Value_Bool = raw.igValue_Bool;

/// Value_Int(prefix: ?[*:0]const u8, v: i32) void
pub const Value_Int = raw.igValue_Int;

/// Value_Uint(prefix: ?[*:0]const u8, v: u32) void
pub const Value_Uint = raw.igValue_Uint;

/// Value_FloatExt(prefix: ?[*:0]const u8, v: f32, float_format: ?[*:0]const u8) void
pub const Value_FloatExt = raw.igValue_Float;
pub inline fn Value_Float(prefix: ?[*:0]const u8, v: f32) void {
    return @This().Value_FloatExt(prefix, v, null);
}

pub inline fn WindowRectAbsToRel(window: ?*Window, r: ?*const Rect) Rect {
    var out: Rect = undefined;
    raw.igWindowRectAbsToRel(&out, window, r);
    return out;
}

pub inline fn WindowRectRelToAbs(window: ?*Window, r: ?*const Rect) Rect {
    var out: Rect = undefined;
    raw.igWindowRectRelToAbs(&out, window, r);
    return out;
}

pub const raw = struct {
    pub extern fn ImBitVector_Clear(self: *BitVector) callconv(.C) void;
    pub extern fn ImBitVector_ClearBit(self: *BitVector, n: i32) callconv(.C) void;
    pub extern fn ImBitVector_Create(self: *BitVector, sz: i32) callconv(.C) void;
    pub extern fn ImBitVector_SetBit(self: *BitVector, n: i32) callconv(.C) void;
    pub extern fn ImBitVector_TestBit(self: *const BitVector, n: i32) callconv(.C) bool;
    pub extern fn ImColor_HSV(pOut: *Color, h: f32, s: f32, v: f32, a: f32) callconv(.C) void;
    pub extern fn ImColor_ImColor_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImColor_ImColor_Float(self: ?*anyopaque, r: f32, g: f32, b: f32, a: f32) callconv(.C) void;
    pub extern fn ImColor_ImColor_Vec4(self: ?*anyopaque, col: *const Vec4) callconv(.C) void;
    pub extern fn ImColor_ImColor_Int(self: ?*anyopaque, r: i32, g: i32, b: i32, a: i32) callconv(.C) void;
    pub extern fn ImColor_ImColor_U32(self: ?*anyopaque, rgba: u32) callconv(.C) void;
    pub extern fn ImColor_SetHSV(self: *Color, h: f32, s: f32, v: f32, a: f32) callconv(.C) void;
    pub extern fn ImColor_destroy(self: *Color) callconv(.C) void;
    pub extern fn ImDrawCmd_GetTexID(self: *const DrawCmd) callconv(.C) TextureID;
    pub extern fn ImDrawCmd_ImDrawCmd(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImDrawCmd_destroy(self: *DrawCmd) callconv(.C) void;
    pub extern fn ImDrawDataBuilder_Clear(self: *DrawDataBuilder) callconv(.C) void;
    pub extern fn ImDrawDataBuilder_ClearFreeMemory(self: *DrawDataBuilder) callconv(.C) void;
    pub extern fn ImDrawDataBuilder_FlattenIntoSingleLayer(self: *DrawDataBuilder) callconv(.C) void;
    pub extern fn ImDrawDataBuilder_GetDrawListCount(self: *const DrawDataBuilder) callconv(.C) i32;
    pub extern fn ImDrawData_Clear(self: *DrawData) callconv(.C) void;
    pub extern fn ImDrawData_DeIndexAllBuffers(self: *DrawData) callconv(.C) void;
    pub extern fn ImDrawData_ImDrawData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImDrawData_ScaleClipRects(self: *DrawData, fb_scale: *const Vec2) callconv(.C) void;
    pub extern fn ImDrawData_destroy(self: *DrawData) callconv(.C) void;
    pub extern fn ImDrawListSharedData_ImDrawListSharedData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImDrawListSharedData_SetCircleTessellationMaxError(self: *DrawListSharedData, max_error: f32) callconv(.C) void;
    pub extern fn ImDrawListSharedData_destroy(self: *DrawListSharedData) callconv(.C) void;
    pub extern fn ImDrawListSplitter_Clear(self: *DrawListSplitter) callconv(.C) void;
    pub extern fn ImDrawListSplitter_ClearFreeMemory(self: *DrawListSplitter) callconv(.C) void;
    pub extern fn ImDrawListSplitter_ImDrawListSplitter(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImDrawListSplitter_Merge(self: *DrawListSplitter, draw_list: ?*DrawList) callconv(.C) void;
    pub extern fn ImDrawListSplitter_SetCurrentChannel(self: *DrawListSplitter, draw_list: ?*DrawList, channel_idx: i32) callconv(.C) void;
    pub extern fn ImDrawListSplitter_Split(self: *DrawListSplitter, draw_list: ?*DrawList, count: i32) callconv(.C) void;
    pub extern fn ImDrawListSplitter_destroy(self: *DrawListSplitter) callconv(.C) void;
    pub extern fn ImDrawList_AddBezierCubic(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, p4: *const Vec2, col: u32, thickness: f32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_AddBezierQuadratic(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, col: u32, thickness: f32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_AddCallback(self: *DrawList, callback: DrawCallback, callback_data: ?*anyopaque) callconv(.C) void;
    pub extern fn ImDrawList_AddCircle(self: *DrawList, center: *const Vec2, radius: f32, col: u32, num_segments: i32, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddCircleFilled(self: *DrawList, center: *const Vec2, radius: f32, col: u32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_AddConvexPolyFilled(self: *DrawList, points: ?[*]const Vec2, num_points: i32, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_AddDrawCmd(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_AddImage(self: *DrawList, user_texture_id: TextureID, p_min: *const Vec2, p_max: *const Vec2, uv_min: *const Vec2, uv_max: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_AddImageQuad(self: *DrawList, user_texture_id: TextureID, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, p4: *const Vec2, uv1: *const Vec2, uv2: *const Vec2, uv3: *const Vec2, uv4: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_AddImageRounded(self: *DrawList, user_texture_id: TextureID, p_min: *const Vec2, p_max: *const Vec2, uv_min: *const Vec2, uv_max: *const Vec2, col: u32, rounding: f32, flags: DrawFlagsInt) callconv(.C) void;
    pub extern fn ImDrawList_AddLine(self: *DrawList, p1: *const Vec2, p2: *const Vec2, col: u32, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddNgon(self: *DrawList, center: *const Vec2, radius: f32, col: u32, num_segments: i32, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddNgonFilled(self: *DrawList, center: *const Vec2, radius: f32, col: u32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_AddPolyline(self: *DrawList, points: ?[*]const Vec2, num_points: i32, col: u32, flags: DrawFlagsInt, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddQuad(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, p4: *const Vec2, col: u32, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddQuadFilled(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, p4: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_AddRect(self: *DrawList, p_min: *const Vec2, p_max: *const Vec2, col: u32, rounding: f32, flags: DrawFlagsInt, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddRectFilled(self: *DrawList, p_min: *const Vec2, p_max: *const Vec2, col: u32, rounding: f32, flags: DrawFlagsInt) callconv(.C) void;
    pub extern fn ImDrawList_AddRectFilledMultiColor(self: *DrawList, p_min: *const Vec2, p_max: *const Vec2, col_upr_left: u32, col_upr_right: u32, col_bot_right: u32, col_bot_left: u32) callconv(.C) void;
    pub extern fn ImDrawList_AddText_Vec2(self: *DrawList, pos: *const Vec2, col: u32, text_begin: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) void;
    pub extern fn ImDrawList_AddText_FontPtr(self: *DrawList, font: ?*const Font, font_size: f32, pos: *const Vec2, col: u32, text_begin: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32, cpu_fine_clip_rect: ?*const Vec4) callconv(.C) void;
    pub extern fn ImDrawList_AddTriangle(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, col: u32, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_AddTriangleFilled(self: *DrawList, p1: *const Vec2, p2: *const Vec2, p3: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_ChannelsMerge(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_ChannelsSetCurrent(self: *DrawList, n: i32) callconv(.C) void;
    pub extern fn ImDrawList_ChannelsSplit(self: *DrawList, count: i32) callconv(.C) void;
    pub extern fn ImDrawList_CloneOutput(self: *const DrawList) callconv(.C) ?*DrawList;
    pub extern fn ImDrawList_GetClipRectMax(pOut: *Vec2, self: *const DrawList) callconv(.C) void;
    pub extern fn ImDrawList_GetClipRectMin(pOut: *Vec2, self: *const DrawList) callconv(.C) void;
    pub extern fn ImDrawList_ImDrawList(self: ?*anyopaque, shared_data: ?*const DrawListSharedData) callconv(.C) void;
    pub extern fn ImDrawList_PathArcTo(self: *DrawList, center: *const Vec2, radius: f32, a_min: f32, a_max: f32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_PathArcToFast(self: *DrawList, center: *const Vec2, radius: f32, a_min_of_12: i32, a_max_of_12: i32) callconv(.C) void;
    pub extern fn ImDrawList_PathBezierCubicCurveTo(self: *DrawList, p2: *const Vec2, p3: *const Vec2, p4: *const Vec2, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_PathBezierQuadraticCurveTo(self: *DrawList, p2: *const Vec2, p3: *const Vec2, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList_PathClear(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_PathFillConvex(self: *DrawList, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PathLineTo(self: *DrawList, pos: *const Vec2) callconv(.C) void;
    pub extern fn ImDrawList_PathLineToMergeDuplicate(self: *DrawList, pos: *const Vec2) callconv(.C) void;
    pub extern fn ImDrawList_PathRect(self: *DrawList, rect_min: *const Vec2, rect_max: *const Vec2, rounding: f32, flags: DrawFlagsInt) callconv(.C) void;
    pub extern fn ImDrawList_PathStroke(self: *DrawList, col: u32, flags: DrawFlagsInt, thickness: f32) callconv(.C) void;
    pub extern fn ImDrawList_PopClipRect(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_PopTextureID(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_PrimQuadUV(self: *DrawList, a: *const Vec2, b: *const Vec2, c: *const Vec2, d: *const Vec2, uv_a: *const Vec2, uv_b: *const Vec2, uv_c: *const Vec2, uv_d: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PrimRect(self: *DrawList, a: *const Vec2, b: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PrimRectUV(self: *DrawList, a: *const Vec2, b: *const Vec2, uv_a: *const Vec2, uv_b: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PrimReserve(self: *DrawList, idx_count: i32, vtx_count: i32) callconv(.C) void;
    pub extern fn ImDrawList_PrimUnreserve(self: *DrawList, idx_count: i32, vtx_count: i32) callconv(.C) void;
    pub extern fn ImDrawList_PrimVtx(self: *DrawList, pos: *const Vec2, uv: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PrimWriteIdx(self: *DrawList, idx: DrawIdx) callconv(.C) void;
    pub extern fn ImDrawList_PrimWriteVtx(self: *DrawList, pos: *const Vec2, uv: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn ImDrawList_PushClipRect(self: *DrawList, clip_rect_min: *const Vec2, clip_rect_max: *const Vec2, intersect_with_current_clip_rect: bool) callconv(.C) void;
    pub extern fn ImDrawList_PushClipRectFullScreen(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_PushTextureID(self: *DrawList, texture_id: TextureID) callconv(.C) void;
    pub extern fn ImDrawList__CalcCircleAutoSegmentCount(self: *const DrawList, radius: f32) callconv(.C) i32;
    pub extern fn ImDrawList__ClearFreeMemory(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__OnChangedClipRect(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__OnChangedTextureID(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__OnChangedVtxOffset(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__PathArcToFastEx(self: *DrawList, center: *const Vec2, radius: f32, a_min_sample: i32, a_max_sample: i32, a_step: i32) callconv(.C) void;
    pub extern fn ImDrawList__PathArcToN(self: *DrawList, center: *const Vec2, radius: f32, a_min: f32, a_max: f32, num_segments: i32) callconv(.C) void;
    pub extern fn ImDrawList__PopUnusedDrawCmd(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__ResetForNewFrame(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList__TryMergeDrawCmds(self: *DrawList) callconv(.C) void;
    pub extern fn ImDrawList_destroy(self: *DrawList) callconv(.C) void;
    pub extern fn ImFontAtlasCustomRect_ImFontAtlasCustomRect(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImFontAtlasCustomRect_IsPacked(self: *const FontAtlasCustomRect) callconv(.C) bool;
    pub extern fn ImFontAtlasCustomRect_destroy(self: *FontAtlasCustomRect) callconv(.C) void;
    pub extern fn ImFontAtlas_AddCustomRectFontGlyph(self: *FontAtlas, font: ?*Font, id: Wchar, width: i32, height: i32, advance_x: f32, offset: *const Vec2) callconv(.C) i32;
    pub extern fn ImFontAtlas_AddCustomRectRegular(self: *FontAtlas, width: i32, height: i32) callconv(.C) i32;
    pub extern fn ImFontAtlas_AddFont(self: *FontAtlas, font_cfg: ?*const FontConfig) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_AddFontDefault(self: *FontAtlas, font_cfg: ?*const FontConfig) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_AddFontFromFileTTF(self: *FontAtlas, filename: ?[*:0]const u8, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_AddFontFromMemoryCompressedBase85TTF(self: *FontAtlas, compressed_font_data_base85: ?[*]const u8, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_AddFontFromMemoryCompressedTTF(self: *FontAtlas, compressed_font_data: ?*const anyopaque, compressed_font_size: i32, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_AddFontFromMemoryTTF(self: *FontAtlas, font_data: ?*anyopaque, font_size: i32, size_pixels: f32, font_cfg: ?*const FontConfig, glyph_ranges: ?[*:0]const Wchar) callconv(.C) ?*Font;
    pub extern fn ImFontAtlas_Build(self: *FontAtlas) callconv(.C) bool;
    pub extern fn ImFontAtlas_CalcCustomRectUV(self: *const FontAtlas, rect: ?*const FontAtlasCustomRect, out_uv_min: ?*Vec2, out_uv_max: ?*Vec2) callconv(.C) void;
    pub extern fn ImFontAtlas_Clear(self: *FontAtlas) callconv(.C) void;
    pub extern fn ImFontAtlas_ClearFonts(self: *FontAtlas) callconv(.C) void;
    pub extern fn ImFontAtlas_ClearInputData(self: *FontAtlas) callconv(.C) void;
    pub extern fn ImFontAtlas_ClearTexData(self: *FontAtlas) callconv(.C) void;
    pub extern fn ImFontAtlas_GetCustomRectByIndex(self: *FontAtlas, index: i32) callconv(.C) ?*FontAtlasCustomRect;
    pub extern fn ImFontAtlas_GetGlyphRangesChineseFull(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesChineseSimplifiedCommon(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesCyrillic(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesDefault(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesJapanese(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesKorean(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesThai(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetGlyphRangesVietnamese(self: *FontAtlas) callconv(.C) ?*const Wchar;
    pub extern fn ImFontAtlas_GetMouseCursorTexData(self: *FontAtlas, cursor: MouseCursor, out_offset: ?*Vec2, out_size: ?*Vec2, out_uv_border: *[2]Vec2, out_uv_fill: *[2]Vec2) callconv(.C) bool;
    pub extern fn ImFontAtlas_GetTexDataAsAlpha8(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32, out_bytes_per_pixel: ?*i32) callconv(.C) void;
    pub extern fn ImFontAtlas_GetTexDataAsRGBA32(self: *FontAtlas, out_pixels: *?[*]u8, out_width: *i32, out_height: *i32, out_bytes_per_pixel: ?*i32) callconv(.C) void;
    pub extern fn ImFontAtlas_ImFontAtlas(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImFontAtlas_IsBuilt(self: *const FontAtlas) callconv(.C) bool;
    pub extern fn ImFontAtlas_SetTexID(self: *FontAtlas, id: TextureID) callconv(.C) void;
    pub extern fn ImFontAtlas_destroy(self: *FontAtlas) callconv(.C) void;
    pub extern fn ImFontConfig_ImFontConfig(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImFontConfig_destroy(self: *FontConfig) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_AddChar(self: *FontGlyphRangesBuilder, c: Wchar) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_AddRanges(self: *FontGlyphRangesBuilder, ranges: ?[*:0]const Wchar) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_AddText(self: *FontGlyphRangesBuilder, text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_BuildRanges(self: *FontGlyphRangesBuilder, out_ranges: *Vector(Wchar)) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_Clear(self: *FontGlyphRangesBuilder) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_GetBit(self: *const FontGlyphRangesBuilder, n: usize) callconv(.C) bool;
    pub extern fn ImFontGlyphRangesBuilder_ImFontGlyphRangesBuilder(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_SetBit(self: *FontGlyphRangesBuilder, n: usize) callconv(.C) void;
    pub extern fn ImFontGlyphRangesBuilder_destroy(self: *FontGlyphRangesBuilder) callconv(.C) void;
    pub extern fn ImFont_AddGlyph(self: *Font, src_cfg: ?*const FontConfig, c: Wchar, x0: f32, y0: f32, x1: f32, y1: f32, u0: f32, v0: f32, u1: f32, v1: f32, advance_x: f32) callconv(.C) void;
    pub extern fn ImFont_AddRemapChar(self: *Font, dst: Wchar, src: Wchar, overwrite_dst: bool) callconv(.C) void;
    pub extern fn ImFont_BuildLookupTable(self: *Font) callconv(.C) void;
    pub extern fn ImFont_CalcTextSizeA(pOut: *Vec2, self: *const Font, size: f32, max_width: f32, wrap_width: f32, text_begin: ?[*]const u8, text_end: ?[*]const u8, remaining: ?*?[*:0]const u8) callconv(.C) void;
    pub extern fn ImFont_CalcWordWrapPositionA(self: *const Font, scale: f32, text: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32) callconv(.C) ?[*]const u8;
    pub extern fn ImFont_ClearOutputData(self: *Font) callconv(.C) void;
    pub extern fn ImFont_FindGlyph(self: *const Font, c: Wchar) callconv(.C) ?*const FontGlyph;
    pub extern fn ImFont_FindGlyphNoFallback(self: *const Font, c: Wchar) callconv(.C) ?*const FontGlyph;
    pub extern fn ImFont_GetCharAdvance(self: *const Font, c: Wchar) callconv(.C) f32;
    pub extern fn ImFont_GetDebugName(self: *const Font) callconv(.C) ?[*:0]const u8;
    pub extern fn ImFont_GrowIndex(self: *Font, new_size: i32) callconv(.C) void;
    pub extern fn ImFont_ImFont(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImFont_IsGlyphRangeUnused(self: *Font, c_begin: u32, c_last: u32) callconv(.C) bool;
    pub extern fn ImFont_IsLoaded(self: *const Font) callconv(.C) bool;
    pub extern fn ImFont_RenderChar(self: *const Font, draw_list: ?*DrawList, size: f32, pos: *const Vec2, col: u32, c: Wchar) callconv(.C) void;
    pub extern fn ImFont_RenderText(self: *const Font, draw_list: ?*DrawList, size: f32, pos: *const Vec2, col: u32, clip_rect: *const Vec4, text_begin: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32, cpu_fine_clip: bool) callconv(.C) void;
    pub extern fn ImFont_SetGlyphVisible(self: *Font, c: Wchar, visible: bool) callconv(.C) void;
    pub extern fn ImFont_destroy(self: *Font) callconv(.C) void;
    pub extern fn ImGuiComboPreviewData_ImGuiComboPreviewData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiComboPreviewData_destroy(self: *ComboPreviewData) callconv(.C) void;
    pub extern fn ImGuiContextHook_ImGuiContextHook(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiContextHook_destroy(self: *ContextHook) callconv(.C) void;
    pub extern fn ImGuiContext_ImGuiContext(self: ?*anyopaque, shared_font_atlas: ?*FontAtlas) callconv(.C) void;
    pub extern fn ImGuiContext_destroy(self: *Context) callconv(.C) void;
    pub extern fn ImGuiIO_AddFocusEvent(self: *IO, focused: bool) callconv(.C) void;
    pub extern fn ImGuiIO_AddInputCharacter(self: *IO, c: u32) callconv(.C) void;
    pub extern fn ImGuiIO_AddInputCharacterUTF16(self: *IO, c: Wchar16) callconv(.C) void;
    pub extern fn ImGuiIO_AddInputCharactersUTF8(self: *IO, str: ?[*:0]const u8) callconv(.C) void;
    pub extern fn ImGuiIO_AddKeyAnalogEvent(self: *IO, key: Key, down: bool, v: f32) callconv(.C) void;
    pub extern fn ImGuiIO_AddKeyEvent(self: *IO, key: Key, down: bool) callconv(.C) void;
    pub extern fn ImGuiIO_AddMouseButtonEvent(self: *IO, button: i32, down: bool) callconv(.C) void;
    pub extern fn ImGuiIO_AddMousePosEvent(self: *IO, x: f32, y: f32) callconv(.C) void;
    pub extern fn ImGuiIO_AddMouseWheelEvent(self: *IO, wh_x: f32, wh_y: f32) callconv(.C) void;
    pub extern fn ImGuiIO_ClearInputCharacters(self: *IO) callconv(.C) void;
    pub extern fn ImGuiIO_ClearInputKeys(self: *IO) callconv(.C) void;
    pub extern fn ImGuiIO_ImGuiIO(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiIO_SetAppAcceptingEvents(self: *IO, accepting_events: bool) callconv(.C) void;
    pub extern fn ImGuiIO_SetKeyEventNativeData(self: *IO, key: Key, native_keycode: i32, native_scancode: i32, native_legacy_index: i32) callconv(.C) void;
    pub extern fn ImGuiIO_destroy(self: *IO) callconv(.C) void;
    pub extern fn ImGuiInputEvent_ImGuiInputEvent(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiInputEvent_destroy(self: *InputEvent) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_ClearSelection(self: *InputTextCallbackData) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_DeleteChars(self: *InputTextCallbackData, pos: i32, bytes_count: i32) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_HasSelection(self: *const InputTextCallbackData) callconv(.C) bool;
    pub extern fn ImGuiInputTextCallbackData_ImGuiInputTextCallbackData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_InsertChars(self: *InputTextCallbackData, pos: i32, text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_SelectAll(self: *InputTextCallbackData) callconv(.C) void;
    pub extern fn ImGuiInputTextCallbackData_destroy(self: *InputTextCallbackData) callconv(.C) void;
    pub extern fn ImGuiInputTextState_ClearFreeMemory(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_ClearSelection(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_ClearText(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_CursorAnimReset(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_CursorClamp(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_GetCursorPos(self: *const InputTextState) callconv(.C) i32;
    pub extern fn ImGuiInputTextState_GetRedoAvailCount(self: *const InputTextState) callconv(.C) i32;
    pub extern fn ImGuiInputTextState_GetSelectionEnd(self: *const InputTextState) callconv(.C) i32;
    pub extern fn ImGuiInputTextState_GetSelectionStart(self: *const InputTextState) callconv(.C) i32;
    pub extern fn ImGuiInputTextState_GetUndoAvailCount(self: *const InputTextState) callconv(.C) i32;
    pub extern fn ImGuiInputTextState_HasSelection(self: *const InputTextState) callconv(.C) bool;
    pub extern fn ImGuiInputTextState_ImGuiInputTextState(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiInputTextState_OnKeyPressed(self: *InputTextState, key: i32) callconv(.C) void;
    pub extern fn ImGuiInputTextState_SelectAll(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiInputTextState_destroy(self: *InputTextState) callconv(.C) void;
    pub extern fn ImGuiLastItemData_ImGuiLastItemData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiLastItemData_destroy(self: *LastItemData) callconv(.C) void;
    pub extern fn ImGuiListClipperData_ImGuiListClipperData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiListClipperData_Reset(self: *ListClipperData, clipper: ?*ListClipper) callconv(.C) void;
    pub extern fn ImGuiListClipperData_destroy(self: *ListClipperData) callconv(.C) void;
    pub extern fn ImGuiListClipperRange_FromIndices(min: i32, max: i32) callconv(.C) ListClipperRange;
    pub extern fn ImGuiListClipperRange_FromPositions(y1: f32, y2: f32, off_min: i32, off_max: i32) callconv(.C) ListClipperRange;
    pub extern fn ImGuiListClipper_Begin(self: *ListClipper, items_count: i32, items_height: f32) callconv(.C) void;
    pub extern fn ImGuiListClipper_End(self: *ListClipper) callconv(.C) void;
    pub extern fn ImGuiListClipper_ForceDisplayRangeByIndices(self: *ListClipper, item_min: i32, item_max: i32) callconv(.C) void;
    pub extern fn ImGuiListClipper_ImGuiListClipper(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiListClipper_Step(self: *ListClipper) callconv(.C) bool;
    pub extern fn ImGuiListClipper_destroy(self: *ListClipper) callconv(.C) void;
    pub extern fn ImGuiMenuColumns_CalcNextTotalWidth(self: *MenuColumns, update_offsets: bool) callconv(.C) void;
    pub extern fn ImGuiMenuColumns_DeclColumns(self: *MenuColumns, w_icon: f32, w_label: f32, w_shortcut: f32, w_mark: f32) callconv(.C) f32;
    pub extern fn ImGuiMenuColumns_ImGuiMenuColumns(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiMenuColumns_Update(self: *MenuColumns, spacing: f32, window_reappearing: bool) callconv(.C) void;
    pub extern fn ImGuiMenuColumns_destroy(self: *MenuColumns) callconv(.C) void;
    pub extern fn ImGuiMetricsConfig_ImGuiMetricsConfig(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiMetricsConfig_destroy(self: *MetricsConfig) callconv(.C) void;
    pub extern fn ImGuiNavItemData_Clear(self: *NavItemData) callconv(.C) void;
    pub extern fn ImGuiNavItemData_ImGuiNavItemData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiNavItemData_destroy(self: *NavItemData) callconv(.C) void;
    pub extern fn ImGuiNextItemData_ClearFlags(self: *NextItemData) callconv(.C) void;
    pub extern fn ImGuiNextItemData_ImGuiNextItemData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiNextItemData_destroy(self: *NextItemData) callconv(.C) void;
    pub extern fn ImGuiNextWindowData_ClearFlags(self: *NextWindowData) callconv(.C) void;
    pub extern fn ImGuiNextWindowData_ImGuiNextWindowData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiNextWindowData_destroy(self: *NextWindowData) callconv(.C) void;
    pub extern fn ImGuiOldColumnData_ImGuiOldColumnData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiOldColumnData_destroy(self: *OldColumnData) callconv(.C) void;
    pub extern fn ImGuiOldColumns_ImGuiOldColumns(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiOldColumns_destroy(self: *OldColumns) callconv(.C) void;
    pub extern fn ImGuiOnceUponAFrame_ImGuiOnceUponAFrame(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiOnceUponAFrame_destroy(self: *OnceUponAFrame) callconv(.C) void;
    pub extern fn ImGuiPayload_Clear(self: *Payload) callconv(.C) void;
    pub extern fn ImGuiPayload_ImGuiPayload(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiPayload_IsDataType(self: *const Payload, kind: ?[*:0]const u8) callconv(.C) bool;
    pub extern fn ImGuiPayload_IsDelivery(self: *const Payload) callconv(.C) bool;
    pub extern fn ImGuiPayload_IsPreview(self: *const Payload) callconv(.C) bool;
    pub extern fn ImGuiPayload_destroy(self: *Payload) callconv(.C) void;
    pub extern fn ImGuiPlatformImeData_ImGuiPlatformImeData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiPlatformImeData_destroy(self: *PlatformImeData) callconv(.C) void;
    pub extern fn ImGuiPopupData_ImGuiPopupData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiPopupData_destroy(self: *PopupData) callconv(.C) void;
    pub extern fn ImGuiPtrOrIndex_ImGuiPtrOrIndex_Ptr(self: ?*anyopaque, ptr: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiPtrOrIndex_ImGuiPtrOrIndex_Int(self: ?*anyopaque, index: i32) callconv(.C) void;
    pub extern fn ImGuiPtrOrIndex_destroy(self: *PtrOrIndex) callconv(.C) void;
    pub extern fn ImGuiSettingsHandler_ImGuiSettingsHandler(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiSettingsHandler_destroy(self: *SettingsHandler) callconv(.C) void;
    pub extern fn ImGuiStackLevelInfo_ImGuiStackLevelInfo(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStackLevelInfo_destroy(self: *StackLevelInfo) callconv(.C) void;
    pub extern fn ImGuiStackSizes_CompareWithCurrentState(self: *StackSizes) callconv(.C) void;
    pub extern fn ImGuiStackSizes_ImGuiStackSizes(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStackSizes_SetToCurrentState(self: *StackSizes) callconv(.C) void;
    pub extern fn ImGuiStackSizes_destroy(self: *StackSizes) callconv(.C) void;
    pub extern fn ImGuiStackTool_ImGuiStackTool(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStackTool_destroy(self: *StackTool) callconv(.C) void;
    pub extern fn ImGuiStoragePair_ImGuiStoragePair_Int(self: ?*anyopaque, _key: ID, _val_i: i32) callconv(.C) void;
    pub extern fn ImGuiStoragePair_ImGuiStoragePair_Float(self: ?*anyopaque, _key: ID, _val_f: f32) callconv(.C) void;
    pub extern fn ImGuiStoragePair_ImGuiStoragePair_Ptr(self: ?*anyopaque, _key: ID, _val_p: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStoragePair_destroy(self: *StoragePair) callconv(.C) void;
    pub extern fn ImGuiStorage_BuildSortByKey(self: *Storage) callconv(.C) void;
    pub extern fn ImGuiStorage_Clear(self: *Storage) callconv(.C) void;
    pub extern fn ImGuiStorage_GetBool(self: *const Storage, key: ID, default_val: bool) callconv(.C) bool;
    pub extern fn ImGuiStorage_GetBoolRef(self: *Storage, key: ID, default_val: bool) callconv(.C) ?*bool;
    pub extern fn ImGuiStorage_GetFloat(self: *const Storage, key: ID, default_val: f32) callconv(.C) f32;
    pub extern fn ImGuiStorage_GetFloatRef(self: *Storage, key: ID, default_val: f32) callconv(.C) ?*f32;
    pub extern fn ImGuiStorage_GetInt(self: *const Storage, key: ID, default_val: i32) callconv(.C) i32;
    pub extern fn ImGuiStorage_GetIntRef(self: *Storage, key: ID, default_val: i32) callconv(.C) ?*i32;
    pub extern fn ImGuiStorage_GetVoidPtr(self: *const Storage, key: ID) callconv(.C) ?*anyopaque;
    pub extern fn ImGuiStorage_GetVoidPtrRef(self: *Storage, key: ID, default_val: ?*anyopaque) callconv(.C) ?*?*anyopaque;
    pub extern fn ImGuiStorage_SetAllInt(self: *Storage, val: i32) callconv(.C) void;
    pub extern fn ImGuiStorage_SetBool(self: *Storage, key: ID, val: bool) callconv(.C) void;
    pub extern fn ImGuiStorage_SetFloat(self: *Storage, key: ID, val: f32) callconv(.C) void;
    pub extern fn ImGuiStorage_SetInt(self: *Storage, key: ID, val: i32) callconv(.C) void;
    pub extern fn ImGuiStorage_SetVoidPtr(self: *Storage, key: ID, val: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStyleMod_ImGuiStyleMod_Int(self: ?*anyopaque, idx: StyleVar, v: i32) callconv(.C) void;
    pub extern fn ImGuiStyleMod_ImGuiStyleMod_Float(self: ?*anyopaque, idx: StyleVar, v: f32) callconv(.C) void;
    pub extern fn ImGuiStyleMod_ImGuiStyleMod_Vec2(self: ?*anyopaque, idx: StyleVar, v: *const Vec2) callconv(.C) void;
    pub extern fn ImGuiStyleMod_destroy(self: *StyleMod) callconv(.C) void;
    pub extern fn ImGuiStyle_ImGuiStyle(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiStyle_ScaleAllSizes(self: *Style, scale_factor: f32) callconv(.C) void;
    pub extern fn ImGuiStyle_destroy(self: *Style) callconv(.C) void;
    pub extern fn ImGuiTabBar_GetTabName(self: *const TabBar, tab: ?*const TabItem) callconv(.C) ?[*:0]const u8;
    pub extern fn ImGuiTabBar_GetTabOrder(self: *const TabBar, tab: ?*const TabItem) callconv(.C) i32;
    pub extern fn ImGuiTabBar_ImGuiTabBar(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTabBar_destroy(self: *TabBar) callconv(.C) void;
    pub extern fn ImGuiTabItem_ImGuiTabItem(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTabItem_destroy(self: *TabItem) callconv(.C) void;
    pub extern fn ImGuiTableColumnSettings_ImGuiTableColumnSettings(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableColumnSettings_destroy(self: *TableColumnSettings) callconv(.C) void;
    pub extern fn ImGuiTableColumnSortSpecs_ImGuiTableColumnSortSpecs(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableColumnSortSpecs_destroy(self: *TableColumnSortSpecs) callconv(.C) void;
    pub extern fn ImGuiTableColumn_ImGuiTableColumn(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableColumn_destroy(self: *TableColumn) callconv(.C) void;
    pub extern fn ImGuiTableInstanceData_ImGuiTableInstanceData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableInstanceData_destroy(self: *TableInstanceData) callconv(.C) void;
    pub extern fn ImGuiTableSettings_GetColumnSettings(self: *TableSettings) callconv(.C) ?*TableColumnSettings;
    pub extern fn ImGuiTableSettings_ImGuiTableSettings(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableSettings_destroy(self: *TableSettings) callconv(.C) void;
    pub extern fn ImGuiTableSortSpecs_ImGuiTableSortSpecs(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableSortSpecs_destroy(self: *TableSortSpecs) callconv(.C) void;
    pub extern fn ImGuiTableTempData_ImGuiTableTempData(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTableTempData_destroy(self: *TableTempData) callconv(.C) void;
    pub extern fn ImGuiTable_ImGuiTable(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTable_destroy(self: *Table) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_ImGuiTextBuffer(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_append(self: *TextBuffer, str: ?[*]const u8, str_end: ?[*]const u8) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_appendf(self: *TextBuffer, fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_begin(self: *const TextBuffer) callconv(.C) [*]const u8;
    pub extern fn ImGuiTextBuffer_c_str(self: *const TextBuffer) callconv(.C) [*:0]const u8;
    pub extern fn ImGuiTextBuffer_clear(self: *TextBuffer) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_destroy(self: *TextBuffer) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_empty(self: *const TextBuffer) callconv(.C) bool;
    pub extern fn ImGuiTextBuffer_end(self: *const TextBuffer) callconv(.C) [*]const u8;
    pub extern fn ImGuiTextBuffer_reserve(self: *TextBuffer, capacity: i32) callconv(.C) void;
    pub extern fn ImGuiTextBuffer_size(self: *const TextBuffer) callconv(.C) i32;
    pub extern fn ImGuiTextFilter_Build(self: *TextFilter) callconv(.C) void;
    pub extern fn ImGuiTextFilter_Clear(self: *TextFilter) callconv(.C) void;
    pub extern fn ImGuiTextFilter_Draw(self: *TextFilter, label: ?[*:0]const u8, width: f32) callconv(.C) bool;
    pub extern fn ImGuiTextFilter_ImGuiTextFilter(self: ?*anyopaque, default_filter: ?[*:0]const u8) callconv(.C) void;
    pub extern fn ImGuiTextFilter_IsActive(self: *const TextFilter) callconv(.C) bool;
    pub extern fn ImGuiTextFilter_PassFilter(self: *const TextFilter, text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) bool;
    pub extern fn ImGuiTextFilter_destroy(self: *TextFilter) callconv(.C) void;
    pub extern fn ImGuiTextRange_ImGuiTextRange_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiTextRange_ImGuiTextRange_Str(self: ?*anyopaque, _b: ?[*]const u8, _e: ?[*]const u8) callconv(.C) void;
    pub extern fn ImGuiTextRange_destroy(self: *TextRange) callconv(.C) void;
    pub extern fn ImGuiTextRange_empty(self: *const TextRange) callconv(.C) bool;
    pub extern fn ImGuiTextRange_split(self: *const TextRange, separator: u8, out: ?*Vector(TextRange)) callconv(.C) void;
    pub extern fn ImGuiViewportP_CalcWorkRectPos(pOut: *Vec2, self: *const ViewportP, off_min: ?*const Vec2) callconv(.C) void;
    pub extern fn ImGuiViewportP_CalcWorkRectSize(pOut: *Vec2, self: *const ViewportP, off_min: ?*const Vec2, off_max: ?*const Vec2) callconv(.C) void;
    pub extern fn ImGuiViewportP_GetBuildWorkRect(pOut: *Rect, self: *const ViewportP) callconv(.C) void;
    pub extern fn ImGuiViewportP_GetMainRect(pOut: *Rect, self: *const ViewportP) callconv(.C) void;
    pub extern fn ImGuiViewportP_GetWorkRect(pOut: *Rect, self: *const ViewportP) callconv(.C) void;
    pub extern fn ImGuiViewportP_ImGuiViewportP(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiViewportP_UpdateWorkRect(self: *ViewportP) callconv(.C) void;
    pub extern fn ImGuiViewportP_destroy(self: *ViewportP) callconv(.C) void;
    pub extern fn ImGuiViewport_GetCenter(pOut: *Vec2, self: *const Viewport) callconv(.C) void;
    pub extern fn ImGuiViewport_GetWorkCenter(pOut: *Vec2, self: *const Viewport) callconv(.C) void;
    pub extern fn ImGuiViewport_ImGuiViewport(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiViewport_destroy(self: *Viewport) callconv(.C) void;
    pub extern fn ImGuiWindowSettings_GetName(self: *WindowSettings) callconv(.C) ?[*:0]u8;
    pub extern fn ImGuiWindowSettings_ImGuiWindowSettings(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImGuiWindowSettings_destroy(self: *WindowSettings) callconv(.C) void;
    pub extern fn ImGuiWindow_CalcFontSize(self: *const Window) callconv(.C) f32;
    pub extern fn ImGuiWindow_GetID_Str(self: *Window, str: ?[*:0]const u8, str_end: ?[*]const u8) callconv(.C) ID;
    pub extern fn ImGuiWindow_GetID_Ptr(self: *Window, ptr: ?*const anyopaque) callconv(.C) ID;
    pub extern fn ImGuiWindow_GetID_Int(self: *Window, n: i32) callconv(.C) ID;
    pub extern fn ImGuiWindow_GetIDFromRectangle(self: *Window, r_abs: *const Rect) callconv(.C) ID;
    pub extern fn ImGuiWindow_ImGuiWindow(self: ?*anyopaque, context: ?*Context, name: ?[*:0]const u8) callconv(.C) void;
    pub extern fn ImGuiWindow_MenuBarHeight(self: *const Window) callconv(.C) f32;
    pub extern fn ImGuiWindow_MenuBarRect(pOut: *Rect, self: *const Window) callconv(.C) void;
    pub extern fn ImGuiWindow_Rect(pOut: *Rect, self: *const Window) callconv(.C) void;
    pub extern fn ImGuiWindow_TitleBarHeight(self: *const Window) callconv(.C) f32;
    pub extern fn ImGuiWindow_TitleBarRect(pOut: *Rect, self: *const Window) callconv(.C) void;
    pub extern fn ImGuiWindow_destroy(self: *Window) callconv(.C) void;
    pub extern fn ImRect_Add_Vec2(self: *Rect, p: *const Vec2) callconv(.C) void;
    pub extern fn ImRect_Add_Rect(self: *Rect, r: *const Rect) callconv(.C) void;
    pub extern fn ImRect_ClipWith(self: *Rect, r: *const Rect) callconv(.C) void;
    pub extern fn ImRect_ClipWithFull(self: *Rect, r: *const Rect) callconv(.C) void;
    pub extern fn ImRect_Contains_Vec2(self: *const Rect, p: *const Vec2) callconv(.C) bool;
    pub extern fn ImRect_Contains_Rect(self: *const Rect, r: *const Rect) callconv(.C) bool;
    pub extern fn ImRect_Expand_Float(self: *Rect, amount: const f32) callconv(.C) void;
    pub extern fn ImRect_Expand_Vec2(self: *Rect, amount: *const Vec2) callconv(.C) void;
    pub extern fn ImRect_Floor(self: *Rect) callconv(.C) void;
    pub extern fn ImRect_GetArea(self: *const Rect) callconv(.C) f32;
    pub extern fn ImRect_GetBL(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetBR(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetCenter(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetHeight(self: *const Rect) callconv(.C) f32;
    pub extern fn ImRect_GetSize(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetTL(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetTR(pOut: *Vec2, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_GetWidth(self: *const Rect) callconv(.C) f32;
    pub extern fn ImRect_ImRect_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImRect_ImRect_Vec2(self: ?*anyopaque, min: *const Vec2, max: *const Vec2) callconv(.C) void;
    pub extern fn ImRect_ImRect_Vec4(self: ?*anyopaque, v: *const Vec4) callconv(.C) void;
    pub extern fn ImRect_ImRect_Float(self: ?*anyopaque, x1: f32, y1: f32, x2: f32, y2: f32) callconv(.C) void;
    pub extern fn ImRect_IsInverted(self: *const Rect) callconv(.C) bool;
    pub extern fn ImRect_Overlaps(self: *const Rect, r: *const Rect) callconv(.C) bool;
    pub extern fn ImRect_ToVec4(pOut: *Vec4, self: *const Rect) callconv(.C) void;
    pub extern fn ImRect_Translate(self: *Rect, d: *const Vec2) callconv(.C) void;
    pub extern fn ImRect_TranslateX(self: *Rect, dx: f32) callconv(.C) void;
    pub extern fn ImRect_TranslateY(self: *Rect, dy: f32) callconv(.C) void;
    pub extern fn ImRect_destroy(self: *Rect) callconv(.C) void;
    pub extern fn ImVec1_ImVec1_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImVec1_ImVec1_Float(self: ?*anyopaque, _x: f32) callconv(.C) void;
    pub extern fn ImVec1_destroy(self: *Vec1) callconv(.C) void;
    pub extern fn ImVec2_ImVec2_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImVec2_ImVec2_Float(self: ?*anyopaque, _x: f32, _y: f32) callconv(.C) void;
    pub extern fn ImVec2_destroy(self: *Vec2) callconv(.C) void;
    pub extern fn ImVec2ih_ImVec2ih_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImVec2ih_ImVec2ih_short(self: ?*anyopaque, _x: i16, _y: i16) callconv(.C) void;
    pub extern fn ImVec2ih_ImVec2ih_Vec2(self: ?*anyopaque, rhs: *const Vec2) callconv(.C) void;
    pub extern fn ImVec2ih_destroy(self: *Vec2ih) callconv(.C) void;
    pub extern fn ImVec4_ImVec4_Nil(self: ?*anyopaque) callconv(.C) void;
    pub extern fn ImVec4_ImVec4_Float(self: ?*anyopaque, _x: f32, _y: f32, _z: f32, _w: f32) callconv(.C) void;
    pub extern fn ImVec4_destroy(self: *Vec4) callconv(.C) void;
    pub extern fn igAcceptDragDropPayload(kind: ?[*:0]const u8, flags: DragDropFlagsInt) callconv(.C) ?*const Payload;
    pub extern fn igActivateItem(id: ID) callconv(.C) void;
    pub extern fn igAddContextHook(context: ?*Context, hook: ?*const ContextHook) callconv(.C) ID;
    pub extern fn igAddSettingsHandler(handler: ?*const SettingsHandler) callconv(.C) void;
    pub extern fn igAlignTextToFramePadding() callconv(.C) void;
    pub extern fn igArrowButton(str_id: ?[*:0]const u8, dir: Dir) callconv(.C) bool;
    pub extern fn igArrowButtonEx(str_id: ?[*:0]const u8, dir: Dir, size_arg: *const Vec2, flags: ButtonFlagsInt) callconv(.C) bool;
    pub extern fn igBegin(name: ?[*:0]const u8, p_open: ?*bool, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginChild_Str(str_id: ?[*:0]const u8, size: *const Vec2, border: bool, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginChild_ID(id: ID, size: *const Vec2, border: bool, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginChildEx(name: ?[*:0]const u8, id: ID, size_arg: *const Vec2, border: bool, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginChildFrame(id: ID, size: *const Vec2, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginColumns(str_id: ?[*:0]const u8, count: i32, flags: OldColumnFlagsInt) callconv(.C) void;
    pub extern fn igBeginCombo(label: ?[*:0]const u8, preview_value: ?[*:0]const u8, flags: ComboFlagsInt) callconv(.C) bool;
    pub extern fn igBeginComboPopup(popup_id: ID, bb: *const Rect, flags: ComboFlagsInt) callconv(.C) bool;
    pub extern fn igBeginComboPreview() callconv(.C) bool;
    pub extern fn igBeginDisabled(disabled: bool) callconv(.C) void;
    pub extern fn igBeginDragDropSource(flags: DragDropFlagsInt) callconv(.C) bool;
    pub extern fn igBeginDragDropTarget() callconv(.C) bool;
    pub extern fn igBeginDragDropTargetCustom(bb: *const Rect, id: ID) callconv(.C) bool;
    pub extern fn igBeginGroup() callconv(.C) void;
    pub extern fn igBeginListBox(label: ?[*:0]const u8, size: *const Vec2) callconv(.C) bool;
    pub extern fn igBeginMainMenuBar() callconv(.C) bool;
    pub extern fn igBeginMenu(label: ?[*:0]const u8, enabled: bool) callconv(.C) bool;
    pub extern fn igBeginMenuBar() callconv(.C) bool;
    pub extern fn igBeginMenuEx(label: ?[*:0]const u8, icon: [*c]const u8, enabled: bool) callconv(.C) bool;
    pub extern fn igBeginPopup(str_id: ?[*:0]const u8, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginPopupContextItem(str_id: ?[*:0]const u8, popup_flags: PopupFlagsInt) callconv(.C) bool;
    pub extern fn igBeginPopupContextVoid(str_id: ?[*:0]const u8, popup_flags: PopupFlagsInt) callconv(.C) bool;
    pub extern fn igBeginPopupContextWindow(str_id: ?[*:0]const u8, popup_flags: PopupFlagsInt) callconv(.C) bool;
    pub extern fn igBeginPopupEx(id: ID, extra_flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginPopupModal(name: ?[*:0]const u8, p_open: ?*bool, flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBeginTabBar(str_id: ?[*:0]const u8, flags: TabBarFlagsInt) callconv(.C) bool;
    pub extern fn igBeginTabBarEx(tab_bar: ?*TabBar, bb: *const Rect, flags: TabBarFlagsInt) callconv(.C) bool;
    pub extern fn igBeginTabItem(label: ?[*:0]const u8, p_open: ?*bool, flags: TabItemFlagsInt) callconv(.C) bool;
    pub extern fn igBeginTable(str_id: ?[*:0]const u8, column: i32, flags: TableFlagsInt, outer_size: *const Vec2, inner_width: f32) callconv(.C) bool;
    pub extern fn igBeginTableEx(name: ?[*:0]const u8, id: ID, columns_count: i32, flags: TableFlagsInt, outer_size: *const Vec2, inner_width: f32) callconv(.C) bool;
    pub extern fn igBeginTooltip() callconv(.C) void;
    pub extern fn igBeginTooltipEx(tooltip_flags: TooltipFlagsInt, extra_window_flags: WindowFlagsInt) callconv(.C) void;
    pub extern fn igBeginViewportSideBar(name: ?[*:0]const u8, viewport: ?*Viewport, dir: Dir, size: f32, window_flags: WindowFlagsInt) callconv(.C) bool;
    pub extern fn igBringWindowToDisplayBack(window: ?*Window) callconv(.C) void;
    pub extern fn igBringWindowToDisplayBehind(window: ?*Window, above_window: ?*Window) callconv(.C) void;
    pub extern fn igBringWindowToDisplayFront(window: ?*Window) callconv(.C) void;
    pub extern fn igBringWindowToFocusFront(window: ?*Window) callconv(.C) void;
    pub extern fn igBullet() callconv(.C) void;
    pub extern fn igBulletText(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igButton(label: ?[*:0]const u8, size: *const Vec2) callconv(.C) bool;
    pub extern fn igButtonBehavior(bb: *const Rect, id: ID, out_hovered: *bool, out_held: *bool, flags: ButtonFlagsInt) callconv(.C) bool;
    pub extern fn igButtonEx(label: ?[*:0]const u8, size_arg: *const Vec2, flags: ButtonFlagsInt) callconv(.C) bool;
    pub extern fn igCalcItemSize(pOut: *Vec2, size: ?*const Vec2, default_w: f32, default_h: f32) callconv(.C) void;
    pub extern fn igCalcItemWidth() callconv(.C) f32;
    pub extern fn igCalcTextSize(pOut: *Vec2, text: ?[*]const u8, text_end: ?[*]const u8, hide_text_after_double_hash: bool, wrap_width: f32) callconv(.C) void;
    pub extern fn igCalcTypematicRepeatAmount(t0: f32, t1: f32, repeat_delay: f32, repeat_rate: f32) callconv(.C) i32;
    pub extern fn igCalcWindowNextAutoFitSize(pOut: *Vec2, window: ?*Window) callconv(.C) void;
    pub extern fn igCalcWrapWidthForPos(pos: *const Vec2, wrap_pos_x: f32) callconv(.C) f32;
    pub extern fn igCallContextHooks(context: ?*Context, kind: ContextHookType) callconv(.C) void;
    pub extern fn igCheckbox(label: ?[*:0]const u8, v: *bool) callconv(.C) bool;
    pub extern fn igCheckboxFlags_IntPtr(label: ?[*:0]const u8, flags: *i32, flags_value: i32) callconv(.C) bool;
    pub extern fn igCheckboxFlags_UintPtr(label: ?[*:0]const u8, flags: *u32, flags_value: u32) callconv(.C) bool;
    pub extern fn igCheckboxFlags_S64Ptr(label: ?[*:0]const u8, flags: [*c]i64, flags_value: i64) callconv(.C) bool;
    pub extern fn igCheckboxFlags_U64Ptr(label: ?[*:0]const u8, flags: [*c]u64, flags_value: u64) callconv(.C) bool;
    pub extern fn igClearActiveID() callconv(.C) void;
    pub extern fn igClearDragDrop() callconv(.C) void;
    pub extern fn igClearIniSettings() callconv(.C) void;
    pub extern fn igCloseButton(id: ID, pos: *const Vec2) callconv(.C) bool;
    pub extern fn igCloseCurrentPopup() callconv(.C) void;
    pub extern fn igClosePopupToLevel(remaining: i32, restore_focus_to_window_under_popup: bool) callconv(.C) void;
    pub extern fn igClosePopupsExceptModals() callconv(.C) void;
    pub extern fn igClosePopupsOverWindow(ref_window: ?*Window, restore_focus_to_window_under_popup: bool) callconv(.C) void;
    pub extern fn igCollapseButton(id: ID, pos: *const Vec2) callconv(.C) bool;
    pub extern fn igCollapsingHeader_TreeNodeFlags(label: ?[*:0]const u8, flags: TreeNodeFlagsInt) callconv(.C) bool;
    pub extern fn igCollapsingHeader_BoolPtr(label: ?[*:0]const u8, p_visible: ?*bool, flags: TreeNodeFlagsInt) callconv(.C) bool;
    pub extern fn igColorButton(desc_id: ?[*:0]const u8, col: *const Vec4, flags: ColorEditFlagsInt, size: *const Vec2) callconv(.C) bool;
    pub extern fn igColorConvertFloat4ToU32(in: *const Vec4) callconv(.C) u32;
    pub extern fn igColorConvertHSVtoRGB(h: f32, s: f32, v: f32, out_r: *f32, out_g: *f32, out_b: *f32) callconv(.C) void;
    pub extern fn igColorConvertRGBtoHSV(r: f32, g: f32, b: f32, out_h: *f32, out_s: *f32, out_v: *f32) callconv(.C) void;
    pub extern fn igColorConvertU32ToFloat4(pOut: *Vec4, in: u32) callconv(.C) void;
    pub extern fn igColorEdit3(label: ?[*:0]const u8, col: *[3]f32, flags: ColorEditFlagsInt) callconv(.C) bool;
    pub extern fn igColorEdit4(label: ?[*:0]const u8, col: *[4]f32, flags: ColorEditFlagsInt) callconv(.C) bool;
    pub extern fn igColorEditOptionsPopup(col: [*c]const f32, flags: ColorEditFlagsInt) callconv(.C) void;
    pub extern fn igColorPicker3(label: ?[*:0]const u8, col: *[3]f32, flags: ColorEditFlagsInt) callconv(.C) bool;
    pub extern fn igColorPicker4(label: ?[*:0]const u8, col: *[4]f32, flags: ColorEditFlagsInt, ref_col: ?*const[4]f32) callconv(.C) bool;
    pub extern fn igColorPickerOptionsPopup(ref_col: [*c]const f32, flags: ColorEditFlagsInt) callconv(.C) void;
    pub extern fn igColorTooltip(text: ?[*]const u8, col: [*c]const f32, flags: ColorEditFlagsInt) callconv(.C) void;
    pub extern fn igColumns(count: i32, id: ?[*:0]const u8, border: bool) callconv(.C) void;
    pub extern fn igCombo_Str_arr(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32, popup_max_height_in_items: i32) callconv(.C) bool;
    pub extern fn igCombo_Str(label: ?[*:0]const u8, current_item: ?*i32, items_separated_by_zeros: ?[*]const u8, popup_max_height_in_items: i32) callconv(.C) bool;
    pub extern fn igCombo_FnBoolPtr(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32, popup_max_height_in_items: i32) callconv(.C) bool;
    pub extern fn igCreateContext(shared_font_atlas: ?*FontAtlas) callconv(.C) ?*Context;
    pub extern fn igCreateNewWindowSettings(name: ?[*:0]const u8) callconv(.C) ?*WindowSettings;
    pub extern fn igDataTypeApplyFromText(buf: ?[*]const u8, data_type: DataType, p_data: ?*anyopaque, format: ?[*:0]const u8) callconv(.C) bool;
    pub extern fn igDataTypeApplyOp(data_type: DataType, op: i32, output: ?*anyopaque, arg_1: ?*const anyopaque, arg_2: ?*const anyopaque) callconv(.C) void;
    pub extern fn igDataTypeClamp(data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque) callconv(.C) bool;
    pub extern fn igDataTypeCompare(data_type: DataType, arg_1: ?*const anyopaque, arg_2: ?*const anyopaque) callconv(.C) i32;
    pub extern fn igDataTypeFormatString(buf: ?[*]u8, buf_size: i32, data_type: DataType, p_data: ?*const anyopaque, format: ?[*:0]const u8) callconv(.C) i32;
    pub extern fn igDataTypeGetInfo(data_type: DataType) callconv(.C) ?*const DataTypeInfo;
    pub extern fn igDebugCheckVersionAndDataLayout(version_str: ?[*:0]const u8, sz_io: usize, sz_style: usize, sz_vec2: usize, sz_vec4: usize, sz_drawvert: usize, sz_drawidx: usize) callconv(.C) bool;
    pub extern fn igDebugDrawItemRect(col: u32) callconv(.C) void;
    pub extern fn igDebugHookIdInfo(id: ID, data_type: DataType, data_id: ?*const anyopaque, data_id_end: ?*const anyopaque) callconv(.C) void;
    pub extern fn igDebugLog(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igDebugNodeColumns(columns: [*c]OldColumns) callconv(.C) void;
    pub extern fn igDebugNodeDrawCmdShowMeshAndBoundingBox(out_draw_list: ?*DrawList, draw_list: ?*const DrawList, draw_cmd: ?*const DrawCmd, show_mesh: bool, show_aabb: bool) callconv(.C) void;
    pub extern fn igDebugNodeDrawList(window: ?*Window, draw_list: ?*const DrawList, label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igDebugNodeFont(font: ?*Font) callconv(.C) void;
    pub extern fn igDebugNodeFontGlyph(font: ?*Font, glyph: ?*const FontGlyph) callconv(.C) void;
    pub extern fn igDebugNodeInputTextState(state: ?*InputTextState) callconv(.C) void;
    pub extern fn igDebugNodeStorage(storage: ?*Storage, label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igDebugNodeTabBar(tab_bar: ?*TabBar, label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igDebugNodeTable(table: ?*Table) callconv(.C) void;
    pub extern fn igDebugNodeTableSettings(settings: [*c]TableSettings) callconv(.C) void;
    pub extern fn igDebugNodeViewport(viewport: ?*ViewportP) callconv(.C) void;
    pub extern fn igDebugNodeWindow(window: ?*Window, label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igDebugNodeWindowSettings(settings: [*c]WindowSettings) callconv(.C) void;
    pub extern fn igDebugNodeWindowsList(windows: [*c]Vector(?*Window), label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igDebugNodeWindowsListByBeginStackParent(windows: [*c][*c]Window, windows_size: i32, parent_in_begin_stack: ?*Window) callconv(.C) void;
    pub extern fn igDebugRenderViewportThumbnail(draw_list: ?*DrawList, viewport: ?*ViewportP, bb: *const Rect) callconv(.C) void;
    pub extern fn igDebugStartItemPicker() callconv(.C) void;
    pub extern fn igDebugTextEncoding(text: ?[*]const u8) callconv(.C) void;
    pub extern fn igDestroyContext(ctx: ?*Context) callconv(.C) void;
    pub extern fn igDragBehavior(id: ID, data_type: DataType, p_v: ?*anyopaque, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragFloat(label: ?[*:0]const u8, v: *f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragFloat2(label: ?[*:0]const u8, v: *[2]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragFloat3(label: ?[*:0]const u8, v: *[3]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragFloat4(label: ?[*:0]const u8, v: *[4]f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragFloatRange2(label: ?[*:0]const u8, v_current_min: *f32, v_current_max: *f32, v_speed: f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, format_max: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragInt(label: ?[*:0]const u8, v: *i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragInt2(label: ?[*:0]const u8, v: *[2]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragInt3(label: ?[*:0]const u8, v: *[3]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragInt4(label: ?[*:0]const u8, v: *[4]i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragIntRange2(label: ?[*:0]const u8, v_current_min: *i32, v_current_max: *i32, v_speed: f32, v_min: i32, v_max: i32, format: ?[*:0]const u8, format_max: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDragScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, v_speed: f32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igDummy(size: *const Vec2) callconv(.C) void;
    pub extern fn igEnd() callconv(.C) void;
    pub extern fn igEndChild() callconv(.C) void;
    pub extern fn igEndChildFrame() callconv(.C) void;
    pub extern fn igEndColumns() callconv(.C) void;
    pub extern fn igEndCombo() callconv(.C) void;
    pub extern fn igEndComboPreview() callconv(.C) void;
    pub extern fn igEndDisabled() callconv(.C) void;
    pub extern fn igEndDragDropSource() callconv(.C) void;
    pub extern fn igEndDragDropTarget() callconv(.C) void;
    pub extern fn igEndFrame() callconv(.C) void;
    pub extern fn igEndGroup() callconv(.C) void;
    pub extern fn igEndListBox() callconv(.C) void;
    pub extern fn igEndMainMenuBar() callconv(.C) void;
    pub extern fn igEndMenu() callconv(.C) void;
    pub extern fn igEndMenuBar() callconv(.C) void;
    pub extern fn igEndPopup() callconv(.C) void;
    pub extern fn igEndTabBar() callconv(.C) void;
    pub extern fn igEndTabItem() callconv(.C) void;
    pub extern fn igEndTable() callconv(.C) void;
    pub extern fn igEndTooltip() callconv(.C) void;
    pub extern fn igErrorCheckEndFrameRecover(log_callback: ErrorLogCallback, user_data: ?*anyopaque) callconv(.C) void;
    pub extern fn igErrorCheckEndWindowRecover(log_callback: ErrorLogCallback, user_data: ?*anyopaque) callconv(.C) void;
    pub extern fn igFindBestWindowPosForPopup(pOut: *Vec2, window: ?*Window) callconv(.C) void;
    pub extern fn igFindBestWindowPosForPopupEx(pOut: *Vec2, ref_pos: [*c]const Vec2, size: ?*const Vec2, last_dir: ?*Dir, r_outer: ?*const Rect, r_avoid: ?*const Rect, policy: PopupPositionPolicy) callconv(.C) void;
    pub extern fn igFindBottomMostVisibleWindowWithinBeginStack(window: ?*Window) callconv(.C) ?*Window;
    pub extern fn igFindOrCreateColumns(window: ?*Window, id: ID) callconv(.C) ?*OldColumns;
    pub extern fn igFindOrCreateWindowSettings(name: ?[*:0]const u8) callconv(.C) ?*WindowSettings;
    pub extern fn igFindRenderedTextEnd(text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) [*c]const u8;
    pub extern fn igFindSettingsHandler(type_name: ?[*:0]const u8) callconv(.C) ?*SettingsHandler;
    pub extern fn igFindWindowByID(id: ID) callconv(.C) ?*Window;
    pub extern fn igFindWindowByName(name: ?[*:0]const u8) callconv(.C) ?*Window;
    pub extern fn igFindWindowDisplayIndex(window: ?*Window) callconv(.C) i32;
    pub extern fn igFindWindowSettings(id: ID) callconv(.C) ?*WindowSettings;
    pub extern fn igFocusTopMostWindowUnderOne(under_this_window: ?*Window, ignore_window: ?*Window) callconv(.C) void;
    pub extern fn igFocusWindow(window: ?*Window) callconv(.C) void;
    pub extern fn igGcAwakeTransientWindowBuffers(window: ?*Window) callconv(.C) void;
    pub extern fn igGcCompactTransientMiscBuffers() callconv(.C) void;
    pub extern fn igGcCompactTransientWindowBuffers(window: ?*Window) callconv(.C) void;
    pub extern fn igGetActiveID() callconv(.C) ID;
    pub extern fn igGetAllocatorFunctions(p_alloc_func: ?*MemAllocFunc, p_free_func: ?*MemFreeFunc, p_user_data: ?*?*anyopaque) callconv(.C) void;
    pub extern fn igGetBackgroundDrawList_Nil() callconv(.C) ?*DrawList;
    pub extern fn igGetBackgroundDrawList_ViewportPtr(viewport: ?*Viewport) callconv(.C) ?*DrawList;
    pub extern fn igGetClipboardText() callconv(.C) ?[*:0]const u8;
    pub extern fn igGetColorU32_Col(idx: Col, alpha_mul: f32) callconv(.C) u32;
    pub extern fn igGetColorU32_Vec4(col: *const Vec4) callconv(.C) u32;
    pub extern fn igGetColorU32_U32(col: u32) callconv(.C) u32;
    pub extern fn igGetColumnIndex() callconv(.C) i32;
    pub extern fn igGetColumnNormFromOffset(columns: [*c]const OldColumns, offset: f32) callconv(.C) f32;
    pub extern fn igGetColumnOffset(column_index: i32) callconv(.C) f32;
    pub extern fn igGetColumnOffsetFromNorm(columns: [*c]const OldColumns, offset_norm: f32) callconv(.C) f32;
    pub extern fn igGetColumnWidth(column_index: i32) callconv(.C) f32;
    pub extern fn igGetColumnsCount() callconv(.C) i32;
    pub extern fn igGetColumnsID(str_id: ?[*:0]const u8, count: i32) callconv(.C) ID;
    pub extern fn igGetContentRegionAvail(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetContentRegionMax(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetContentRegionMaxAbs(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetCurrentContext() callconv(.C) ?*Context;
    pub extern fn igGetCurrentTable() callconv(.C) ?*Table;
    pub extern fn igGetCurrentWindow() callconv(.C) ?*Window;
    pub extern fn igGetCurrentWindowRead() callconv(.C) ?*Window;
    pub extern fn igGetCursorPos(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetCursorPosX() callconv(.C) f32;
    pub extern fn igGetCursorPosY() callconv(.C) f32;
    pub extern fn igGetCursorScreenPos(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetCursorStartPos(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetDefaultFont() callconv(.C) ?*Font;
    pub extern fn igGetDragDropPayload() callconv(.C) ?*const Payload;
    pub extern fn igGetDrawData() callconv(.C) *DrawData;
    pub extern fn igGetDrawListSharedData() callconv(.C) ?*DrawListSharedData;
    pub extern fn igGetFocusID() callconv(.C) ID;
    pub extern fn igGetFocusScope() callconv(.C) ID;
    pub extern fn igGetFocusedFocusScope() callconv(.C) ID;
    pub extern fn igGetFont() callconv(.C) ?*Font;
    pub extern fn igGetFontSize() callconv(.C) f32;
    pub extern fn igGetFontTexUvWhitePixel(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetForegroundDrawList_Nil() callconv(.C) ?*DrawList;
    pub extern fn igGetForegroundDrawList_WindowPtr(window: ?*Window) callconv(.C) ?*DrawList;
    pub extern fn igGetForegroundDrawList_ViewportPtr(viewport: ?*Viewport) callconv(.C) ?*DrawList;
    pub extern fn igGetFrameCount() callconv(.C) i32;
    pub extern fn igGetFrameHeight() callconv(.C) f32;
    pub extern fn igGetFrameHeightWithSpacing() callconv(.C) f32;
    pub extern fn igGetHoveredID() callconv(.C) ID;
    pub extern fn igGetID_Str(str_id: ?[*:0]const u8) callconv(.C) ID;
    pub extern fn igGetID_StrStr(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8) callconv(.C) ID;
    pub extern fn igGetID_Ptr(ptr_id: ?*const anyopaque) callconv(.C) ID;
    pub extern fn igGetIDWithSeed(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8, seed: ID) callconv(.C) ID;
    pub extern fn igGetIO() callconv(.C) *IO;
    pub extern fn igGetInputTextState(id: ID) callconv(.C) ?*InputTextState;
    pub extern fn igGetItemFlags() callconv(.C) ItemFlagsInt;
    pub extern fn igGetItemID() callconv(.C) ID;
    pub extern fn igGetItemRectMax(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetItemRectMin(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetItemRectSize(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetItemStatusFlags() callconv(.C) ItemStatusFlagsInt;
    pub extern fn igGetKeyData(key: Key) callconv(.C) ?*KeyData;
    pub extern fn igGetKeyIndex(key: Key) callconv(.C) i32;
    pub extern fn igGetKeyName(key: Key) callconv(.C) ?[*:0]const u8;
    pub extern fn igGetKeyPressedAmount(key: Key, repeat_delay: f32, rate: f32) callconv(.C) i32;
    pub extern fn igGetMainViewport() callconv(.C) ?*Viewport;
    pub extern fn igGetMergedModFlags() callconv(.C) ModFlagsInt;
    pub extern fn igGetMouseClickedCount(button: MouseButton) callconv(.C) i32;
    pub extern fn igGetMouseCursor() callconv(.C) MouseCursor;
    pub extern fn igGetMouseDragDelta(pOut: *Vec2, button: MouseButton, lock_threshold: f32) callconv(.C) void;
    pub extern fn igGetMousePos(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetMousePosOnOpeningCurrentPopup(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetNavInputAmount(n: NavInput, mode: NavReadMode) callconv(.C) f32;
    pub extern fn igGetNavInputAmount2d(pOut: *Vec2, dir_sources: NavDirSourceFlagsInt, mode: NavReadMode, slow_factor: f32, fast_factor: f32) callconv(.C) void;
    pub extern fn igGetNavInputName(n: NavInput) callconv(.C) ?[*:0]const u8;
    pub extern fn igGetPopupAllowedExtentRect(pOut: *Rect, window: ?*Window) callconv(.C) void;
    pub extern fn igGetScrollMaxX() callconv(.C) f32;
    pub extern fn igGetScrollMaxY() callconv(.C) f32;
    pub extern fn igGetScrollX() callconv(.C) f32;
    pub extern fn igGetScrollY() callconv(.C) f32;
    pub extern fn igGetStateStorage() callconv(.C) ?*Storage;
    pub extern fn igGetStyle() callconv(.C) ?*Style;
    pub extern fn igGetStyleColorName(idx: Col) callconv(.C) ?[*:0]const u8;
    pub extern fn igGetStyleColorVec4(idx: Col) callconv(.C) ?*const Vec4;
    pub extern fn igGetTextLineHeight() callconv(.C) f32;
    pub extern fn igGetTextLineHeightWithSpacing() callconv(.C) f32;
    pub extern fn igGetTime() callconv(.C) f64;
    pub extern fn igGetTopMostAndVisiblePopupModal() callconv(.C) ?*Window;
    pub extern fn igGetTopMostPopupModal() callconv(.C) ?*Window;
    pub extern fn igGetTreeNodeToLabelSpacing() callconv(.C) f32;
    pub extern fn igGetVersion() callconv(.C) ?[*:0]const u8;
    pub extern fn igGetWindowContentRegionMax(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetWindowContentRegionMin(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetWindowDrawList() callconv(.C) ?*DrawList;
    pub extern fn igGetWindowHeight() callconv(.C) f32;
    pub extern fn igGetWindowPos(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetWindowResizeBorderID(window: ?*Window, dir: Dir) callconv(.C) ID;
    pub extern fn igGetWindowResizeCornerID(window: ?*Window, n: i32) callconv(.C) ID;
    pub extern fn igGetWindowScrollbarID(window: ?*Window, axis: Axis) callconv(.C) ID;
    pub extern fn igGetWindowScrollbarRect(pOut: *Rect, window: ?*Window, axis: Axis) callconv(.C) void;
    pub extern fn igGetWindowSize(pOut: *Vec2) callconv(.C) void;
    pub extern fn igGetWindowWidth() callconv(.C) f32;
    pub extern fn igImAbs_Int(x: i32) callconv(.C) i32;
    pub extern fn igImAbs_Float(x: f32) callconv(.C) f32;
    pub extern fn igImAbs_double(x: f64) callconv(.C) f64;
    pub extern fn igImAlphaBlendColors(col_a: u32, col_b: u32) callconv(.C) u32;
    pub extern fn igImBezierCubicCalc(pOut: *Vec2, p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, t: f32) callconv(.C) void;
    pub extern fn igImBezierCubicClosestPoint(pOut: *Vec2, p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, p: ?*const Vec2, num_segments: i32) callconv(.C) void;
    pub extern fn igImBezierCubicClosestPointCasteljau(pOut: *Vec2, p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, p4: ?*const Vec2, p: ?*const Vec2, tess_tol: f32) callconv(.C) void;
    pub extern fn igImBezierQuadraticCalc(pOut: *Vec2, p1: ?*const Vec2, p2: ?*const Vec2, p3: ?*const Vec2, t: f32) callconv(.C) void;
    pub extern fn igImBitArrayClearBit(arr: ?*u32, n: i32) callconv(.C) void;
    pub extern fn igImBitArraySetBit(arr: ?*u32, n: i32) callconv(.C) void;
    pub extern fn igImBitArraySetBitRange(arr: ?*u32, n: i32, n2: i32) callconv(.C) void;
    pub extern fn igImBitArrayTestBit(arr: ?*const u32, n: i32) callconv(.C) bool;
    pub extern fn igImCharIsBlankA(c: u8) callconv(.C) bool;
    pub extern fn igImCharIsBlankW(c: u32) callconv(.C) bool;
    pub extern fn igImClamp(pOut: *Vec2, v: ?*const Vec2, mn: ?*const Vec2, mx: ?*const Vec2) callconv(.C) void;
    pub extern fn igImDot(a: *const Vec2, b: *const Vec2) callconv(.C) f32;
    pub extern fn igImFileClose(file: FileHandle) callconv(.C) bool;
    pub extern fn igImFileGetSize(file: FileHandle) callconv(.C) u64;
    pub extern fn igImFileLoadToMemory(filename: ?[*:0]const u8, mode: [*c]const u8, out_file_size: *usize, padding_bytes: i32) callconv(.C) ?*anyopaque;
    pub extern fn igImFileOpen(filename: ?[*:0]const u8, mode: [*c]const u8) callconv(.C) FileHandle;
    pub extern fn igImFileRead(data: ?*anyopaque, size: u64, count: u64, file: FileHandle) callconv(.C) u64;
    pub extern fn igImFileWrite(data: ?*const anyopaque, size: u64, count: u64, file: FileHandle) callconv(.C) u64;
    pub extern fn igImFloor_Float(f: f32) callconv(.C) f32;
    pub extern fn igImFloor_Vec2(pOut: *Vec2, v: ?*const Vec2) callconv(.C) void;
    pub extern fn igImFloorSigned_Float(f: f32) callconv(.C) f32;
    pub extern fn igImFloorSigned_Vec2(pOut: *Vec2, v: ?*const Vec2) callconv(.C) void;
    pub extern fn igImFontAtlasBuildFinish(atlas: ?*FontAtlas) callconv(.C) void;
    pub extern fn igImFontAtlasBuildInit(atlas: ?*FontAtlas) callconv(.C) void;
    pub extern fn igImFontAtlasBuildMultiplyCalcLookupTable(out_table: *[256]u8, in_multiply_factor: f32) callconv(.C) void;
    pub extern fn igImFontAtlasBuildMultiplyRectAlpha8(table: *const[256]u8, pixels: [*c]u8, x: i32, y: i32, w: i32, h: i32, stride: i32) callconv(.C) void;
    pub extern fn igImFontAtlasBuildPackCustomRects(atlas: ?*FontAtlas, stbrp_context_opaque: ?*anyopaque) callconv(.C) void;
    pub extern fn igImFontAtlasBuildRender32bppRectFromString(atlas: ?*FontAtlas, x: i32, y: i32, w: i32, h: i32, in_str: ?[*:0]const u8, in_marker_char: u8, in_marker_pixel_value: u32) callconv(.C) void;
    pub extern fn igImFontAtlasBuildRender8bppRectFromString(atlas: ?*FontAtlas, x: i32, y: i32, w: i32, h: i32, in_str: ?[*:0]const u8, in_marker_char: u8, in_marker_pixel_value: u8) callconv(.C) void;
    pub extern fn igImFontAtlasBuildSetupFont(atlas: ?*FontAtlas, font: ?*Font, font_config: ?*FontConfig, ascent: f32, descent: f32) callconv(.C) void;
    pub extern fn igImFontAtlasGetBuilderForStbTruetype() callconv(.C) ?*const FontBuilderIO;
    pub extern fn igImFormatString(buf: ?[*]u8, buf_size: usize, fmt: ?[*:0]const u8, ...) callconv(.C) i32;
    pub extern fn igImFormatStringToTempBuffer(out_buf: [*c][*c]const u8, out_buf_end: [*c][*c]const u8, fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igImGetDirQuadrantFromDelta(dx: f32, dy: f32) callconv(.C) Dir;
    pub extern fn igImHashData(data: ?*const anyopaque, data_size: usize, seed: u32) callconv(.C) ID;
    pub extern fn igImHashStr(data: ?[*]const u8, data_size: usize, seed: u32) callconv(.C) ID;
    pub extern fn igImInvLength(lhs: *const Vec2, fail_value: f32) callconv(.C) f32;
    pub extern fn igImIsFloatAboveGuaranteedIntegerPrecision(f: f32) callconv(.C) bool;
    pub extern fn igImIsPowerOfTwo_Int(v: i32) callconv(.C) bool;
    pub extern fn igImIsPowerOfTwo_U64(v: u64) callconv(.C) bool;
    pub extern fn igImLengthSqr_Vec2(lhs: *const Vec2) callconv(.C) f32;
    pub extern fn igImLengthSqr_Vec4(lhs: *const Vec4) callconv(.C) f32;
    pub extern fn igImLerp_Vec2Float(pOut: *Vec2, a: ?*const Vec2, b: ?*const Vec2, t: f32) callconv(.C) void;
    pub extern fn igImLerp_Vec2Vec2(pOut: *Vec2, a: ?*const Vec2, b: ?*const Vec2, t: ?*const Vec2) callconv(.C) void;
    pub extern fn igImLerp_Vec4(pOut: *Vec4, a: ?*const Vec4, b: ?*const Vec4, t: f32) callconv(.C) void;
    pub extern fn igImLineClosestPoint(pOut: *Vec2, a: ?*const Vec2, b: ?*const Vec2, p: ?*const Vec2) callconv(.C) void;
    pub extern fn igImLinearSweep(current: f32, target: f32, speed: f32) callconv(.C) f32;
    pub extern fn igImLog_Float(x: f32) callconv(.C) f32;
    pub extern fn igImLog_double(x: f64) callconv(.C) f64;
    pub extern fn igImMax(pOut: *Vec2, lhs: [*c]const Vec2, rhs: [*c]const Vec2) callconv(.C) void;
    pub extern fn igImMin(pOut: *Vec2, lhs: [*c]const Vec2, rhs: [*c]const Vec2) callconv(.C) void;
    pub extern fn igImModPositive(a: i32, b: i32) callconv(.C) i32;
    pub extern fn igImMul(pOut: *Vec2, lhs: [*c]const Vec2, rhs: [*c]const Vec2) callconv(.C) void;
    pub extern fn igImParseFormatFindEnd(format: ?[*:0]const u8) callconv(.C) [*c]const u8;
    pub extern fn igImParseFormatFindStart(format: ?[*:0]const u8) callconv(.C) [*c]const u8;
    pub extern fn igImParseFormatPrecision(format: ?[*:0]const u8, default_value: i32) callconv(.C) i32;
    pub extern fn igImParseFormatSanitizeForPrinting(fmt_in: [*c]const u8, fmt_out: [*c]u8, fmt_out_size: usize) callconv(.C) void;
    pub extern fn igImParseFormatSanitizeForScanning(fmt_in: [*c]const u8, fmt_out: [*c]u8, fmt_out_size: usize) callconv(.C) [*c]const u8;
    pub extern fn igImParseFormatTrimDecorations(format: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize) callconv(.C) [*c]const u8;
    pub extern fn igImPow_Float(x: f32, y: f32) callconv(.C) f32;
    pub extern fn igImPow_double(x: f64, y: f64) callconv(.C) f64;
    pub extern fn igImQsort(base: ?*anyopaque, count: usize, size_of_element: usize, compare_func: ?fn (const*: void, const*: void) callconv(.C) i32) callconv(.C) void;
    pub extern fn igImRotate(pOut: *Vec2, v: ?*const Vec2, cos_a: f32, sin_a: f32) callconv(.C) void;
    pub extern fn igImRsqrt_Float(x: f32) callconv(.C) f32;
    pub extern fn igImRsqrt_double(x: f64) callconv(.C) f64;
    pub extern fn igImSaturate(f: f32) callconv(.C) f32;
    pub extern fn igImSign_Float(x: f32) callconv(.C) f32;
    pub extern fn igImSign_double(x: f64) callconv(.C) f64;
    pub extern fn igImStrSkipBlank(str: ?[*:0]const u8) callconv(.C) [*c]const u8;
    pub extern fn igImStrTrimBlanks(str: ?[*:0]u8) callconv(.C) void;
    pub extern fn igImStrbolW(buf_mid_line: ?*const Wchar, buf_begin: ?*const Wchar) callconv(.C) ?*const Wchar;
    pub extern fn igImStrchrRange(str_begin: ?[*]const u8, str_end: ?[*]const u8, c: u8) callconv(.C) [*c]const u8;
    pub extern fn igImStrdup(str: ?[*:0]const u8) callconv(.C) [*c]u8;
    pub extern fn igImStrdupcpy(dst: [*c]u8, p_dst_size: ?*usize, str: ?[*:0]const u8) callconv(.C) [*c]u8;
    pub extern fn igImStreolRange(str: ?[*:0]const u8, str_end: ?[*]const u8) callconv(.C) [*c]const u8;
    pub extern fn igImStricmp(str1: [*c]const u8, str2: [*c]const u8) callconv(.C) i32;
    pub extern fn igImStristr(haystack: [*c]const u8, haystack_end: ?[*]const u8, needle: [*c]const u8, needle_end: ?[*]const u8) callconv(.C) [*c]const u8;
    pub extern fn igImStrlenW(str: ?*const Wchar) callconv(.C) i32;
    pub extern fn igImStrncpy(dst: [*c]u8, src: [*c]const u8, count: usize) callconv(.C) void;
    pub extern fn igImStrnicmp(str1: [*c]const u8, str2: [*c]const u8, count: usize) callconv(.C) i32;
    pub extern fn igImTextCharFromUtf8(out_char: *u32, in_text: [*c]const u8, in_text_end: ?[*]const u8) callconv(.C) i32;
    pub extern fn igImTextCharToUtf8(out_buf: *[5]u8, c: u32) callconv(.C) [*c]const u8;
    pub extern fn igImTextCountCharsFromUtf8(in_text: [*c]const u8, in_text_end: ?[*]const u8) callconv(.C) i32;
    pub extern fn igImTextCountUtf8BytesFromChar(in_text: [*c]const u8, in_text_end: ?[*]const u8) callconv(.C) i32;
    pub extern fn igImTextCountUtf8BytesFromStr(in_text: ?*const Wchar, in_text_end: ?*const Wchar) callconv(.C) i32;
    pub extern fn igImTextStrFromUtf8(out_buf: ?*Wchar, out_buf_size: i32, in_text: [*c]const u8, in_text_end: ?[*]const u8, in_remaining: [*c][*c]const u8) callconv(.C) i32;
    pub extern fn igImTextStrToUtf8(out_buf: *u8, out_buf_size: i32, in_text: ?*const Wchar, in_text_end: ?*const Wchar) callconv(.C) i32;
    pub extern fn igImTriangleArea(a: *const Vec2, b: *const Vec2, c: *const Vec2) callconv(.C) f32;
    pub extern fn igImTriangleBarycentricCoords(a: *const Vec2, b: *const Vec2, c: *const Vec2, p: *const Vec2, out_u: *f32, out_v: *f32, out_w: *f32) callconv(.C) void;
    pub extern fn igImTriangleClosestPoint(pOut: *Vec2, a: ?*const Vec2, b: ?*const Vec2, c: ?*const Vec2, p: ?*const Vec2) callconv(.C) void;
    pub extern fn igImTriangleContainsPoint(a: *const Vec2, b: *const Vec2, c: *const Vec2, p: *const Vec2) callconv(.C) bool;
    pub extern fn igImUpperPowerOfTwo(v: i32) callconv(.C) i32;
    pub extern fn igImage(user_texture_id: TextureID, size: *const Vec2, uv0: *const Vec2, uv1: *const Vec2, tint_col: *const Vec4, border_col: *const Vec4) callconv(.C) void;
    pub extern fn igImageButton(user_texture_id: TextureID, size: *const Vec2, uv0: *const Vec2, uv1: *const Vec2, frame_padding: i32, bg_col: *const Vec4, tint_col: *const Vec4) callconv(.C) bool;
    pub extern fn igImageButtonEx(id: ID, texture_id: TextureID, size: *const Vec2, uv0: *const Vec2, uv1: *const Vec2, padding: *const Vec2, bg_col: *const Vec4, tint_col: *const Vec4) callconv(.C) bool;
    pub extern fn igIndent(indent_w: f32) callconv(.C) void;
    pub extern fn igInitialize() callconv(.C) void;
    pub extern fn igInputDouble(label: ?[*:0]const u8, v: *f64, step: f64, step_fast: f64, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputFloat(label: ?[*:0]const u8, v: *f32, step: f32, step_fast: f32, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputFloat2(label: ?[*:0]const u8, v: *[2]f32, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputFloat3(label: ?[*:0]const u8, v: *[3]f32, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputFloat4(label: ?[*:0]const u8, v: *[4]f32, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputInt(label: ?[*:0]const u8, v: *i32, step: i32, step_fast: i32, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputInt2(label: ?[*:0]const u8, v: *[2]i32, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputInt3(label: ?[*:0]const u8, v: *[3]i32, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputInt4(label: ?[*:0]const u8, v: *[4]i32, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, p_step: ?*const anyopaque, p_step_fast: ?*const anyopaque, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, p_step: ?*const anyopaque, p_step_fast: ?*const anyopaque, format: ?[*:0]const u8, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igInputText(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, flags: InputTextFlagsInt, callback: InputTextCallback, user_data: ?*anyopaque) callconv(.C) bool;
    pub extern fn igInputTextEx(label: ?[*:0]const u8, hint: [*c]const u8, buf: ?[*]u8, buf_size: i32, size_arg: *const Vec2, flags: InputTextFlagsInt, callback: InputTextCallback, user_data: ?*anyopaque) callconv(.C) bool;
    pub extern fn igInputTextMultiline(label: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, size: *const Vec2, flags: InputTextFlagsInt, callback: InputTextCallback, user_data: ?*anyopaque) callconv(.C) bool;
    pub extern fn igInputTextWithHint(label: ?[*:0]const u8, hint: ?[*:0]const u8, buf: ?[*]u8, buf_size: usize, flags: InputTextFlagsInt, callback: InputTextCallback, user_data: ?*anyopaque) callconv(.C) bool;
    pub extern fn igInvisibleButton(str_id: ?[*:0]const u8, size: *const Vec2, flags: ButtonFlagsInt) callconv(.C) bool;
    pub extern fn igIsActiveIdUsingKey(key: Key) callconv(.C) bool;
    pub extern fn igIsActiveIdUsingNavDir(dir: Dir) callconv(.C) bool;
    pub extern fn igIsActiveIdUsingNavInput(input: NavInput) callconv(.C) bool;
    pub extern fn igIsAnyItemActive() callconv(.C) bool;
    pub extern fn igIsAnyItemFocused() callconv(.C) bool;
    pub extern fn igIsAnyItemHovered() callconv(.C) bool;
    pub extern fn igIsAnyMouseDown() callconv(.C) bool;
    pub extern fn igIsClippedEx(bb: *const Rect, id: ID) callconv(.C) bool;
    pub extern fn igIsDragDropActive() callconv(.C) bool;
    pub extern fn igIsDragDropPayloadBeingAccepted() callconv(.C) bool;
    pub extern fn igIsGamepadKey(key: Key) callconv(.C) bool;
    pub extern fn igIsItemActivated() callconv(.C) bool;
    pub extern fn igIsItemActive() callconv(.C) bool;
    pub extern fn igIsItemClicked(mouse_button: MouseButton) callconv(.C) bool;
    pub extern fn igIsItemDeactivated() callconv(.C) bool;
    pub extern fn igIsItemDeactivatedAfterEdit() callconv(.C) bool;
    pub extern fn igIsItemEdited() callconv(.C) bool;
    pub extern fn igIsItemFocused() callconv(.C) bool;
    pub extern fn igIsItemHovered(flags: HoveredFlagsInt) callconv(.C) bool;
    pub extern fn igIsItemToggledOpen() callconv(.C) bool;
    pub extern fn igIsItemToggledSelection() callconv(.C) bool;
    pub extern fn igIsItemVisible() callconv(.C) bool;
    pub extern fn igIsKeyDown(key: Key) callconv(.C) bool;
    pub extern fn igIsKeyPressed(key: Key, repeat: bool) callconv(.C) bool;
    pub extern fn igIsKeyReleased(key: Key) callconv(.C) bool;
    pub extern fn igIsLegacyKey(key: Key) callconv(.C) bool;
    pub extern fn igIsMouseClicked(button: MouseButton, repeat: bool) callconv(.C) bool;
    pub extern fn igIsMouseDoubleClicked(button: MouseButton) callconv(.C) bool;
    pub extern fn igIsMouseDown(button: MouseButton) callconv(.C) bool;
    pub extern fn igIsMouseDragPastThreshold(button: MouseButton, lock_threshold: f32) callconv(.C) bool;
    pub extern fn igIsMouseDragging(button: MouseButton, lock_threshold: f32) callconv(.C) bool;
    pub extern fn igIsMouseHoveringRect(r_min: *const Vec2, r_max: *const Vec2, clip: bool) callconv(.C) bool;
    pub extern fn igIsMousePosValid(mouse_pos: ?*const Vec2) callconv(.C) bool;
    pub extern fn igIsMouseReleased(button: MouseButton) callconv(.C) bool;
    pub extern fn igIsNamedKey(key: Key) callconv(.C) bool;
    pub extern fn igIsNavInputDown(n: NavInput) callconv(.C) bool;
    pub extern fn igIsNavInputTest(n: NavInput, rm: NavReadMode) callconv(.C) bool;
    pub extern fn igIsPopupOpen_Str(str_id: ?[*:0]const u8, flags: PopupFlagsInt) callconv(.C) bool;
    pub extern fn igIsPopupOpen_ID(id: ID, popup_flags: PopupFlagsInt) callconv(.C) bool;
    pub extern fn igIsRectVisible_Nil(size: *const Vec2) callconv(.C) bool;
    pub extern fn igIsRectVisible_Vec2(rect_min: *const Vec2, rect_max: *const Vec2) callconv(.C) bool;
    pub extern fn igIsWindowAbove(potential_above: ?*Window, potential_below: ?*Window) callconv(.C) bool;
    pub extern fn igIsWindowAppearing() callconv(.C) bool;
    pub extern fn igIsWindowChildOf(window: ?*Window, potential_parent: ?*Window, popup_hierarchy: bool) callconv(.C) bool;
    pub extern fn igIsWindowCollapsed() callconv(.C) bool;
    pub extern fn igIsWindowFocused(flags: FocusedFlagsInt) callconv(.C) bool;
    pub extern fn igIsWindowHovered(flags: HoveredFlagsInt) callconv(.C) bool;
    pub extern fn igIsWindowNavFocusable(window: ?*Window) callconv(.C) bool;
    pub extern fn igIsWindowWithinBeginStackOf(window: ?*Window, potential_parent: ?*Window) callconv(.C) bool;
    pub extern fn igItemAdd(bb: *const Rect, id: ID, nav_bb: ?*const Rect, extra_flags: ItemFlagsInt) callconv(.C) bool;
    pub extern fn igItemHoverable(bb: *const Rect, id: ID) callconv(.C) bool;
    pub extern fn igItemSize_Vec2(size: *const Vec2, text_baseline_y: f32) callconv(.C) void;
    pub extern fn igItemSize_Rect(bb: *const Rect, text_baseline_y: f32) callconv(.C) void;
    pub extern fn igKeepAliveID(id: ID) callconv(.C) void;
    pub extern fn igLabelText(label: ?[*:0]const u8, fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igListBox_Str_arr(label: ?[*:0]const u8, current_item: ?*i32, items: [*]const[*:0]const u8, items_count: i32, height_in_items: i32) callconv(.C) bool;
    pub extern fn igListBox_FnBoolPtr(label: ?[*:0]const u8, current_item: ?*i32, items_getter: ?fn (data: ?*anyopaque, idx: i32, out_text: *?[*:0]const u8) callconv(.C) bool, data: ?*anyopaque, items_count: i32, height_in_items: i32) callconv(.C) bool;
    pub extern fn igLoadIniSettingsFromDisk(ini_filename: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igLoadIniSettingsFromMemory(ini_data: ?[*]const u8, ini_size: usize) callconv(.C) void;
    pub extern fn igLogBegin(kind: LogType, auto_open_depth: i32) callconv(.C) void;
    pub extern fn igLogButtons() callconv(.C) void;
    pub extern fn igLogFinish() callconv(.C) void;
    pub extern fn igLogRenderedText(ref_pos: [*c]const Vec2, text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) void;
    pub extern fn igLogSetNextTextDecoration(prefix: ?[*:0]const u8, suffix: [*c]const u8) callconv(.C) void;
    pub extern fn igLogText(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igLogToBuffer(auto_open_depth: i32) callconv(.C) void;
    pub extern fn igLogToClipboard(auto_open_depth: i32) callconv(.C) void;
    pub extern fn igLogToFile(auto_open_depth: i32, filename: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igLogToTTY(auto_open_depth: i32) callconv(.C) void;
    pub extern fn igMarkIniSettingsDirty_Nil() callconv(.C) void;
    pub extern fn igMarkIniSettingsDirty_WindowPtr(window: ?*Window) callconv(.C) void;
    pub extern fn igMarkItemEdited(id: ID) callconv(.C) void;
    pub extern fn igMemAlloc(size: usize) callconv(.C) ?*anyopaque;
    pub extern fn igMemFree(ptr: ?*anyopaque) callconv(.C) void;
    pub extern fn igMenuItem_Bool(label: ?[*:0]const u8, shortcut: ?[*:0]const u8, selected: bool, enabled: bool) callconv(.C) bool;
    pub extern fn igMenuItem_BoolPtr(label: ?[*:0]const u8, shortcut: ?[*:0]const u8, p_selected: ?*bool, enabled: bool) callconv(.C) bool;
    pub extern fn igMenuItemEx(label: ?[*:0]const u8, icon: [*c]const u8, shortcut: ?[*:0]const u8, selected: bool, enabled: bool) callconv(.C) bool;
    pub extern fn igNavInitRequestApplyResult() callconv(.C) void;
    pub extern fn igNavInitWindow(window: ?*Window, force_reinit: bool) callconv(.C) void;
    pub extern fn igNavMoveRequestApplyResult() callconv(.C) void;
    pub extern fn igNavMoveRequestButNoResultYet() callconv(.C) bool;
    pub extern fn igNavMoveRequestCancel() callconv(.C) void;
    pub extern fn igNavMoveRequestForward(move_dir: Dir, clip_dir: Dir, move_flags: NavMoveFlagsInt, scroll_flags: ScrollFlagsInt) callconv(.C) void;
    pub extern fn igNavMoveRequestResolveWithLastItem(result: ?*NavItemData) callconv(.C) void;
    pub extern fn igNavMoveRequestSubmit(move_dir: Dir, clip_dir: Dir, move_flags: NavMoveFlagsInt, scroll_flags: ScrollFlagsInt) callconv(.C) void;
    pub extern fn igNavMoveRequestTryWrapping(window: ?*Window, move_flags: NavMoveFlagsInt) callconv(.C) void;
    pub extern fn igNewFrame() callconv(.C) void;
    pub extern fn igNewLine() callconv(.C) void;
    pub extern fn igNextColumn() callconv(.C) void;
    pub extern fn igOpenPopup_Str(str_id: ?[*:0]const u8, popup_flags: PopupFlagsInt) callconv(.C) void;
    pub extern fn igOpenPopup_ID(id: ID, popup_flags: PopupFlagsInt) callconv(.C) void;
    pub extern fn igOpenPopupEx(id: ID, popup_flags: PopupFlagsInt) callconv(.C) void;
    pub extern fn igOpenPopupOnItemClick(str_id: ?[*:0]const u8, popup_flags: PopupFlagsInt) callconv(.C) void;
    pub extern fn igPlotEx(plot_type: PlotType, label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, frame_size: *const Vec2) callconv(.C) i32;
    pub extern fn igPlotHistogram_FloatPtr(label: ?[*:0]const u8, values: *const f32, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: *const Vec2, stride: i32) callconv(.C) void;
    pub extern fn igPlotHistogram_FnFloatPtr(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: *const Vec2) callconv(.C) void;
    pub extern fn igPlotLines_FloatPtr(label: ?[*:0]const u8, values: *const f32, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: *const Vec2, stride: i32) callconv(.C) void;
    pub extern fn igPlotLines_FnFloatPtr(label: ?[*:0]const u8, values_getter: ?fn (data: ?*anyopaque, idx: i32) callconv(.C) f32, data: ?*anyopaque, values_count: i32, values_offset: i32, overlay_text: ?[*:0]const u8, scale_min: f32, scale_max: f32, graph_size: *const Vec2) callconv(.C) void;
    pub extern fn igPopAllowKeyboardFocus() callconv(.C) void;
    pub extern fn igPopButtonRepeat() callconv(.C) void;
    pub extern fn igPopClipRect() callconv(.C) void;
    pub extern fn igPopColumnsBackground() callconv(.C) void;
    pub extern fn igPopFocusScope() callconv(.C) void;
    pub extern fn igPopFont() callconv(.C) void;
    pub extern fn igPopID() callconv(.C) void;
    pub extern fn igPopItemFlag() callconv(.C) void;
    pub extern fn igPopItemWidth() callconv(.C) void;
    pub extern fn igPopStyleColor(count: i32) callconv(.C) void;
    pub extern fn igPopStyleVar(count: i32) callconv(.C) void;
    pub extern fn igPopTextWrapPos() callconv(.C) void;
    pub extern fn igProgressBar(fraction: f32, size_arg: *const Vec2, overlay: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igPushAllowKeyboardFocus(allow_keyboard_focus: bool) callconv(.C) void;
    pub extern fn igPushButtonRepeat(repeat: bool) callconv(.C) void;
    pub extern fn igPushClipRect(clip_rect_min: *const Vec2, clip_rect_max: *const Vec2, intersect_with_current_clip_rect: bool) callconv(.C) void;
    pub extern fn igPushColumnClipRect(column_index: i32) callconv(.C) void;
    pub extern fn igPushColumnsBackground() callconv(.C) void;
    pub extern fn igPushFocusScope(id: ID) callconv(.C) void;
    pub extern fn igPushFont(font: ?*Font) callconv(.C) void;
    pub extern fn igPushID_Str(str_id: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igPushID_StrStr(str_id_begin: ?[*]const u8, str_id_end: ?[*]const u8) callconv(.C) void;
    pub extern fn igPushID_Ptr(ptr_id: ?*const anyopaque) callconv(.C) void;
    pub extern fn igPushID_Int(int_id: i32) callconv(.C) void;
    pub extern fn igPushItemFlag(option: ItemFlagsInt, enabled: bool) callconv(.C) void;
    pub extern fn igPushItemWidth(item_width: f32) callconv(.C) void;
    pub extern fn igPushMultiItemsWidths(components: i32, width_full: f32) callconv(.C) void;
    pub extern fn igPushOverrideID(id: ID) callconv(.C) void;
    pub extern fn igPushStyleColor_U32(idx: Col, col: u32) callconv(.C) void;
    pub extern fn igPushStyleColor_Vec4(idx: Col, col: *const Vec4) callconv(.C) void;
    pub extern fn igPushStyleVar_Float(idx: StyleVar, val: f32) callconv(.C) void;
    pub extern fn igPushStyleVar_Vec2(idx: StyleVar, val: *const Vec2) callconv(.C) void;
    pub extern fn igPushTextWrapPos(wrap_local_pos_x: f32) callconv(.C) void;
    pub extern fn igRadioButton_Bool(label: ?[*:0]const u8, active: bool) callconv(.C) bool;
    pub extern fn igRadioButton_IntPtr(label: ?[*:0]const u8, v: *i32, v_button: i32) callconv(.C) bool;
    pub extern fn igRemoveContextHook(context: ?*Context, hook_to_remove: ID) callconv(.C) void;
    pub extern fn igRemoveSettingsHandler(type_name: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igRender() callconv(.C) void;
    pub extern fn igRenderArrow(draw_list: ?*DrawList, pos: *const Vec2, col: u32, dir: Dir, scale: f32) callconv(.C) void;
    pub extern fn igRenderArrowPointingAt(draw_list: ?*DrawList, pos: *const Vec2, half_sz: *const Vec2, direction: Dir, col: u32) callconv(.C) void;
    pub extern fn igRenderBullet(draw_list: ?*DrawList, pos: *const Vec2, col: u32) callconv(.C) void;
    pub extern fn igRenderCheckMark(draw_list: ?*DrawList, pos: *const Vec2, col: u32, sz: f32) callconv(.C) void;
    pub extern fn igRenderColorRectWithAlphaCheckerboard(draw_list: ?*DrawList, p_min: *const Vec2, p_max: *const Vec2, fill_col: u32, grid_step: f32, grid_off: *const Vec2, rounding: f32, flags: DrawFlagsInt) callconv(.C) void;
    pub extern fn igRenderFrame(p_min: *const Vec2, p_max: *const Vec2, fill_col: u32, border: bool, rounding: f32) callconv(.C) void;
    pub extern fn igRenderFrameBorder(p_min: *const Vec2, p_max: *const Vec2, rounding: f32) callconv(.C) void;
    pub extern fn igRenderMouseCursor(pos: *const Vec2, scale: f32, mouse_cursor: MouseCursor, col_fill: u32, col_border: u32, col_shadow: u32) callconv(.C) void;
    pub extern fn igRenderNavHighlight(bb: *const Rect, id: ID, flags: NavHighlightFlagsInt) callconv(.C) void;
    pub extern fn igRenderRectFilledRangeH(draw_list: ?*DrawList, rect: *const Rect, col: u32, x_start_norm: f32, x_end_norm: f32, rounding: f32) callconv(.C) void;
    pub extern fn igRenderRectFilledWithHole(draw_list: ?*DrawList, outer: *const Rect, inner: *const Rect, col: u32, rounding: f32) callconv(.C) void;
    pub extern fn igRenderText(pos: *const Vec2, text: ?[*]const u8, text_end: ?[*]const u8, hide_text_after_hash: bool) callconv(.C) void;
    pub extern fn igRenderTextClipped(pos_min: *const Vec2, pos_max: *const Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2, align: *const Vec2, clip_rect: ?*const Rect) callconv(.C) void;
    pub extern fn igRenderTextClippedEx(draw_list: ?*DrawList, pos_min: *const Vec2, pos_max: *const Vec2, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2, align: *const Vec2, clip_rect: ?*const Rect) callconv(.C) void;
    pub extern fn igRenderTextEllipsis(draw_list: ?*DrawList, pos_min: *const Vec2, pos_max: *const Vec2, clip_max_x: f32, ellipsis_max_x: f32, text: ?[*]const u8, text_end: ?[*]const u8, text_size_if_known: ?*const Vec2) callconv(.C) void;
    pub extern fn igRenderTextWrapped(pos: *const Vec2, text: ?[*]const u8, text_end: ?[*]const u8, wrap_width: f32) callconv(.C) void;
    pub extern fn igResetMouseDragDelta(button: MouseButton) callconv(.C) void;
    pub extern fn igSameLine(offset_from_start_x: f32, spacing: f32) callconv(.C) void;
    pub extern fn igSaveIniSettingsToDisk(ini_filename: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igSaveIniSettingsToMemory(out_ini_size: ?*usize) callconv(.C) ?[*:0]const u8;
    pub extern fn igScrollToBringRectIntoView(window: ?*Window, rect: *const Rect) callconv(.C) void;
    pub extern fn igScrollToItem(flags: ScrollFlagsInt) callconv(.C) void;
    pub extern fn igScrollToRect(window: ?*Window, rect: *const Rect, flags: ScrollFlagsInt) callconv(.C) void;
    pub extern fn igScrollToRectEx(pOut: *Vec2, window: ?*Window, rect: ?*const Rect, flags: ScrollFlagsInt) callconv(.C) void;
    pub extern fn igScrollbar(axis: Axis) callconv(.C) void;
    pub extern fn igScrollbarEx(bb: *const Rect, id: ID, axis: Axis, p_scroll_v: ?*i64, avail_v: i64, contents_v: i64, flags: DrawFlagsInt) callconv(.C) bool;
    pub extern fn igSelectable_Bool(label: ?[*:0]const u8, selected: bool, flags: SelectableFlagsInt, size: *const Vec2) callconv(.C) bool;
    pub extern fn igSelectable_BoolPtr(label: ?[*:0]const u8, p_selected: ?*bool, flags: SelectableFlagsInt, size: *const Vec2) callconv(.C) bool;
    pub extern fn igSeparator() callconv(.C) void;
    pub extern fn igSeparatorEx(flags: SeparatorFlagsInt) callconv(.C) void;
    pub extern fn igSetActiveID(id: ID, window: ?*Window) callconv(.C) void;
    pub extern fn igSetActiveIdUsingKey(key: Key) callconv(.C) void;
    pub extern fn igSetActiveIdUsingNavAndKeys() callconv(.C) void;
    pub extern fn igSetAllocatorFunctions(alloc_func: MemAllocFunc, free_func: MemFreeFunc, user_data: ?*anyopaque) callconv(.C) void;
    pub extern fn igSetClipboardText(text: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igSetColorEditOptions(flags: ColorEditFlagsInt) callconv(.C) void;
    pub extern fn igSetColumnOffset(column_index: i32, offset_x: f32) callconv(.C) void;
    pub extern fn igSetColumnWidth(column_index: i32, width: f32) callconv(.C) void;
    pub extern fn igSetCurrentContext(ctx: ?*Context) callconv(.C) void;
    pub extern fn igSetCurrentFont(font: ?*Font) callconv(.C) void;
    pub extern fn igSetCursorPos(local_pos: *const Vec2) callconv(.C) void;
    pub extern fn igSetCursorPosX(local_x: f32) callconv(.C) void;
    pub extern fn igSetCursorPosY(local_y: f32) callconv(.C) void;
    pub extern fn igSetCursorScreenPos(pos: *const Vec2) callconv(.C) void;
    pub extern fn igSetDragDropPayload(kind: ?[*:0]const u8, data: ?*const anyopaque, sz: usize, cond: CondFlagsInt) callconv(.C) bool;
    pub extern fn igSetFocusID(id: ID, window: ?*Window) callconv(.C) void;
    pub extern fn igSetHoveredID(id: ID) callconv(.C) void;
    pub extern fn igSetItemAllowOverlap() callconv(.C) void;
    pub extern fn igSetItemDefaultFocus() callconv(.C) void;
    pub extern fn igSetItemUsingMouseWheel() callconv(.C) void;
    pub extern fn igSetKeyboardFocusHere(offset: i32) callconv(.C) void;
    pub extern fn igSetLastItemData(item_id: ID, in_flags: ItemFlagsInt, status_flags: ItemStatusFlagsInt, item_rect: *const Rect) callconv(.C) void;
    pub extern fn igSetMouseCursor(cursor_type: MouseCursor) callconv(.C) void;
    pub extern fn igSetNavID(id: ID, nav_layer: NavLayer, focus_scope_id: ID, rect_rel: *const Rect) callconv(.C) void;
    pub extern fn igSetNavWindow(window: ?*Window) callconv(.C) void;
    pub extern fn igSetNextFrameWantCaptureKeyboard(want_capture_keyboard: bool) callconv(.C) void;
    pub extern fn igSetNextFrameWantCaptureMouse(want_capture_mouse: bool) callconv(.C) void;
    pub extern fn igSetNextItemOpen(is_open: bool, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetNextItemWidth(item_width: f32) callconv(.C) void;
    pub extern fn igSetNextWindowBgAlpha(alpha: f32) callconv(.C) void;
    pub extern fn igSetNextWindowCollapsed(collapsed: bool, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetNextWindowContentSize(size: *const Vec2) callconv(.C) void;
    pub extern fn igSetNextWindowFocus() callconv(.C) void;
    pub extern fn igSetNextWindowPos(pos: *const Vec2, cond: CondFlagsInt, pivot: *const Vec2) callconv(.C) void;
    pub extern fn igSetNextWindowScroll(scroll: *const Vec2) callconv(.C) void;
    pub extern fn igSetNextWindowSize(size: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetNextWindowSizeConstraints(size_min: *const Vec2, size_max: *const Vec2, custom_callback: SizeCallback, custom_callback_data: ?*anyopaque) callconv(.C) void;
    pub extern fn igSetScrollFromPosX_Float(local_x: f32, center_x_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollFromPosX_WindowPtr(window: ?*Window, local_x: f32, center_x_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollFromPosY_Float(local_y: f32, center_y_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollFromPosY_WindowPtr(window: ?*Window, local_y: f32, center_y_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollHereX(center_x_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollHereY(center_y_ratio: f32) callconv(.C) void;
    pub extern fn igSetScrollX_Float(scroll_x: f32) callconv(.C) void;
    pub extern fn igSetScrollX_WindowPtr(window: ?*Window, scroll_x: f32) callconv(.C) void;
    pub extern fn igSetScrollY_Float(scroll_y: f32) callconv(.C) void;
    pub extern fn igSetScrollY_WindowPtr(window: ?*Window, scroll_y: f32) callconv(.C) void;
    pub extern fn igSetStateStorage(storage: ?*Storage) callconv(.C) void;
    pub extern fn igSetTabItemClosed(tab_or_docked_window_label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igSetTooltip(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igSetWindowClipRectBeforeSetChannel(window: ?*Window, clip_rect: *const Rect) callconv(.C) void;
    pub extern fn igSetWindowCollapsed_Bool(collapsed: bool, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowCollapsed_Str(name: ?[*:0]const u8, collapsed: bool, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowCollapsed_WindowPtr(window: ?*Window, collapsed: bool, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowFocus_Nil() callconv(.C) void;
    pub extern fn igSetWindowFocus_Str(name: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igSetWindowFontScale(scale: f32) callconv(.C) void;
    pub extern fn igSetWindowHitTestHole(window: ?*Window, pos: *const Vec2, size: *const Vec2) callconv(.C) void;
    pub extern fn igSetWindowPos_Vec2(pos: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowPos_Str(name: ?[*:0]const u8, pos: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowPos_WindowPtr(window: ?*Window, pos: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowSize_Vec2(size: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowSize_Str(name: ?[*:0]const u8, size: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowSize_WindowPtr(window: ?*Window, size: *const Vec2, cond: CondFlagsInt) callconv(.C) void;
    pub extern fn igSetWindowViewport(window: ?*Window, viewport: ?*ViewportP) callconv(.C) void;
    pub extern fn igShadeVertsLinearColorGradientKeepAlpha(draw_list: ?*DrawList, vert_start_idx: i32, vert_end_idx: i32, gradient_p0: *const Vec2, gradient_p1: *const Vec2, col0: u32, col1: u32) callconv(.C) void;
    pub extern fn igShadeVertsLinearUV(draw_list: ?*DrawList, vert_start_idx: i32, vert_end_idx: i32, a: *const Vec2, b: *const Vec2, uv_a: *const Vec2, uv_b: *const Vec2, clamp: bool) callconv(.C) void;
    pub extern fn igShowAboutWindow(p_open: ?*bool) callconv(.C) void;
    pub extern fn igShowDebugLogWindow(p_open: ?*bool) callconv(.C) void;
    pub extern fn igShowDemoWindow(p_open: ?*bool) callconv(.C) void;
    pub extern fn igShowFontAtlas(atlas: ?*FontAtlas) callconv(.C) void;
    pub extern fn igShowFontSelector(label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igShowMetricsWindow(p_open: ?*bool) callconv(.C) void;
    pub extern fn igShowStackToolWindow(p_open: ?*bool) callconv(.C) void;
    pub extern fn igShowStyleEditor(ref: ?*Style) callconv(.C) void;
    pub extern fn igShowStyleSelector(label: ?[*:0]const u8) callconv(.C) bool;
    pub extern fn igShowUserGuide() callconv(.C) void;
    pub extern fn igShrinkWidths(items: [*c]ShrinkWidthItem, count: i32, width_excess: f32) callconv(.C) void;
    pub extern fn igShutdown() callconv(.C) void;
    pub extern fn igSliderAngle(label: ?[*:0]const u8, v_rad: *f32, v_degrees_min: f32, v_degrees_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderBehavior(bb: *const Rect, id: ID, data_type: DataType, p_v: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt, out_grab_bb: ?*Rect) callconv(.C) bool;
    pub extern fn igSliderFloat(label: ?[*:0]const u8, v: *f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderFloat2(label: ?[*:0]const u8, v: *[2]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderFloat3(label: ?[*:0]const u8, v: *[3]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderFloat4(label: ?[*:0]const u8, v: *[4]f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderInt(label: ?[*:0]const u8, v: *i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderInt2(label: ?[*:0]const u8, v: *[2]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderInt3(label: ?[*:0]const u8, v: *[3]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderInt4(label: ?[*:0]const u8, v: *[4]i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderScalar(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSliderScalarN(label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, components: i32, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igSmallButton(label: ?[*:0]const u8) callconv(.C) bool;
    pub extern fn igSpacing() callconv(.C) void;
    pub extern fn igSplitterBehavior(bb: *const Rect, id: ID, axis: Axis, size1: [*c]f32, size2: [*c]f32, min_size1: f32, min_size2: f32, hover_extend: f32, hover_visibility_delay: f32) callconv(.C) bool;
    pub extern fn igStartMouseMovingWindow(window: ?*Window) callconv(.C) void;
    pub extern fn igStyleColorsClassic(dst: ?*Style) callconv(.C) void;
    pub extern fn igStyleColorsDark(dst: ?*Style) callconv(.C) void;
    pub extern fn igStyleColorsLight(dst: ?*Style) callconv(.C) void;
    pub extern fn igTabBarCloseTab(tab_bar: ?*TabBar, tab: ?*TabItem) callconv(.C) void;
    pub extern fn igTabBarFindTabByID(tab_bar: ?*TabBar, tab_id: ID) callconv(.C) ?*TabItem;
    pub extern fn igTabBarProcessReorder(tab_bar: ?*TabBar) callconv(.C) bool;
    pub extern fn igTabBarQueueReorder(tab_bar: ?*TabBar, tab: ?*const TabItem, offset: i32) callconv(.C) void;
    pub extern fn igTabBarQueueReorderFromMousePos(tab_bar: ?*TabBar, tab: ?*const TabItem, mouse_pos: *const Vec2) callconv(.C) void;
    pub extern fn igTabBarRemoveTab(tab_bar: ?*TabBar, tab_id: ID) callconv(.C) void;
    pub extern fn igTabItemBackground(draw_list: ?*DrawList, bb: *const Rect, flags: TabItemFlagsInt, col: u32) callconv(.C) void;
    pub extern fn igTabItemButton(label: ?[*:0]const u8, flags: TabItemFlagsInt) callconv(.C) bool;
    pub extern fn igTabItemCalcSize(pOut: *Vec2, label: ?[*:0]const u8, has_close_button: bool) callconv(.C) void;
    pub extern fn igTabItemEx(tab_bar: ?*TabBar, label: ?[*:0]const u8, p_open: ?*bool, flags: TabItemFlagsInt) callconv(.C) bool;
    pub extern fn igTabItemLabelAndCloseButton(draw_list: ?*DrawList, bb: *const Rect, flags: TabItemFlagsInt, frame_padding: *const Vec2, label: ?[*:0]const u8, tab_id: ID, close_button_id: ID, is_contents_visible: bool, out_just_closed: *bool, out_text_clipped: *bool) callconv(.C) void;
    pub extern fn igTableBeginApplyRequests(table: ?*Table) callconv(.C) void;
    pub extern fn igTableBeginCell(table: ?*Table, column_n: i32) callconv(.C) void;
    pub extern fn igTableBeginInitMemory(table: ?*Table, columns_count: i32) callconv(.C) void;
    pub extern fn igTableBeginRow(table: ?*Table) callconv(.C) void;
    pub extern fn igTableDrawBorders(table: ?*Table) callconv(.C) void;
    pub extern fn igTableDrawContextMenu(table: ?*Table) callconv(.C) void;
    pub extern fn igTableEndCell(table: ?*Table) callconv(.C) void;
    pub extern fn igTableEndRow(table: ?*Table) callconv(.C) void;
    pub extern fn igTableFindByID(id: ID) callconv(.C) ?*Table;
    pub extern fn igTableFixColumnSortDirection(table: ?*Table, column: ?*TableColumn) callconv(.C) void;
    pub extern fn igTableGcCompactSettings() callconv(.C) void;
    pub extern fn igTableGcCompactTransientBuffers_TablePtr(table: ?*Table) callconv(.C) void;
    pub extern fn igTableGcCompactTransientBuffers_TableTempDataPtr(table: ?*TableTempData) callconv(.C) void;
    pub extern fn igTableGetBoundSettings(table: ?*Table) callconv(.C) ?*TableSettings;
    pub extern fn igTableGetCellBgRect(pOut: *Rect, table: ?*const Table, column_n: i32) callconv(.C) void;
    pub extern fn igTableGetColumnCount() callconv(.C) i32;
    pub extern fn igTableGetColumnFlags(column_n: i32) callconv(.C) TableColumnFlagsInt;
    pub extern fn igTableGetColumnIndex() callconv(.C) i32;
    pub extern fn igTableGetColumnName_Int(column_n: i32) callconv(.C) [*c]const u8;
    pub extern fn igTableGetColumnName_TablePtr(table: ?*const Table, column_n: i32) callconv(.C) [*c]const u8;
    pub extern fn igTableGetColumnNextSortDirection(column: ?*TableColumn) callconv(.C) SortDirection;
    pub extern fn igTableGetColumnResizeID(table: ?*const Table, column_n: i32, instance_no: i32) callconv(.C) ID;
    pub extern fn igTableGetColumnWidthAuto(table: ?*Table, column: ?*TableColumn) callconv(.C) f32;
    pub extern fn igTableGetHeaderRowHeight() callconv(.C) f32;
    pub extern fn igTableGetHoveredColumn() callconv(.C) i32;
    pub extern fn igTableGetInstanceData(table: ?*Table, instance_no: i32) callconv(.C) ?*TableInstanceData;
    pub extern fn igTableGetMaxColumnWidth(table: ?*const Table, column_n: i32) callconv(.C) f32;
    pub extern fn igTableGetRowIndex() callconv(.C) i32;
    pub extern fn igTableGetSortSpecs() callconv(.C) ?*TableSortSpecs;
    pub extern fn igTableHeader(label: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igTableHeadersRow() callconv(.C) void;
    pub extern fn igTableLoadSettings(table: ?*Table) callconv(.C) void;
    pub extern fn igTableMergeDrawChannels(table: ?*Table) callconv(.C) void;
    pub extern fn igTableNextColumn() callconv(.C) bool;
    pub extern fn igTableNextRow(row_flags: TableRowFlagsInt, min_row_height: f32) callconv(.C) void;
    pub extern fn igTableOpenContextMenu(column_n: i32) callconv(.C) void;
    pub extern fn igTablePopBackgroundChannel() callconv(.C) void;
    pub extern fn igTablePushBackgroundChannel() callconv(.C) void;
    pub extern fn igTableRemove(table: ?*Table) callconv(.C) void;
    pub extern fn igTableResetSettings(table: ?*Table) callconv(.C) void;
    pub extern fn igTableSaveSettings(table: ?*Table) callconv(.C) void;
    pub extern fn igTableSetBgColor(target: TableBgTarget, color: u32, column_n: i32) callconv(.C) void;
    pub extern fn igTableSetColumnEnabled(column_n: i32, v: bool) callconv(.C) void;
    pub extern fn igTableSetColumnIndex(column_n: i32) callconv(.C) bool;
    pub extern fn igTableSetColumnSortDirection(column_n: i32, sort_direction: SortDirection, append_to_sort_specs: bool) callconv(.C) void;
    pub extern fn igTableSetColumnWidth(column_n: i32, width: f32) callconv(.C) void;
    pub extern fn igTableSetColumnWidthAutoAll(table: ?*Table) callconv(.C) void;
    pub extern fn igTableSetColumnWidthAutoSingle(table: ?*Table, column_n: i32) callconv(.C) void;
    pub extern fn igTableSettingsAddSettingsHandler() callconv(.C) void;
    pub extern fn igTableSettingsCreate(id: ID, columns_count: i32) callconv(.C) ?*TableSettings;
    pub extern fn igTableSettingsFindByID(id: ID) callconv(.C) ?*TableSettings;
    pub extern fn igTableSetupColumn(label: ?[*:0]const u8, flags: TableColumnFlagsInt, init_width_or_weight: f32, user_id: ID) callconv(.C) void;
    pub extern fn igTableSetupDrawChannels(table: ?*Table) callconv(.C) void;
    pub extern fn igTableSetupScrollFreeze(cols: i32, rows: i32) callconv(.C) void;
    pub extern fn igTableSortSpecsBuild(table: ?*Table) callconv(.C) void;
    pub extern fn igTableSortSpecsSanitize(table: ?*Table) callconv(.C) void;
    pub extern fn igTableUpdateBorders(table: ?*Table) callconv(.C) void;
    pub extern fn igTableUpdateColumnsWeightFromWidth(table: ?*Table) callconv(.C) void;
    pub extern fn igTableUpdateLayout(table: ?*Table) callconv(.C) void;
    pub extern fn igTempInputIsActive(id: ID) callconv(.C) bool;
    pub extern fn igTempInputScalar(bb: *const Rect, id: ID, label: ?[*:0]const u8, data_type: DataType, p_data: ?*anyopaque, format: ?[*:0]const u8, p_clamp_min: ?*const anyopaque, p_clamp_max: ?*const anyopaque) callconv(.C) bool;
    pub extern fn igTempInputText(bb: *const Rect, id: ID, label: ?[*:0]const u8, buf: ?[*]u8, buf_size: i32, flags: InputTextFlagsInt) callconv(.C) bool;
    pub extern fn igText(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igTextColored(col: *const Vec4, fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igTextDisabled(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igTextEx(text: ?[*]const u8, text_end: ?[*]const u8, flags: TextFlagsInt) callconv(.C) void;
    pub extern fn igTextUnformatted(text: ?[*]const u8, text_end: ?[*]const u8) callconv(.C) void;
    pub extern fn igTextWrapped(fmt: ?[*:0]const u8, ...) callconv(.C) void;
    pub extern fn igTreeNode_Str(label: ?[*:0]const u8) callconv(.C) bool;
    pub extern fn igTreeNode_StrStr(str_id: ?[*:0]const u8, fmt: ?[*:0]const u8, ...) callconv(.C) bool;
    pub extern fn igTreeNode_Ptr(ptr_id: ?*const anyopaque, fmt: ?[*:0]const u8, ...) callconv(.C) bool;
    pub extern fn igTreeNodeBehavior(id: ID, flags: TreeNodeFlagsInt, label: ?[*:0]const u8, label_end: ?[*]const u8) callconv(.C) bool;
    pub extern fn igTreeNodeBehaviorIsOpen(id: ID, flags: TreeNodeFlagsInt) callconv(.C) bool;
    pub extern fn igTreeNodeEx_Str(label: ?[*:0]const u8, flags: TreeNodeFlagsInt) callconv(.C) bool;
    pub extern fn igTreeNodeEx_StrStr(str_id: ?[*:0]const u8, flags: TreeNodeFlagsInt, fmt: ?[*:0]const u8, ...) callconv(.C) bool;
    pub extern fn igTreeNodeEx_Ptr(ptr_id: ?*const anyopaque, flags: TreeNodeFlagsInt, fmt: ?[*:0]const u8, ...) callconv(.C) bool;
    pub extern fn igTreePop() callconv(.C) void;
    pub extern fn igTreePush_Str(str_id: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igTreePush_Ptr(ptr_id: ?*const anyopaque) callconv(.C) void;
    pub extern fn igTreePushOverrideID(id: ID) callconv(.C) void;
    pub extern fn igUnindent(indent_w: f32) callconv(.C) void;
    pub extern fn igUpdateHoveredWindowAndCaptureFlags() callconv(.C) void;
    pub extern fn igUpdateInputEvents(trickle_fast_inputs: bool) callconv(.C) void;
    pub extern fn igUpdateMouseMovingWindowEndFrame() callconv(.C) void;
    pub extern fn igUpdateMouseMovingWindowNewFrame() callconv(.C) void;
    pub extern fn igUpdateWindowParentAndRootLinks(window: ?*Window, flags: WindowFlagsInt, parent_window: ?*Window) callconv(.C) void;
    pub extern fn igVSliderFloat(label: ?[*:0]const u8, size: *const Vec2, v: *f32, v_min: f32, v_max: f32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igVSliderInt(label: ?[*:0]const u8, size: *const Vec2, v: *i32, v_min: i32, v_max: i32, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igVSliderScalar(label: ?[*:0]const u8, size: *const Vec2, data_type: DataType, p_data: ?*anyopaque, p_min: ?*const anyopaque, p_max: ?*const anyopaque, format: ?[*:0]const u8, flags: SliderFlagsInt) callconv(.C) bool;
    pub extern fn igValue_Bool(prefix: ?[*:0]const u8, b: bool) callconv(.C) void;
    pub extern fn igValue_Int(prefix: ?[*:0]const u8, v: i32) callconv(.C) void;
    pub extern fn igValue_Uint(prefix: ?[*:0]const u8, v: u32) callconv(.C) void;
    pub extern fn igValue_Float(prefix: ?[*:0]const u8, v: f32, float_format: ?[*:0]const u8) callconv(.C) void;
    pub extern fn igWindowRectAbsToRel(pOut: *Rect, window: ?*Window, r: ?*const Rect) callconv(.C) void;
    pub extern fn igWindowRectRelToAbs(pOut: *Rect, window: ?*Window, r: ?*const Rect) callconv(.C) void;
};
