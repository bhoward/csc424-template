const Group = require("./Group");

test("An empty Group should not contain any elements", () => {
    const empty = new Group()
    const sample = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    expect(sample.some(x => empty.has(x))).toBe(false);
});

test("An empty Group should allow deleting a non-existent element", () => {
    const empty = new Group();
    expect(() => empty.delete(0)).not.toThrowError();
});

test("A group created from a List should contain those elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    expect(sample.every(x => group.has(x))).toBe(true);
});

test("A group created from a List should not contain other elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    const others = [0, 7, 8];
    expect(others.some(x => group.has(x))).toBe(false);
});

test("An element added to a group should be contained in the group", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.add(7);
    expect(group.has(7)).toBe(true);
});

test("An element added to a group should not change the other elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.add(7);
    expect(sample.every(x => group.has(x))).toBe(true);
});

test("An element added to a group should not add other elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.add(7);
    const others = [0, 8];
    expect(others.some(x => group.has(x))).toBe(false);
});

test("An element deleted from a group should not be contained in the group", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.delete(1);
    expect(group.has(1)).toBe(false);
});

test("An element deleted from a group should not change the other elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.delete(1);
    const rest = [2, 3, 4, 5, 6, 9];
    expect(rest.every(x => group.has(x))).toBe(true);
});

test("An element deleted from a group should not add other elements", () => {
    const sample = [3, 1, 4, 1, 5, 9, 2, 6];
    const group = Group.from(sample);
    group.delete(1);
    const others = [0, 7, 8];
    expect(others.some(x => group.has(x))).toBe(false);
});
