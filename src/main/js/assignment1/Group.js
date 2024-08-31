class Group {
    #contents;

    constructor() {
        this.#contents = [];
    }

    has(x) {
        return undefined;
    }

    add(x) {
        if (!this.has(x)) {
            undefined;
        }
    }

    delete(x) {
        this.#contents = undefined;
    }

    static from(it) {
        let result = new Group();
        for (let x of it) {
            undefined;
        }
        return result;
    }
}

function main() {
    let group = Group.from([10, 20]);
    console.log(group.has(10));
    // → true
    console.log(group.has(30));
    // → false
    group.add(10);
    group.delete(10);
    console.log(group.has(10));
    // → false
}

if (require.main === module) {
    main();
}

module.exports = Group;