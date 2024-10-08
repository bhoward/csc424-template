// Logging proxy
function logging(target) {
    return new Proxy(target, {
        get: function(target, prop, self) {
            console.log(`GET: ${JSON.stringify({target, prop})}`);
            return Reflect.get(target, prop, target);
        },

        set: function(target, prop, value, self) {
            console.log(`SET: ${JSON.stringify({target, prop, value})}`);
            return Reflect.set(target, prop, value, target);
        },
    });
}

class Demo {
    // Note that #private fields don't play well with proxies

    constructor(name) {
        this.name = name;
        this._age = 0; // Only private by convention...
    }

    get age() {
        return this._age;
    }

    birthdays(n) {
        for (let i = 0; i < n; i++) {
            this._age++;
            console.log(`Happy Birthday, ${this.name}! Now you are ${this.age}!`);
        }
    }
}

function main() {
    const demo = logging(new Demo('Joe', 3));
    console.log(demo.name);
    console.log(demo.age);
    demo.age = 42; // no effect
    demo.birthdays(5);
    for (let key in demo) {
        console.log(`${key} -> ${demo[key]}`);
    }
}

class Demo2 extends Demo {
    constructor(name, maxAge) {
        super(name);
        this.maxAge = maxAge;
    }

    get age() {
        return Math.min(super.age, this.maxAge);
    }
}

function logging2(target) {
    // Proxy to create handlers; see
    // https://exploringjs.com/deep-js/ch_proxies.html#improvement-implementing-the-handler-with-proxy
    const handler = new Proxy({}, {
        get(target, trapName, receiver) {
            // Return the handler method named trapName
            return (...args) => {
                const result = Reflect[trapName](...args);
                console.log(`${trapName.toUpperCase()}: ${String(args[1])} = ${JSON.stringify(result)}`);
                // Forward the operation
                return result;
            };
        },
    });
    return new Proxy(target, handler);
}

if (require.main === module) {
    main();
}