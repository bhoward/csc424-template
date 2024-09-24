// Demo of using closures to create a "counter" module

const Counter = (function() {
    let count = 0;

    function get() {
        return count;
    }

    function inc() {
        count++;
    }

    return { get, inc };
    // return Object.freeze({ get, inc });
})();

console.log(Counter.get());
Counter.inc();
console.log(Counter.get());

// Counter.count = 42;
// console.log(Counter.get());
// console.log(Counter.count);
