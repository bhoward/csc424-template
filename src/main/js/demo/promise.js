// Using callbacks...

// Simulate an asynchronous task that returns a value after some time
function delayedValueCB(time, value, cb) {
    setTimeout(() => cb(null, value), time);
}

// Simulate an asynchronous task that fails after some time
function delayedFailureCB(time, error, cb) {
    setTimeout(() => cb(error), time)
}

// "Callback hell"
function callbackDemo(ret) {
    delayedValueCB(2000, "Step 1", (err, v1) => {
        // Ought to check for err non-null here...
        console.log(v1);
        delayedValueCB(2000, "Step 2", (err, v2) => {
            // And here...
            console.log(v2);
            delayedFailureCB(2000, "Oops!", (err, v3) => {
                if (!err) {
                    delayedValueCB(2000, "Profit!", (err, v4) => {
                        ret(v4);
                    });
                } else {
                    console.log("Error: " + err);
                    ret("We failed");
                }
            });
        });
    });
}

// Call the demo with a continuation to use the return value
callbackDemo(function (v) {
    console.log(v);
    console.log("Done");
});

/////////////////////////////////////////////

// Using promises...

// Simulate an asynchronous task that returns a value after some time
function delayedValue(time, value) {
    return new Promise(
        function (resolve, reject) {
            setTimeout(() => resolve(value), time);
        }
    );
}

// Simulate an asynchronous task that fails after some time
function delayedFailure(time, error) {
    return new Promise(
        function (resolve, reject) {
            setTimeout(() => reject(error), time)
        }
    );
}

// Chaining promise handlers
function promiseDemo() {
    return delayedValue(2000, "Step 1")
        .then(
            function (v) {
                console.log(v);
                return delayedValue(2000, "Step 2");
            }
        )
        .then(
            function (v) {
                console.log(v);
                return delayedFailure(2000, "Oops!");
            }
        )
        .then(
            function (v) {
                console.log(v);
                return delayedValue(2000, "Profit!");
            },
            function (e) {
                console.log("Error: " + e);
                return "We failed";
            }
        );
}

// Call the demo with one more handler
promiseDemo()
    .then(
        function (v) {
            console.log(v);
            console.log("Done");
        }
    );

/////////////////////////////////////////////

// Operations on promises...

// "Parallel" execution: wait for all
Promise.all([
    delayedValue(5000, "Long task"),
    delayedValue(2000, "Short task")
])
    .then(
        function (values) {
            console.log("Got " + values);
        }
    );

Promise.all([
    delayedValue(5000, "Long task"),
    delayedValue(2000, "Short task"),
    delayedFailure(3000, "Uh oh")
])
    .then(
        function (values) {
            console.log("Got " + values);
        }
    )
    .catch(
        function (e) {
            console.log("Error: " + e);
        }
    );

// first one wins
Promise.race([
    delayedValue(5000, "Long task"),
    delayedValue(2000, "Short task")
])
    .then(
        function (v) {
            console.log("Got " + v);
        }
    );

Promise.race([
    delayedValue(5000, "Long task"),
    delayedValue(2000, "Short task"),
    delayedFailure(3000, "Uh oh")
])
    .then(
        function (v) {
            console.log("Got " + v);
        }
    )
    .catch(
        function (e) {
            console.log("Error: " + e);
        }
    );

/////////////////////////////////////////////

// Using async/await...

// Imperative-looking code, to be turned into equivalent
// Promise code using "Continuation-Passing Style" transform
async function aaDemo() {
    let v1 = await delayedValue(2000, "Step 1");
    console.log(v1);
    let v2 = await delayedValue(2000, "Step 2");
    console.log(v2);
    try {
        await delayedFailure(2000, "Oops!");
        return await delayedValue(2000, "Profit!");
    }
    catch (e) {
        console.log("Error: " + e);
        return "We failed";
    }
}

// Call the demo with a promise handler
aaDemo()
    .then(
        function (v) {
            console.log(v);
            console.log("Done");
        }
    );
