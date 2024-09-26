// Utility functions to demonstrate promises
function delayedValue(time, value) {
  return new Promise(
    function(resolve, reject) {
	  setTimeout(() => resolve(value), time);
    }
  );
}

function delayedFailure(time, error) {
  return new Promise(
    function(resolve, reject) {
      setTimeout(() => reject(error), time)
    }
  );
}

// Chaining
delayedValue(2000, "Step 1")
.then(
  function(v) {
    console.log(v);
    return delayedValue(2000, "Step 2");
  }
)
.then(
  function(v) {
    console.log(v);
    return delayedFailure(2000, "Oops!");
  }
)
.then(
  function(v) {
    console.log(v);
    return delayedValue(2000, "Profit!");
  },
  function(e) {
    console.log("Error: " + e);
    return "We failed";
  }
)
.then(
  function(v) {
    console.log(v);
    console.log("Done");
  }
);

// "Parallel" execution: wait for all
Promise.all([
  delayedValue(5000, "Long task"),
  delayedValue(2000, "Short task")
])
.then(
  function(values) {
    console.log("Got " + values);
  }
);

Promise.all([
    delayedValue(5000, "Long task"),
    delayedValue(2000, "Short task"),
    delayedFailure(3000, "Uh oh")
])
.then(
  function(values) {
    console.log("Got " + values);
  }
)
.catch(
  function(e) {
    console.log("Error: " + e);
  }
);

// first one wins
Promise.race([
  delayedValue(5000, "Long task"),
  delayedValue(2000, "Short task")
])
.then(
  function(v) {
    console.log("Got " + v);
  }
);
  
Promise.race([
  delayedValue(5000, "Long task"),
  delayedValue(2000, "Short task"),
  delayedFailure(3000, "Uh oh")
])
.then(
  function(v) {
    console.log("Got " + v);
  }
)
.catch(
  function(e) {
    console.log("Error: " + e);
  }
);

// Using async/await
async function demo() {
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

demo()
.then(
  function(v) {
    console.log(v);
    console.log("Done");
  }
);