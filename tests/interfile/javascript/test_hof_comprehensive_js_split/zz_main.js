// Comprehensive HOF test: Custom functions and all built-in array methods
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====




// ===== Test Cases =====




// ===== Built-in Array Methods =====











// ===== Complex Example from Original Request =====



// ===== Top-level HOF Tests =====
// These test HOF callback detection at module level (outside any function)

// Top-level lambda callback
toplevelSink(source());

// Top-level method HOF (forEach with named callback)

const toplevelItems = [source()];
toplevelItems.forEach(toplevelHandler);

// Top-level user-defined HOF
customForEach(toplevelItems, toplevelHandler);
