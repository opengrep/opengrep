// Test for taint flow through destructuring and helper functions
// Based on grafana RegExp finding

function cleanNeedle(needle) {
  return needle.replace(/[[{(][\w,.\/:;<=>?:*+]+$/, '');
}

function parseFlags(text) {
  const flags = 'g';
  const cleaned = text.replace(/some-pattern/, '');

  return {
    cleaned: cleaned,
    flags: flags
  };
}

function findHighlightChunksInText(needle) {
  const { cleaned, flags } = parseFlags(cleanNeedle(needle));
  let regexp;

  try {
    // ruleid: regexp_destructure_taint
    regexp = new RegExp(`(?:${cleaned})`, flags);
  } catch (e) {
    // error handling
  }

  return regexp;
}

function source() {
  return "tainted";
}

function test1() {
  const userInput = source();
  findHighlightChunksInText(userInput);
}
