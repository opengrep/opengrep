// ruleid: redos-repetition-overflow
/^(a+){1,99999999999999999999}$/.test(input);

// ok: redos-repetition-overflow
/^a{1,99999999999999999999}$/.test(input);
