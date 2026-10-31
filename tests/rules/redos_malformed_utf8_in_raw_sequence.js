// ruleid: redos-malformed-utf8-in-raw-sequence
/^(a+)+\Qé\E$/.test(input);

// ok: redos-malformed-utf8-in-raw-sequence
/^a+\Qé\E$/.test(input);
