<?php

// PHP 8.5: attributes on (global) constants

#[\Deprecated]
const OLD_CONST = 1;

#[\Deprecated(message: "use NEW_ONE", since: "8.5")]
const OLDER_CONST = 2;

#[MyAttr]
const TYPED_CONST = "three";
